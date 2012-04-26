'''
TODO:
  * figure out solution pango/pangocairo, lilypond/lilypondcairo mess
  * add jack for windows?
  * what about timidity?
  * relocation: non-windows dynamic relocation in main.c
  * relocation: fix locale dir
'''

from gub import misc
from gub import repository
from gub import target

class Denemo (target.AutoBuild):
    source = 'git://git.savannah.gnu.org/denemo.git'
    branch = 'master'
    patches = [ ]
    subpackage_names = ['']
    dependencies = [
        'cross/gcc-c++-runtime',
        'tools::automake',
        'tools::gettext',
        'tools::libtool',
        'tools::pkg-config',
	'tools::glib',
        #'epdfview', # Builds, but needs dynamic relocation patches.
        'fluidsynth',
        'guile-devel',
        'gtk+-devel',
 	'glib-devel',
	'evince',
        #'jack-devel',
        #'lash-devel',
        'libaubio-devel',
        'libgtksourceview-devel',
        'librsvg-devel', 
        'libxml2-devel',
        'lilypondcairo',
        'portaudio-devel',
 	'libsndfile',
	'cairo',
        ]
    configure_flags = (target.AutoBuild.configure_flags
                       + ' --enable-binreloc'
                       + ' --enable-jack'
                       + ' --enable-fluidsynth'
                       + ' --program-prefix='
                       )
    # FIXME: --enable-binreloc has been neutralized.
    #make_flags = 'BINRELOC_CFLAGS=-DENABLE_BINRELOC=1'

    def __init__ (self, settings, source):
        target.AutoBuild.__init__ (self, settings, source)
        if isinstance (source, repository.Git):
            source.version = misc.bind_method (repository.Repository.version_from_configure_in, source)
    def compile (self):
        if isinstance (self.source, repository.Git):
            # FIXME: missing dependency
            self.system ('cd %(builddir)s/src && make lylexer.c')
        target.AutoBuild.compile (self)

class Denemo__mingw__windows (Denemo):
    dependencies = [x for x in Denemo.dependencies
                    if x.replace ('-devel', '') not in [
            'jack',
            'lash',
            ]] + ['lilypad']
    configure_flags = (Denemo.configure_flags
                       .replace ('--enable-jack', '--disable-jack'))
    make_flags = ''

class Denemo__mingw__console (Denemo__mingw__windows):
    configure_flags = (Denemo__mingw__windows.configure_flags
                          .replace ('--enable-binreloc', '--disable-binreloc')
                       + ' --enable-debug')
    def __init__ (self, settings, source):
        Denemo__mingw__windows.__init__ (self, settings, source)
        # Configure (link) without -mwindows for denemo-console.exe
        self.target_gcc_flags = '-mms-bitfields'
    def compile (self):
        Denemo__mingw__windows.compile (self)
        self.system ('''
cd %(builddir)s/src && mv .libs/denemo.exe denemo-console.exe && rm -f denemo.exe
cd %(builddir)s/src && make AM_LDFLAGS="-mwindows" && cp -p .libs/denemo.exe denemo-windows.exe
''')
    def install (self):
        Denemo__mingw__windows.install (self)
        self.system ('''
install -m755 %(builddir)s/src/denemo-windows.exe %(install_prefix)s/bin/denemo.exe
install -m755 %(builddir)s/src/denemo-console.exe %(install_prefix)s/bin/denemo-console.exe
''')

# Use debugging for Windows for now.
# Denemo__mingw = Denemo__mingw__windows
Denemo__mingw = Denemo__mingw__console

class Denemo__darwin (Denemo):
    dependencies = [x for x in Denemo.dependencies
                    if x.replace ('-devel', '') not in [
            'jack',
            'lash',
            'libxml2', # Included in darwin-sdk, hmm?
            ]] + [
        'fondu',
        'osx-lilypad',
        ]
    configure_flags = (Denemo.configure_flags
                       .replace ('--enable-jack', '--disable-jack')
                       + ' "CPPFLAGS=-I%(system_prefix)s/include -I%(system_prefix)s/include/sys"')

class Denemo__darwin__ppc (Denemo__darwin):
    # make sure that PREFIX/include/unistd.h gets included
    def patch (self):
        Denemo__darwin.patch (self)
        self.system ('''
mkdir -p %(builddir)s/src
cp -pv %(system_prefix)s/include/unistd.h %(builddir)s/src
''')
