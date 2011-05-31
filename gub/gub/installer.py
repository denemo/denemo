import os
import re
import time
import md5
import pickle
#
from gub.syntax import printf
from gub import context
from gub import darwin
from gub import gup
from gub import target
from gub import context
from gub import misc
from gub import commands

# UGH -  we don't have the package dicts yet.
# barf, this should be in config file, not in code
pretty_names = {
    'denemo': 'Denemo',
    'git': 'Git',
    'lilypond': 'LilyPond',
    'openoffice': 'Go-Oo_OpenOffice.org',
    'ooo-build': 'Go-Oo_OpenOffice.org',
    }

class ChecksumShortCircuit (Exception):
    def __init__ (self, file):
        Exception.__init__ (self)
        self.file = file

class Installer (context.RunnableContext):
    def __init__ (self, settings, arguments, version_db, branch_dict):
        context.RunnableContext.__init__ (self, settings)

        self.arguments = arguments
        name = arguments[0]
        self.settings = settings
        self.version_db = version_db
        self.strip_command \
            = '%(cross_prefix)s/bin/%(target_architecture)s-strip' 
        self.no_binary_strip = []
        self.no_binary_strip_extensions = ['.la', '.py', '.def', '.scm', '.pyc']
        self.installer_uploads = settings.uploads
        self.checksum = ''
        self.name = name
        self.pretty_name = pretty_names.get (name, name)

        self.branch_dict = branch_dict
        self.package_branch = self.branch_dict.get (name, '')
        if ':' in self.package_branch:
            (self.remote_package_branch,
             self.package_branch) = tuple (self.package_branch.split (':'))

        self.installerdir = settings.targetdir + '/installer'
        self.installer_root = ('%(installerdir)s/%(name)s-%(package_branch)s'
                               % self.__dict__)
        self.installer_prefix = self.installer_root + settings.prefix_dir
        self.installer_checksum_file = self.installer_root + '.checksum'
        self.installer_db = self.installer_root + '-dbdir'

# hmm?
#    @context.subst_method
    def installer_file (self):
        return ''

    def building_root_image (self):
        # FIXME: why are we removing these, we need these in a root image.
        # How to make a better check here?
        return (self.name.startswith ('root-image')
                or self.name.startswith ('phone'))

    @context.subst_method
    def version (self):
        return self.installer_version

    def build (self):
        os.system ('mkdir -p ' + self.installerdir)
        install_manager = gup.DependencyManager (self.installer_root,
                                                 dbdir=self.installer_db,
                                                 clean=True)

        install_manager.include_build_deps = False
        install_manager.read_package_headers (self.settings.packages,
                                              self.branch_dict)
        install_manager.read_package_headers (self.settings.cross_packages,
                                              self.branch_dict)

        self.package_manager = install_manager

        def get_dep (x):
            return install_manager.dependencies (x)


        package_names = gup.topologically_sorted (self.arguments, {},
                                                  get_dep,
                                                  None)
        self.checksum_matches = False
        checksum_file = self.installer_checksum_file
        self.checksum = self.calculate_checksum (package_names)
        if os.path.exists (checksum_file) and open (checksum_file).read () == self.checksum:
            raise ChecksumShortCircuit (checksum_file)

        for a in package_names:
            install_manager.install_package (a)

        version = install_manager.package_dict (self.name)['version']
        version_tup = misc.string_to_version (version)
        buildnumber = '%d' % self.version_db.get_next_build_number (version_tup)

        self.installer_version = version
        self.installer_build = buildnumber

    def calculate_checksum (self, names):
        checksum_list = []
        for name in sorted (names):
            dict = self.package_manager.package_dict (name)
            if not dict:
                m = 'no such package: %(name)s\n' % locals ()
                self.runner.error (m)
                raise Exception (m)
            package_checksum = file (dict['checksum_file']).read ()
            package_checksum = md5.md5 (package_checksum).hexdigest ()
            checksum_list.append ((dict['name'],        
                                   dict['source_checksum'],
                                   package_checksum))
        string = pickle.dumps (checksum_list, protocol=2)
        # Take the hash to make sure that there are no random strings
        # (which may trigger substitutions) in the attributes of a
        # Context
        return md5.md5 (string).hexdigest ()

    def stages (self):
        return ['build', 'shortcircuit_checksum', 'strip', 'create',
                'write_checksum']

    def strip_prefixes (self):
        return ['', 'usr/']
        
    def strip_unnecessary_files (self):
        "Remove unnecessary cruft."

        globs = [
            'bin/autopoint',
            'bin/glib-mkenums',
            'bin/guile-*',
            'bin/*-config',
            'bin/*gettext*',
            'bin/gs??',
            'bin/gsdj500',
            'bin/dbftops',
            'bin/dvipdf',
            'bin/pf2afm',
            'bin/printafm',
            'bin/pv.sh',
            'bin/unix-lpr.sh',
            'bin/wftopfa',
            'bin/idle',
            'bin/font2c',
            'bin/fixmswrd.pl',
            'bin/dumphint',
            'bin/[cd]jpeg',
            'bin/envsubst*',
            'bin/msg*',
            'bin/xmlwf',
            'cross',
            'doc',
            'include',
            'info',
            'lib/gettext',
            'lib/gettext/hostname*',
            'lib/gettext/urlget*',
            'lib/glib-2.0/include/glibconfig.h',
            'lib/glib-2.0',
            'lib/pkgconfig',
            'lib/*~',
            'lib/*.a',
            'lib/python*/distutils/command/wininst-6*',
            'lib/python*/distutils/command/wininst-7.1*',
            'man',
            'share/doc',
            'share/guile/*/ice-9/debugger/',
            'share/gettext/intl',
            'share/ghostscript/*/{doc,examples}/',
            'share/ghostscript/*/Resource/{CMap,ColorSpace,Decoding}/',
# keep font, otherwise mingw users have problems.
            'share/ghostscript/*/Resource/{Encoding,SubstCID}/',
            'share/gs/*/{doc,examples}/',
            'share/gtk-doc',
            'share/info',
# Ugh, ugh.  At least, keep Denemo.ttf
#            'share/fonts/',
            'share/fonts/default',
            'share/man',
            'share/omf',
            'share/libtool/',

            # prune harder
            'lib/python*/bsddb',
            'lib/python*/compiler',
            'lib/python*/curses',
            'lib/python*/distutils',
            'lib/python*/email',
            'lib/python*/hotshot',
            'lib/python*/idlelib',
            'lib/python*/lib-old',
            'lib/python*/lib-tk',
            'lib/python*/logging',
            'lib/python*/test',
# xml2ly needs xml.dom
#                        'lib/python*/xml',
            'share/lilypond/*/make',
            'share/gettext',
            'usr/share/aclocal',
            'share/lilypond/*/tex',
            'share/lilypond/*/fonts/source',
# Keep svg fonts.  They are needed for usable/sharable svg output.
#                        'share/lilypond/*/fonts/svg',
            'share/lilypond/*/fonts/tfm',
            'share/lilypond/*/fonts/type1/feta[0-9]*pfa',
            'share/lilypond/*/fonts/type1/feta-braces-[a-z]*pfa',
            'share/lilypond/*/fonts/type1/parmesan*pfa',
            'share/omf',
            ## 2.6 installer: leave c059*
            'share/gs/fonts/[a-bd-z]*',
            'share/gs/fonts/c[^0][^5][^9]*',
# Urg: prune qt fonts, keep helvetica, fontdir
            'lib/fonts/[^hf]*',
            'share/mkspecs',
            'share/terminfo',
            ]

        # FIXME: why are we removing these, we need these in a root image.
        # How to make a better check here?
        if not self.building_root_image ():
            globs += [
            'lib/libc.*',
            'lib/libm.*',
            ]

        delete_me = ''
        for p in self.strip_prefixes ():
            delete_me += p + '%(i)s '

        keep = []
        if self.name.startswith ('lilypond-ancient'):
            keep += [
                'cross',
                'share/doc',
                'share/lilypond/*/tex',
                'share/lilypond/*/fonts/source',
                'share/lilypond/*/fonts/tfm',
                ]
            globs += [
                'share/doc/[^l]*',
                'share/doc/l[^i]*',
                'share/doc/li[^l]*',
                'cross/',
                'cross-ancient/bin',
                'cross-ancient/i686-linux',
                'cross-ancient/include',
                ]

        for i in globs:
            if not i in keep:
                # [^..] dash globbing is broken, {,} globbing is bashism
                self.system ("cd %(installer_root)s && bash -c 'rm -rvf " + delete_me + "'", {'i': i }, locals ())

    def strip_dir (self, dir):
        misc.map_command_dir (self,
                              self.expand (dir),
                              self.expand ('%(strip_command)s'),
                              misc.binary_strip_p (self.no_binary_strip,
                                                   self.no_binary_strip_extensions))
        
    def strip (self):
        self.strip_unnecessary_files ()
        self.strip_dir ('%(installer_prefix)s/bin')
        self.strip_dir ('%(installer_prefix)s/lib')

    def create (self):
        self.system ('mkdir -p %(installer_root)s/license')
        self.system ('cp %(sourcefiledir)s/gub.license %(installer_root)s/license/README', ignore_errors=True)

    def write_checksum (self):
        if self.checksum:
            open (self.expand ('%(installer_checksum_file)s'), 'w').write (self.checksum)
        else:
            self.runner.warning ('checksum is empty.')

class DarwinRoot (Installer):
    def __init__ (self, settings, *args):
        Installer.__init__ (self, settings, *args)
        self.strip_command += ' -S '
        self.rewirer = darwin.Rewirer (self.settings)

    def connect_command_runner (self, runner):
        self.rewirer.connect_command_runner (runner)
        return Installer.connect_command_runner (self, runner)
        
    def build (self):
        Installer.build (self)
        tarball = self.package_manager.package_dict ('darwin-sdk')['split_ball']
        self.rewirer.set_ignore_libs_from_tarball (tarball)

    def create (self):
        Installer.create (self)
        self.rewirer.rewire_root (self.expand ('%(installer_root)s'))
        
    
class DarwinBundle (DarwinRoot):
    def __init__ (self, *args):
        DarwinRoot.__init__ (self, *args)
        self.darwin_bundle_dir = '%(installerdir)s/%(pretty_name)s.app'
        
    def installer_file (self):
        return self.expand ('%(uploads)s/%(name)s-%(installer_version)s-%(installer_build)s.%(platform)s.tar.bz2')

    def create (self):
        DarwinRoot.create (self)
        
        osx_lilypad_version = self.package_manager.package_dict ('osx-lilypad')['version']

        ## cpu_type = self.expand ('%(platform)s').replace ('darwin-', '')
        cpu_type = 'ppc'
        installer_version = self.installer_version
        installer_build = self.installer_build
        
        bundle_zip = self.installer_file ()
        self.system ('''
rm -f %(bundle_zip)s 
rm -rf %(darwin_bundle_dir)s
# FIXME: ask TarBall where source lives
# Was hardcoded: Instead of lilypad build a real filestructure.
#tar -C %(installerdir)s -zxf %(downloads)s/osx-lilypad/osx-lilypad-universal-%(osx_lilypad_version)s.tar.gz
mkdir -p %(darwin_bundle_dir)s/Contents/Resources
#The Uberhack. Download the Info.plist each time from the denemo server. Old: #touch %(darwin_bundle_dir)s/Contents/Info.plist # FIXME - this may need content
wget http://git.savannah.gnu.org/cgit/denemo.git/plain/Info.plist?id=ce42f8f196b3f05314d4f4814208b6cfef4ec962 -O %(darwin_bundle_dir)s/Contents/Info.plist
touch %(darwin_bundle_dir)s/Contents/Resources/Credits.html # FIXME - this may need content
cp -pR --link %(installer_prefix)s/* %(darwin_bundle_dir)s/Contents/Resources/
mkdir -p %(darwin_bundle_dir)s/Contents/MacOS
cp -pR --link %(darwin_bundle_dir)s/Contents/Resources/bin/* %(darwin_bundle_dir)s/Contents/MacOS 
mkdir -p %(darwin_bundle_dir)s/Contents/Resources/license
cp -pR --link %(installer_root)s/license*/* %(darwin_bundle_dir)s/Contents/Resources/license/
''', locals ())
        self.file_sub ([('''PACKAGE_NAME=Denemo
MAJOR_VERSION=0
MINOR_VERSION=9
PATCH_LEVEL=1
MY_PATCH_LEVEL=
''', '%(installer_version)s-%(installer_build)s'),
                        ('2.[0-9]+.[0-9]+-[0-9]', '%(installer_version)s-%(installer_build)s'),
                        ('Build from .*',
                         'Build from %s' % time.asctime ()),
                        ],
            '%(darwin_bundle_dir)s/Contents/Info.plist',
            env=locals ())

        majmin = '.'.join (installer_version.split ('.')[:2])
        self.file_sub (
            [('doc/v2.12/',
             'doc/v%(majmin)s/'),
            ],
            '%(darwin_bundle_dir)s/Contents/Resources/Credits.html',
            env=locals ())
        
        if 'lilypond' in self.name.lower ():
            self.file_sub (
                [('2.12.0', installer_version),
                 ],
                '%(darwin_bundle_dir)s/Contents/Resources/Welcome-to-LilyPond-MacOS.ly',
            env=locals ())
        self.system ('cd %(darwin_bundle_dir)s/../ && find %(pretty_name)s.app -print0 | xargs -0 touch', locals ())
        
        self.system ('cd %(darwin_bundle_dir)s/../ && tar cjf %(bundle_zip)s %(pretty_name)s.app',
                     locals ())
        
        
class MingwRoot (Installer):
    def __init__ (self, *args):
        Installer.__init__ (self, *args)
        self.strip_command += ' -g '
    
class Nsis (MingwRoot):
    def installer_file (self):
        return self.expand ('%(installer_uploads)s/%(name)s-%(installer_version)s-%(installer_build)s.%(platform)s.exe')
    def create (self):
        MingwRoot.create (self)
        
        # FIXME: build in separate nsis dir, copy or use symlink
        installer = os.path.basename (self.expand ('%(installer_root)s'))
        ns_dir = self.expand ('%(installer_db)s')

        self.dump (r'''
!define INSTALLER_VERSION "%(installer_version)s"
!define INSTALLER_BUILD "%(installer_build)s"
!define INSTALLER_OUTPUT_DIR "%(ns_dir)s"
!define ROOT "%(installer)s"
!define PRETTY_NAME "%(pretty_name)s"
!define CANARY_EXE "%(name)s"
!define NAME "%(name)s"

!addincludedir "${INSTALLER_OUTPUT_DIR}"
OutFile "${INSTALLER_OUTPUT_DIR}/setup.exe"
''',
                   '%(ns_dir)s/definitions.nsh',
                   env=locals ())
        
        self.system (r'''cp %(nsisdir)s/*.nsh %(ns_dir)s
cp %(nsisdir)s/*.nsi %(ns_dir)s
cp %(nsisdir)s/*.sh.in %(ns_dir)s''', locals ())

        root = self.expand ('%(installer_root)s')
        files = [f.replace (root, '').replace ('/', '\\')
                 for f in misc.locate_files (root, '*')]

        self.dump ('\r\n'.join (files) + '\r\n',
                   '%(installer_root)s/files.txt',
                   expand_string=False)

        PATH = os.environ['PATH']
        PATH = '%(tools_prefix)s/bin:' + PATH

        nsi = self.name
        if self.name == 'mingit':
            # urgme
            nsi = 'git'
        if not os.path.exists (self.expand ('%(nsisdir)s/%(nsi)s.nsi', env=locals ())):
            nsi = 'installer'
        self.system ('cd %(installerdir)s && makensis -NOCD %(ns_dir)s/definitions.nsh %(ns_dir)s/%(nsi)s.nsi', locals ())

        final = self.installer_file ()
        self.system ('mv %(ns_dir)s/setup.exe %(final)s', locals ())


class Linux_installer (Installer):
    def __init__ (self, *args):
        Installer.__init__ (self, *args)

        self.bundle_tarball = '%(installer_uploads)s/%(name)s-%(installer_version)s-%(installer_build)s.%(platform)s.tar.bz2'

    def strip_prefixes (self):
        return Installer.strip_prefixes (self)

    def create_tarball (self, bundle_tarball):
        self.system ('tar --owner=0 --group=0 -C %(installer_root)s -jcf %(bundle_tarball)s .', locals ())

    def create (self):
        Installer.create (self)
        self.create_tarball (self.bundle_tarball)

class Shar (Linux_installer):
    def create (self):
        Linux_installer.create (self)
        name = self.name
        pretty_name = self.pretty_name
        release = self.expand ('%(installer_build)s')
        shar_file = self.installer_file ()
        shar_head = self.expand ('%(sourcefiledir)s/sharhead.sh')
        if 'lilypond' in self.name.lower ():
            shar_head = self.expand ('%(sourcefiledir)s/lilypond-sharhead.sh')
        tarball = self.expand (self.bundle_tarball)
        version = self.expand ('%(installer_version)s')
        self.runner._execute (commands.CreateShar (name=name, pretty_name=pretty_name, release=release, shar_file=shar_file, shar_head=shar_head, tarball=tarball, version=version))
# hmm?
#    @context.subst_method
    def installer_file (self):
        return self.expand ('%(installer_uploads)s/%(name)s-%(installer_version)s-%(installer_build)s.%(platform)s.sh')
        
def get_installer (settings, *arguments):

    installer_class = {
        # TODO: ipkg/dpkg
        'debian' : Shar,
        'debian-arm' : Shar,
        'debian-mipsel' : Shar,
        
        'darwin-ppc' : DarwinBundle,
        'darwin-x86' : DarwinBundle,
        'freebsd-x86' : Shar,
        'freebsd4-x86' : Shar,
        'freebsd6-x86' : Shar,
        'freebsd-64' : Shar,
        'linux-arm-softfloat' : Shar,
        'linux-arm-vfp' : Linux_installer,
        'linux-x86' : Shar,
        'linux-64' : Shar,
        'linux-ppc' : Shar,
        'mingw' : Nsis,
#        'mingw' : MingwRoot,
    }

    ctor = installer_class[settings.platform]
    printf ('Installer:', ctor)
    installer = ctor (settings, *arguments)
    return installer
