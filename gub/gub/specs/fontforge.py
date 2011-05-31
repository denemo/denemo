from gub import context
from gub import tools 

class Fontforge__tools (tools.AutoBuild):
    # 20100501 breaks flags
    #source = 'http://surfnet.dl.sourceforge.net/sourceforge/fontforge/fontforge-source/fontforge_full-20100501.tar.bz2'
    #source = ':pserver:anonymous@fontforge.cvs.sourceforge.net:/cvsroot/fontforge&module=fontforge&tag=HEAD'
    #source = 'git://git.debian.org/pkg-fonts/fontforge.git&revision=4ab4fe9a81a8e15c1c7d479dc710c54335083042'
    source = 'http://www.nilsgey.de/fontforge.tar.bz2'
    parallel_build_broken = True
    srcdir_build_broken = True
    dependencies = ['freetype', 'libpng', 'libjpeg', 'libxml2']
    def srcdir (self):
        return tools.AutoBuild.srcdir (self).replace ('_full', '')
    def patch (self):
        tools.AutoBuild.patch (self)
        for name in ['%(srcdir)s/fontforge/Makefile.dynamic.in',
             '%(srcdir)s/fontforge/Makefile.static.in',
             '%(srcdir)s/gdraw/Makefile.dynamic.in',
             '%(srcdir)s/gdraw/Makefile.static.in',
             '%(srcdir)s/gutils/Makefile.dynamic.in',
             '%(srcdir)s/gutils/Makefile.static.in',
             '%(srcdir)s/Unicode/Makefile.dynamic.in',
             '%(srcdir)s/Unicode/Makefile.static.in']:
            self.file_sub (
                [(' -I$(top_srcdir)/inc',
                  ' -I$(top_srcdir)/inc -I$(top_builddir)/inc')],
                name, use_re=False)
        # URG, fontforge uses no *-config or *.pc files, but
        # looks in /usr/include,
        self.file_sub ([('([I" \(])/usr/include',
                         r'\1%(system_prefix)s/include')],
                       '%(srcdir)s/configure')
        # and /*/lib :-)
        # Just override /lib checks, picking-up of /usr/lib*
        # for tools is allowed...
        self.file_sub ([('"/lib/lib', '"%(system_prefix)s/lib/lib')],
                       '%(srcdir)s/configure')
    configure_flags = (tools.AutoBuild.configure_flags
                + ' --without-freetype-src'
                + ' --disable-libff '
                + ' --enable-double '
                # let's ignore python (and its dynamic link intracies
                # for now).
                + ' --without-python')
