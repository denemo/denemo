from gub import target

class Pixman (target.AutoBuild):
    source = 'http://xorg.freedesktop.org/archive/individual/lib/pixman-0.13.2.tar.gz'
    dependencies = ['libtool']

class Pixman__linux__ppc (Pixman):
    patches = ['pixman-0.13.2-auxvec.patch']
