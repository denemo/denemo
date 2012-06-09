from gub import gnome
from gub import target

class Orbit (target.AutoBuild):
    source = 'http://ftp.gnome.org/pub/GNOME/platform/2.32/2.32.1/sources/ORBit2-2.14.19.tar.bz2'
    dependencies = [
        'tools::libtool',
        'glib-devel',
        'libidl',
        ]
    config_cache_overrides = target.AutoBuild.config_cache_overrides + '''
ac_cv_alignof_CORBA_boolean=${ac_cv_alignof_CORBA_boolean=1}
ac_cv_alignof_CORBA_char=${ac_cv_alignof_CORBA_char=1}
ac_cv_alignof_CORBA_double=${ac_cv_alignof_CORBA_double=4}
ac_cv_alignof_CORBA_float=${ac_cv_alignof_CORBA_float=4}
ac_cv_alignof_CORBA_long=${ac_cv_alignof_CORBA_long=4}
ac_cv_alignof_CORBA_long_double=${ac_cv_alignof_CORBA_long_double=4}
ac_cv_alignof_CORBA_long_long=${ac_cv_alignof_CORBA_long_long=4}
ac_cv_alignof_CORBA_octet=${ac_cv_alignof_CORBA_octet=1}
ac_cv_alignof_CORBA_pointer=${ac_cv_alignof_CORBA_pointer=4}
ac_cv_alignof_CORBA_short=${ac_cv_alignof_CORBA_short=2}
ac_cv_alignof_CORBA_struct=${ac_cv_alignof_CORBA_struct=1}
ac_cv_alignof_CORBA_wchar=${ac_cv_alignof_CORBA_wchar=2}
'''
    def patch (self):
        target.AutoBuild.patch (self)
        self.file_sub ([('TRY_RUN', 'TRY_COMPILE')], '%(srcdir)s/configure.in', must_succeed=True)
        self.system ('cd %(srcdir)s && autoconf')
