from gub import gnome
from gub import target

class Libidl (target.AutoBuild):
    source = 'http://ftp.acc.umu.se/pub/gnome/sources/libIDL/0.8/libIDL-0.8.10.tar.bz2'
    dependencies = [
        'tools::libtool',
        'glib-devel',
        ]
    config_cache_overrides = target.AutoBuild.config_cache_overrides + '''
libIDL_cv_long_long_format=ll
'''
