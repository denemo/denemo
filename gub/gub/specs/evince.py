from gub import target
from gub import tools

class Evince (target.AutoBuild):
  source = 'http://ftp.gnome.org/pub/GNOME/sources/evince/2.32/evince-2.32.0.tar.bz2'
  dependencies = ['intltool']
		  #'gnome-doc-utils',
		  #'gnome-vfs']
  configure_flags = (tools.AutoBuild.configure_flags
                           #+ ' --with-libintl-prefix=%(system_prefix)s'
                           + '  --without-libgnome'
			   + '  --without-gconf'
                           + '  --without-keyring'
                           + '  --with-platform=win32'
			   + '  --disable-help'
			   + '  --disable-thumbnailer'
			   + '  --disable-nautilus'
			   + '  --disable-dbus'
			   + '  --disable-gtk-doc')

