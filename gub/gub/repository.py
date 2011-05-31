"""
    Copyright (c) 2005--2008
    Jan Nieuwenhuizen <janneke@gnu.org>
    Han-Wen Nienhuys <hanwen@xs4all.nl>

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2, or (at your option)
    any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
"""

import md5
import optparse
import os
import re
import sys
import time
import urllib
import xml.dom.minidom

def argv0_relocation ():
    import os, sys
    bindir = os.path.dirname (sys.argv[0])
    prefix = os.path.dirname (bindir)
    if not prefix:
        prefix = bindir + '/..'
    sys.path.insert (0, prefix)

argv0_relocation ()
#
from gub.syntax import printf
from gub.db import db
from gub import misc
from gub import locker
from gub import tztime
from gub import logging
from gub import loggedos

class UnknownVcSystem (Exception):
    pass

class RepositoryException (Exception):
    pass

class RepositoryProxy:
    repositories = []
    def register (repository):
        RepositoryProxy.repositories.append (repository)
    register = staticmethod (register)

    # FIXME: drop branch, module, revision and pass as parameters
    @staticmethod
    def get_repository (dir, url, branch='', module='', revision=''):
        parameters = dict ()
        if url:
            url, parameters = misc.dissect_url (url)
            branch = parameters.get ('branch', [branch])[0]
            revision = parameters.get ('revision', [revision])[0]
            # FIXME/TODO: pass these nicely to create ()
            # possibly do dir,url,branch,revision also as dict or kwargs?
            name = parameters.get ('name', [''])[0]
            module = parameters.get ('module', [module])[0]

        for proxy in RepositoryProxy.repositories:
            if proxy.check_url (proxy, url):
                # FIXME: drop branch, module, revision and pass as parameters
                return proxy.create (proxy, dir, url, branch, module, revision, parameters)
            
        if url and url.startswith ('file://'):
            u = misc.Url (url)
            proto = u.protocol
            host = u.host
            url_dir = u.dir
            for proxy in RepositoryProxy.repositories:
                if proxy.check_dir (proxy, url_dir):
                    return proxy.create (proxy, dir, url, branch, module, revision, parameters)
                
        for proxy in RepositoryProxy.repositories:
            if proxy.check_dir (proxy, dir):
                return proxy.create (proxy, dir, url, branch, module, revision, parameters)
        for proxy in RepositoryProxy.repositories:
            if proxy.check_suffix (proxy, url):
                return proxy.create (proxy, dir, url, branch, module, revision, parameters)
        for proxy in RepositoryProxy.repositories:
            if os.path.isdir (os.path.join (dir, '.gub' + proxy.vc_system)):
                d = misc.find_dirs (dir, '^' + proxy.vc_system)
                if d and proxy.check_dir (proxy, os.path.dirname (d[0])):
                    return proxy.create (proxy, dir, url, branch, module, revision, parameters)
        for proxy in RepositoryProxy.repositories:
            # FIXME: this is currently used to determine flavour of
            # downloads/lilypond.git.  But is is ugly and fragile;
            # what if I do brz branch foo foo.git?
            if proxy.check_suffix (proxy, dir):
                return proxy.create (proxy, dir, url, branch, module, revision, parameters)
        raise UnknownVcSystem ('Cannot determine source: url=%(url)s, dir=%(dir)s'
                               % locals ())

## Rename to Source/source.py?
class Repository: 
    vc_system = None
    tag_dateformat = '%Y-%m-%d_%H-%M-%S%z'
    def version_from_shell_script (self, file_name, canary, version_string, default_version='0.0.0'):
        try:
            return misc.version_from_shell_script (self.read_file (file_name),
                                                   canary, version_string ,default_version)
        except:
            return default_version
    def version_from_configure_in (self, file_name='configure.in', default_version='0.0.0'):
        try:
            return misc.version_from_configure_in (self.read_file (file_name),
                                                   default_version)
        except:
            return default_version
    def version_from_pc_in (self, file_name, default_version='0.0.0'):
        try:
            return misc.version_from_pc_in (self.read_file (file_name),
                                            default_version)
        except:
            return default_version
    @staticmethod
    def check_dir (rety, dir):
        return os.path.isdir (os.path.join (dir, rety.vc_system))
    @staticmethod
    def check_url (rety, url):
        vcs = rety.vc_system.replace ('_', '',).replace ('.', '').lower ()
        return url and (url.startswith (vcs + ':')
                        or url.startswith (vcs + '+')
                        or url.startswith ('http' + '+' + vcs)
                        or url.startswith ('ssh' + '+' + vcs))
    @staticmethod
    def check_suffix (rety, url):
        return url and url.endswith (rety.vc_system)
    @staticmethod
    def create (rety, dir, source, branch='', module='', revision='', parameters=list ()):
        return rety (dir, source, branch, module, revision)
    def have_client (self):
        # FIXME: make generic: have_CLIENT ()?
        # this would even work for TAR, vs SimpleTar (without --strip-comp)
        if self.settings:
            tools_dir = (self.settings.alltargetdir + '/tools'
                         + self.settings.root_dir)
            tools_bin = (tools_dir
                         + self.settings.prefix_dir
                         + '/bin')
            return os.path.exists (os.path.join (tools_bin, self.vc_system[1:]))
        return False
    def migrate (self, dir, dir_slash_vcs):
        self.info ('migrate not implemented for: ' + self.vc_system)
    def __init__ (self, dir, source):
        self.settings = None
        self.source = source
        self.logger = logging.default_logger
        self.system = self.logged_indirection (loggedos.system)
        self._read_file = self.logged_indirection (loggedos.read_file)
        self.download_url = self.logged_indirection (loggedos.download_url)
        self.read_pipe = self.logged_indirection (loggedos.read_pipe)
        self.dir = os.path.normpath (dir)
        dir_vcs = self.dir + self.vc_system
        if not os.path.isdir (dir) and os.path.isdir (dir_vcs):
            # URG, Fixme, wtf?:
            sys.stderr.write ('appending %s to checkout dir %s\n'
                              % (self.vc_system, self.dir))
            self.dir = dir_vcs
        # cloning/creating --git-dir == downloads/lilypond
        # fails if we already have downloads/lilypond/lily-1.2.3.tar.gz
        # there; migrate to downloads/lilypond/git/
        dir_slash_vcs = os.path.join (self.dir, self.vc_system[1:])
        if os.path.isdir (self.dir) and not os.path.isdir (dir_slash_vcs):
            self.migrate (self.dir, dir_slash_vcs)
        if self.vc_system == '.git' and not os.path.isdir (os.path.join (self.dir, 'objects')):
            self.dir = dir_slash_vcs
        if not dir or dir == '.':
            dir = os.getcwd ()
            if os.path.isdir (os.path.join (dir, self.vc_system)):
                # Support user-checkouts: If we're already checked-out
                # HERE, use that as repository
                self.dir = dir
            else:
                # Otherwise, check fresh repository out under .gub.VC_SYSTEM
                self.dir = os.path.join (os.getcwd (), '.gub' + self.vc_system)
    def get_env (self):
        env = os.environ
        # Hmm, Repository.system and .read_pipe are used in
        # build.Build.__init__ () [read: GIT] when no
        # get_substitution_dict expansion is allowed.  Use settings to
        # sneak in correct paths.
        if self.settings:
            tools_dir = (self.settings.alltargetdir + '/tools'
                         + self.settings.root_dir)
            tools_bin = (tools_dir
                         + self.settings.prefix_dir
                         + '/bin')
            tools_lib = (tools_dir
                         + self.settings.prefix_dir
                         + '/lib')
            return {
                'PATH': tools_bin + ':' + os.environ['PATH'],
                'LD_LIBRARY_PATH': tools_lib
                + misc.append_path (os.environ.get ('LD_LIBRARY_PATH', '')),
                }
        return os.environ
    def logged_indirection (self, loggedos_func):
        def logged (*args, **kwargs):
            return loggedos_func (self.logger, *args, **kwargs)
        return logged
    def connect_logger (self, logger):
        assert logger
        self.logger = logger
    def info (self, message):
        self.logger.write_log (message + '\n', 'info')
    def error (self, message):
        self.logger.write_log (message + '\n', 'error')
    def warning (self, message):
        self.logger.write_log (message + '\n', 'warning')
    def filter_branch_arg (self, branch):
        if '=' in branch:
            (name, branch) = tuple (branch.split ('='))
            if name != self.file_name ():
                branch = ''
        return branch
    def full_branch_name (self):
        if self.is_tracking ():
            return self.branch.replace ('/', '-')
        return ''
    def file_name (self):
        if not self.source:
            return os.path.splitext (os.path.basename (self.dir))[0]
        return os.path.splitext (os.path.basename (self.source))[0]
    def download (self):
        pass
    def checksum (self):
        '''A checksum that characterizes the entire repository.

Typically a hash of all source files.'''
        return self.version ()
    def read_file (self, file_name):
        return ''
    def is_distributed (self):
        '''Whether repository is central or uses distributed repositories'''
        return True
    def is_tracking (self):
        '''Whether download will fetch newer versions if available'''
        return False
    def is_downloaded (self):
        '''Whether repository is available'''
        return True
    def update_workdir (self, destdir):
        '''Populate DESTDIR with sources of specified version/branch

Updating is preferably done by updating'''
        pass

    ## Version should be human-readable version.
    def version  (self):
        '''A human-readable revision number.

It need not be unique over revisions.'''
        return '0'

    def last_patch_date (self):
        '''Return timestamp of last patch'''
        return None

    def read_last_patch (self):
        '''Return a dict with info about the last patch'''
        return {'date': None, 'patch': None}
    def get_diff_from_tag (self, name):
        '''Return diff wrt to tag NAME'''
        None
    def get_diff_from_tag_base (self, name):
        '''Return diff wrt to last tag that starts with NAME'''
        tags = self.tag_list (name)
        tags.sort ()
        if tags:
            return self.get_diff_from_tag (tags[-1])
        return None

class TagDb:
    def __init__ (self, dir):
        self.db = db.open (os.path.join (dir, 'tag.db'), 'c')
    def tag (self, name, repo):
        stamp = repo.last_patch_date ()
        if stamp:
            date = tztime.format (stamp)
            self.db[name + '-' + date] = date
    def tag_list (self, name):
        return [self.db[y] for y in
                [x for x in list (self.db.keys ()) if x.startswith (name)]]
    def get_diff_from_tag_base (self, name, repo):
        tags = self.tag_list (name)
        if tags:
            tags.sort ()
            return repo.get_diff_from_date (tztime.parse (tags[-1]))
        return None

class Version (Repository):
    vc_system = 'url'
    @staticmethod
    def create (rety, dir, source, branch='', module='', revision='', parameters=list ()):
        return Version (source, revision)
    def __init__ (self, name, version=''):
        self.dir = None
        self._name = name
        self.source = name + '-' + version
        self._version = version
        if not version:
            x, v, f = misc.split_ball (name)
            self._version = '.'.join (map (str, v[:-1]))
    def version (self):
        return self._version
    def name (self):
        return self._name
    def connect_logger (self, logger):
        pass

RepositoryProxy.register (Version)

class Darcs (Repository):
    vc_system = '_darcs'

    @staticmethod
    def create (rety, dir, source, branch='', module='', revision='', parameters=list ()):
        return Darcs (dir, source)
 
    def __init__ (self, dir, source=''):
        Repository.__init__ (self, dir, source)

    def darcs_pipe (self, cmd):
        dir = self.dir
        return self.read_pipe ('cd %(dir)s && darcs %(cmd)s' % locals ())

    def darcs (self, cmd):
        dir = self.dir
        return self.system ('cd %(dir)s && darcs %(cmd)s' % locals ())

    def get_revision_description (self):
        return self.darcs_pipe ('changes --last=1')
    
    def is_downloaded (self):
        return os.path.isdir (os.path.join (self.dir, self.vc_system))

    def download (self):
        source = self.source
        if not self.is_downloaded ():
            self.darcs ('pull -a %(source)s' % locals ())
        else:
            dir = self.dir
            self.system ('darcs get %(source)s %(dir)s' % locals ())
        
    def is_tracking (self):
        return True

    ## UGH.
    def xml_patch_name (self, patch):
        name_elts =  patch.getElementsByTagName ('name')
        try:
            return name_elts[0].childNodes[0].data
        except IndexError:
            return ''

    def checksum (self):
        xml_string = self.darcs_pipe ('changes --xml ')
        dom = xml.dom.minidom.parseString (xml_string)
        patches = dom.documentElement.getElementsByTagName ('patch')
        patches = [p for p in patches if not re.match ('^TAG', self.xml_patch_name (p))]

        patches.sort ()
        release_hash = md5.md5 ()
        for p in patches:
            release_hash.update (p.toxml ())

        return release_hash.hexdigest ()        

    def update_workdir (self, destdir):
        self.system ('mkdir -p %(destdir)s' % locals ())
        dir = self.dir
        
        verbose = ''

        # FIXME
        #if self.oslog and self.oslog.verbose >= self.oslog.commands:
        #   verbose = 'v'
        vc_system = self.vc_system
        self.system ('rsync --exclude %(vc_system)s -a%(verbose)s %(dir)s/* %(destdir)s/' % locals ())

    def read_file (self, file):
        dir = self.dir
        return self._read_file ('%(dir)s/%(file)s' % locals ())

RepositoryProxy.register (Darcs)
    
class TarBall (Repository):
    vc_system = '.tar'

    @staticmethod
    def create (rety, dir, source, branch='', module='', revision='', parameters=list ()):
        name = parameters.get ('name', [''])[0]
        version = parameters.get ('version', [''])[0]
        strip = parameters.get ('strip', [1])[0]
        strip = parameters.get ('strip_components', [strip])[0]
        return TarBall (dir, source, version, int (strip))

    @staticmethod
    def check_suffix (rety, url):
         return url and (url.endswith (rety.vc_system)
                         or url.endswith (rety.vc_system + '.gz')
                         or url.endswith ('.tgz')
                         or url.endswith (rety.vc_system + '.bz2'))

    def migrate (self, dir, dir_slash_vcs):
        pass

    # TODO: s/url/source
    def __init__ (self, dir, url, version=None, strip_components=1):
        Repository.__init__ (self, dir, url)

        self.dir = dir
        if not os.path.isdir (self.dir):
            self.system ('mkdir -p %s' % self.dir)

        self._version = version
        if not version:
            x, v, f = misc.split_ball (url)
            self._version = '.'.join (map (str, v[:-1]))

        self.branch = None
        self.strip_components = strip_components
        
    def is_tracking (self):
        return False

    def _file_name (self):
        return re.search ('.*/([^/]+)$', self.source).group (1)
    
    def is_downloaded (self):
        name = os.path.join (self.dir, self._file_name  ())
        return os.path.exists (name)
    
    def download (self):
        if self.is_downloaded ():
            return
        self.download_url (self.source, self.dir)

    def checksum (self):
        return misc.ball_basename (self._file_name ())
    
    def update_workdir (self, destdir):
        tarball = self.dir + '/' + self._file_name ()
        if os.path.isdir (destdir):
            self.system ('rm -rf %s' % destdir)
        self.system ('mkdir %s' % destdir)
        self._unpack (destdir, tarball)

    def _unpack (self, destdir, tarball):
        strip_components = self.strip_components
        _v = '-v'
        # fixme
        #if self.oslog:  #urg, will be fixed when .source is mandatory
        #    _v = self.oslog.verbose_flag ()
        _z = misc.compression_flag (tarball)
        self.system ('tar -C %(destdir)s --strip-component=%(strip_components)d %(_v)s%(_z)s -xf %(tarball)s' % locals ())

    def _unpack_promise_well_behaved (self, destdir, tarball):
        assert (self.strip_components == 1)
        _v = '-v'
        # fixme
        #if self.oslog:  #urg, will be fixed when .source is mandatory
        #    _v = self.oslog.verbose_flag ()
        _z = misc.compression_flag (tarball)
        status = self.system ('tar -C %(destdir)s/.. %(_v)s%(_z)s -xf %(tarball)s' % locals (), ignore_errors=True)
        if status:
            self.error ('retry using gzip pipe')
            self.system ('gzip -dc %(tarball)s | tar -C %(destdir)s/.. %(_v)s -xf-' % locals ())

    def version (self):
        return self._version

RepositoryProxy.register (TarBall)

class DebianPackage (TarBall):
    vc_system = '.deb'

    @staticmethod
    def create (rety, dir, source, branch='', module='', revision='', parameters=list ()):
        return DebianPackage (dir, source)

    @staticmethod
    def check_suffix (rety, url):
         return url and url.endswith (rety.vc_system)

    # TODO: s/url/source
    def __init__ (self, dir, url, version=None, strip_components=0):
        TarBall.__init__ (self, dir, url, version, strip_components)

    def _unpack (self, destdir, tarball):
        strip_components = self.strip_components
        # fixme
        #if self.oslog:  #urg, will be fixed when .source is mandatory
        #    _v = self.oslog.verbose_flag ()
        _v = ''   #     self.oslog.verbose_flag ()
        self.system ('ar p %(tarball)s data.tar.gz | tar -C %(destdir)s --strip-component=%(strip_components)d%(_v)s -zxf -' % locals ())

RepositoryProxy.register (DebianPackage)

class ZipFile (TarBall):
    vc_system = '.zip'

    @staticmethod
    def create (rety, dir, source, branch='', module='', revision='', parameters=list ()):
        return ZipFile (dir, source)

    @staticmethod
    def check_suffix (rety, url):
         return url and url.endswith (rety.vc_system)

    # TODO: s/url/source
    def __init__ (self, dir, url, version=None, strip_components=0):
        TarBall.__init__ (self, dir, url, version, strip_components)

    def _unpack (self, destdir, tarball):
        strip_components = self.strip_components
        # fixme
        #if self.oslog:  #urg, will be fixed when .source is mandatory
        #    _v = self.oslog.verbose_flag ()
        _v = ''   #     self.oslog.verbose_flag ()
        self.system ('unzip %(_v)s %(tarball)s -d %(destdir)s' % locals ())

RepositoryProxy.register (ZipFile)

class Git (Repository):
    vc_system = '.git'

    def migrate (self, dir, dir_slash_vcs):
        if os.path.isdir (os.path.join (dir, 'objects')):
            self.info ('migrating %(dir)s --> %(dir_slash_vcs)s' % locals ())
            self.system ('''
mkdir -p %(dir_slash_vcs)s
mv %(dir)s/* %(dir_slash_vcs)s
cd %(dir_slash_vcs)s && mv *bz2 *deb *gz *zip .. || :
''' % locals ())

    def __init__ (self, dir, source='', branch='', module='', revision=''):
        Repository.__init__ (self, dir, source)
        self.checksums = {}
        self.source = source
        if source:
            # urlib is not good at splitting ssh urls
            u = misc.Url (source)
            self.url_host = u.host
            self.url_dir = u.dir.replace ('~', '_')
            self.source = self.source.replace ('git+file://' + u.host, '')
            # ``I don't handle protocol git+http/http+git''
            self.source = self.source.replace ('git+http://', 'http://')
            self.source = self.source.replace ('http+git://', 'http://')
        else:
            # repository proxy determined git vcs from dir
            printf ('FIXME: get url from .git dir info')
            assert False
        self.branch = self.filter_branch_arg (branch)
        self.revision = revision
        if self.revision == '' and self.branch == '':
            # note that HEAD doesn't really exist as a branch name.
            self.branch = 'master'
        # We cannot do a shallow download if we're not tracking
        # we have to get at least enough history to include the
        # fixed committish ... :-)
        self.shallow = self.is_tracking ()
        assert self.url_host
        assert self.url_dir
    def version (self):
        return self.revision
    def is_tracking (self):
        return self.revision == ''
    def branch_dir (self):
        # this return something like lp.org//dir/master
        return '/'.join ([self.url_host, self.url_dir, self.branch])
    def full_branch_name (self):
        if self.is_tracking ():
            return self.branch_dir ().replace ('/', '-')
        return ''
    def __repr__ (self):
        b = self.branch
        return '#<GitRepository %s#%s>' % (self.dir, b)
    def get_revision_description (self):
        return self.git_pipe ('log --max-count=1 %(branch)s' % self.__dict__)  
    def read_file (self, file_name):
        ref = self.get_ref ()            
        contents = self.git_pipe ('show %(ref)s:%(file_name)s' % locals ())
        return contents
    def git_command (self, dir=None):
        if not dir:
            dir = self.dir
        return 'cd %(dir)s && git ' % locals ()
    def git (self, command, dir=None, ignore_errors=False):
        self.system (self.git_command (dir) + command,
                     ignore_errors=ignore_errors, env=self.get_env ())
    def git_pipe (self, command, dir=None, ignore_errors=False):
        return self.read_pipe (self.git_command (dir) + command,
                               ignore_errors=ignore_errors,
                               env=self.get_env ())
                               # ugh: suffer errors for now,
                               # python2.6 and up cannot handle
                               # overriding the logger here
                               # logger=self.logger.harmless)
    def is_downloaded (self):
        if not os.path.isdir (os.path.join (self.dir, 'refs')):
            return False
        ref = 'refs/heads/%(url_host)s/%(url_dir)s/%(branch)s' % self.__dict__
        if not os.path.exists (os.path.join (self.dir, ref)):
            return False
        if self.revision:
            result = self.git_pipe ('cat-file commit %s' % self.revision,
                                    ignore_errors=True)
            return bool (result)
        return True
    def download (self):
        if not self.have_client ():
            # sorry, no can do [yet]
            return
        if not os.path.isdir (os.path.join (self.dir, 'refs')):
            source = self.source
            dir = self.dir
            ### AARGH, GIT forces us to download the full history? WTF?
            '''invoking cd /home/janneke/vc/gub/downloads/ghostscript && git clone --depth 10 -l -s /home/janneke/vc/gub/downloads/ghostscript /home/janneke/vc/gub/target/mingw/src/ghostscript-0.0
Initialized empty Git repository in /home/janneke/vc/gub/target/mingw/src/ghostscript-0.0/.git/
fatal: attempt to fetch/clone from a shallow repository
fatal: The remote end hung up unexpectedly
'''
            if self.shallow:
                self.git ('clone --depth 1 --bare %(source)s %(dir)s' % locals (), dir='.')
            else:
                printf ('GIT: FIXME: shallow branching broken? -- getting *whole* history...')
                self.git ('clone --bare %(source)s %(dir)s' % locals (), dir='.')
        if self.branch and not (self.revision and self.is_downloaded ()):
            self.git ('fetch %(source)s %(branch)s:refs/heads/%(url_host)s/%(url_dir)s/%(branch)s' % self.__dict__)
        self.checksums = {}
    def get_ref (self):
        if not self.revision:
            return self.branch_dir ().replace ('//', '/')
        return self.revision
    def checksum (self):
        if self.revision:
            return self.revision
        branch = self.get_ref ()
        if branch in self.checksums:
            return self.checksums[branch]
        if not self.is_downloaded ():
            self.download ()
        if not self.is_downloaded () or not os.path.isdir (self.dir):
            return 'invalid'
        # can't use describe: fails in absence of tags.
        cs = self.git_pipe ('rev-list --max-count=1 %(branch)s' % locals ())
        cs = cs.strip ()
        self.checksums[branch] = cs
        return cs
    def all_files (self):
        branch = self.get_ref ()
        str = self.git_pipe ('ls-tree --name-only -r %(branch)s' % locals ())
        return str.split ('\n')
    def update_workdir (self, destdir):
        if self.shallow:
            self._update_workdir_shallow (destdir)
        else:
            self._update_workdir (destdir)
    def _update_workdir_shallow (self, destdir):
        branch = self.branch # branch is empty?
        branch = 'master'
        HEAD = self.checksum ()
        if HEAD == 'invalid':
            barf
        if not os.path.isdir (os.path.join (destdir, self.vc_system)):
            self.system ('mkdir -p %(destdir)s' % locals ())
            self.system ('cd %(destdir)s && git init' % locals ())
            open ('%(destdir)s/.git/objects/info/alternates' % locals (), 'w').write (os.path.join (self.dir, 'objects'))
#            self.system ('cd %(destdir)s && git reset --hard %(HEAD)s' % locals ())
        if self.git_pipe ('diff' % locals (), dir=destdir):
            self.system ('cd %(destdir)s && git reset --hard %(HEAD)s' % locals ())
        self.system ('cd %(destdir)s && git checkout %(HEAD)s' % locals ())
        self.system ('cd %(destdir)s && (git branch -D %(branch)s; git checkout -b %(branch)s)' % locals ())
    def _update_workdir (self, destdir):
        checkout_dir = self.dir
        branch = self.get_ref ()
        if os.path.isdir (os.path.join (destdir, self.vc_system)):
            revision = 'HEAD'
            if self.revision:
                revision = self.revision
                branch = self.branch
            if self.git_pipe ('diff', dir=destdir):
                self.git ('reset --hard %(revision)s' % locals (), dir=destdir)
            self.git ('pull %(checkout_dir)s %(branch)s:' % locals (), dir=destdir)
        else:
            self.git ('clone --depth 10 -l -s %(checkout_dir)s %(destdir)s' % locals ())
            revision = self.revision
            if not self.revision:
                revision = 'origin/' + self.get_ref ()
            self.git ('update-ref refs/heads/master %(revision)s' % locals (),
                      dir=destdir)
            self.git ('checkout master', dir=destdir)
            self.git ('reset --hard', dir=destdir)
    def get_diff_from_tag (self, tag):
        return self.git_pipe ('diff %(tag)s HEAD' % locals ())
    def last_patch_date (self):
        branch = self.branch
        s = self.git_pipe ('log --pretty=format:%ai -1 %(branch)s' % locals ())
        return tztime.parse (s, self.patch_dateformat)
    def tag (self, name):
        stamp = self.last_patch_date ()
        tag = name + '-' + tztime.format (stamp, self.tag_dateformat)
        branch = self.get_ref ()
        self.git ('tag %(tag)s %(branch)s' % locals ())
        return tag
    def tag_list (self, tag):
        return self.git_pipe ('tag -l %(tag)s*' % locals ()).split ('\n')

RepositoryProxy.register (Git)

class CVS (Repository):
    vc_system = 'CVS'
    cvs_entries_line = re.compile ('^/([^/]*)/([^/]*)/([^/]*)/([^/]*)/')

    @staticmethod
    def create (rety, dir, source, branch='', module='', revision='', parameters=list ()):
        if not branch:
            branch='HEAD'
        source = source.replace ('cvs::pserver', ':pserver')
        p = source.rfind ('/')
        if not module or module == '.':
            module=source[p+1:]
        return CVS (dir, source=source, module=module, tag=branch)

    def __init__ (self, dir, source='', module='', tag='HEAD'):
        Repository.__init__ (self, dir, source)
        self.module = module
        self.checksums = {}
        self.source = source
        self.tag = tag
        self.branch = tag # for vc_version_suffix

        branch_dir = os.path.join (self.dir, tag)
        if not os.path.isdir (branch_dir):
            self.system ('mkdir -p %s' % branch_dir)
            
    def _checkout_dir (self):
        return '%s/%s' % (self.dir, self.tag)

    def is_distributed (self):
        return False

    def is_tracking (self):
        return True ##FIXME

    def get_revision_description (self):
        try:
            contents = read_file ('ChangeLog')
            entry_regex = re.compile ('\n([0-9])')
            entries = entry_regex.split (contents)
            descr = entries[0]
            
            changelog_rev = ''
            
            for (name, version, date, dontknow) in self.cvs_entries (self.dir + '/CVS'):
                if name == 'ChangeLog':
                    changelog_rev  = version
                    break

            return ('ChangeLog rev %(changelog_rev)s\n%(description)s' %
                    locals ())
        
        except IOError:
            return ''

    def checksum (self):
        if self.tag in self.checksums:
            return self.checksums[self.tag]
        
        file = '%s/%s/.vc-checksum' % (self.dir, self.tag)

        if os.path.exists (file):
            cs = open (file).read ()
            self.checksums[self.tag] = cs
            return cs
        else:
            return '0'
        
    def read_file (self, file_name):
        return open (self._checkout_dir () + '/' + file_name).read ()
        
    def read_cvs_entries (self, dir):
        checksum = md5.md5 ()

        latest_stamp = 0
        for d in self.cvs_dirs (dir):
            for e in self.cvs_entries (d):
                (name, version, date, dontknow) = e
                checksum.update (name + ':' + version)

                if date == 'Result of merge':
                    raise Exception ('repository has been altered')
                
                stamp = time.mktime (time.strptime (date))
                latest_stamp = max (stamp, latest_stamp)

        version_checksum = checksum.hexdigest ()
        time_stamp = latest_stamp

        return (version_checksum, time_stamp)
    
    def update_workdir (self, destdir):
        dir = self._checkout_dir ()
        ## TODO: can we get deletes from vc?
        verbose = ''
#        if self.oslog and self.oslog.verbose >= oslog.level['command']:
#            verbose = 'v'
        self.system ('rsync -a%(verbose)s --delete --exclude CVS %(dir)s/ %(destdir)s' % locals ())
        
    def is_downloaded (self):
        dir = self._checkout_dir ()
        return os.path.isdir (dir + '/CVS')

    def download (self):
        suffix = self.tag
        rev_opt = '-r ' + self.tag
        source = self.source
        dir = self._checkout_dir ()
        lock_dir = locker.Locker (dir + '.lock')
        module = self.module
        cmd = ''
        if self.is_downloaded ():
            cmd += 'cd %(dir)s && cvs -q up -dCAP %(rev_opt)s' % locals ()
        else:
            checkout_dir = self.dir
            cmd += 'cd %(checkout_dir)s/ && cvs -d %(source)s -q co -d %(suffix)s %(rev_opt)s %(module)s''' % locals ()

        self.system (cmd)

        (cs, stamp) = self.read_cvs_entries (dir)

        open (dir + '/.vc-checksum', 'w').write (cs)
        open (dir + '/.vc-timestamp', 'w').write ('%d' % stamp)
        
    def cvs_dirs (self, branch_dir):
        retval =  []
        for (base, dirs, files) in os.walk (branch_dir):
            retval += [os.path.join (base, d) for d in dirs
                       if d == 'CVS']
            
        return retval

    def cvs_entries (self, dir):
        entries_str =  open (os.path.join (dir, 'Entries')).read ()

        entries = []
        for e in entries_str.split ('\n'):
            m = self.cvs_entries_line.match (e)
            if m:
                entries.append (tuple (m.groups ()))
        return entries
        
    def all_cvs_entries (self, dir):
        ds = self.cvs_dirs (dir)
        es = []
        for d in ds:
            ## strip CVS/
            basedir = os.path.split (d)[0]
            for e in self.cvs_entries (d):
                file_name = os.path.join (basedir, e[0])
                file_name = file_name.replace (self.dir + '/', '')

                es.append ((file_name,) + e[1:])
        return es

    def all_files (self, branch):
        entries = self.all_cvs_entries (self.dir + '/' + branch)
        return [e[0] for e in entries]
    
RepositoryProxy.register (CVS)

# FIXME: why are cvs, darcs, git so complicated?
class SimpleRepo (Repository):
    def __init__ (self, dir, source, branch, module='', revision='HEAD'):
        Repository.__init__ (self, dir, source)
        self.source = source
        self.revision = revision
        self.branch = branch
        self.module = module
        if not os.path.isdir (self.dir):
            self.system ('mkdir -p %(dir)s' % self.__dict__)
        if not source:
            self.source, self.branch = self._source ()

    def is_tracking (self):
        ## FIXME, probably wrong.
        return (not self.revision or self.revision == 'HEAD')

    def update_workdir (self, destdir):
        dir = self._checkout_dir ()
        self._copy_working_dir (dir, destdir)

    def is_downloaded (self):
        dir = self._checkout_dir ()
        return os.path.isdir (os.path.join (dir, self.vc_system))

    def download (self):
        if not self.is_downloaded ():
            self._checkout ()
        elif self._current_revision () != self.revision:
            self._update (self.revision)
        self.info ('downloaded version: ' + self.version ())

    def _copy_working_dir (self, dir, copy):
        vc_system = self.vc_system
        verbose = ''

#        if self.oslog and self.oslog.verbose >= oslog.level['command']:
#            verbose = 'v'
        self.system ('rsync -a%(verbose)s --exclude=%(vc_system)s %(dir)s/ %(copy)s'
                     % locals ())

    def _checkout_dir (self):
        # Support user-check-outs
        if os.path.isdir (os.path.join (self.dir, self.vc_system)):
            return self.dir
        return '%(dir)s/%(branch)s/%(module)s/%(revision)s' % self.__dict__

    def read_file (self, file_name):
        return self._read_file (self._checkout_dir () + '/' + file_name)

    def checksum (self):
        return self._current_revision ()

    def version (self):
        return ('-'.join ([self.branch, self.revision])
                .replace ('/', '-')
                .replace ('.-', '')
                .replace ('--', '-'))

class Subversion (SimpleRepo):
    vc_system = '.svn'
    patch_dateformat = '%Y-%m-%d %H:%M:%S %z'
    diff_xmldateformat = '%Y-%m-%d %H:%M:%S.999999'
    patch_xmldateformat = '%Y-%m-%dT%H:%M:%S'

    def migrate (self, dir, dir_slash_vcs):
        pass

    @staticmethod
    def create (rety, dir, source, branch, module='.', revision='HEAD', parameters=list (), branchmodule=''):
        source = source.replace ('svn:http://', 'http://')
        source = source.replace ('svn:https://', 'https://')
        if not branch and source:
            if source.endswith ('/'):
                source = source[:-1]
            branch = source[source.rindex ('/')+1:]
            source = source[:source.rindex ('/')]
        if (not module or module == '.') and '/' in branch:
            module = branch[branch.rindex ('/')+1:]
            branch = branch[:branch.rindex ('/')]
        branchmodule = parameters.get ('branchmodule', [branchmodule])[0]
        if not revision:
            revision = 'HEAD'
        return Subversion (dir, source=source, branch=branch,
                           module=module, branchmodule=branchmodule,
                           revision=revision, parameters=parameters)

    def __init__ (self, dir, source=None, branch='', module='.', branchmodule='.', revision='HEAD', parameters=list ()):
        if not branch and source:
            if source.endswith ('/'):
                source = source[:-1]
            branch = source[source.rindex ('/')+1:]
            source = source[:source.rindex ('/')]
        self.branchmodule = branchmodule
        self.parameters = parameters
        if not revision:
            revision = 'HEAD'
        SimpleRepo.__init__ (self, dir, source, branch, module, revision)

    def download (self):
        if not self.have_client ():
            # sorry, no can do [yet]
            return
        SimpleRepo.download (self)

    def is_distributed (self):
        return False

    def _checkout_dir (self):
        # Support user-check-outs
        if os.path.isdir (os.path.join (self.dir, self.vc_system)):
            return self.dir
        return '%(dir)s/%(module)s/%(branch)s/%(branchmodule)s/%(revision)s' % self.__dict__

    def _current_revision (self):
        return self.revision
        dir = self._checkout_dir ()
        ## UGH: should not parse user oriented output
        revno = self.read_pipe ('cd %(dir)s && LANG= svn log --limit=1'
                                % locals ())
        m = re.search  ('-+\nr([0-9]+) |', revno)
        assert m
        return m.group (1)
        
    def _checkout (self):
        dir = self.dir
        source = self.source
        branch = self.branch
        module = self.module
        branchmodule = self.branchmodule
        revision = self.revision
        checkout_dir = self._checkout_dir ()
        rev_opt = '-r %(revision)s ' % locals ()
        options = ''
        if 'depth' in self.parameters:
            options = '--depth=' + self.parameters.get ('depth')[0]
        cmd = 'cd %(dir)s && svn co %(options)s %(rev_opt)s %(source)s/%(module)s/%(branch)s/%(branchmodule)s %(checkout_dir)s''' % locals ()
        self.system (cmd)
        
    def _update (self, revision):
        dir = self._checkout_dir ()
        rev_opt = '-r %(revision)s ' % locals ()
        cmd = 'cd %(dir)s && svn up %(rev_opt)s' % locals ()
        self.system (cmd)

    def get_revision_description (self):
        dir = self._checkout_dir ()
        return self.read_pipe ('cd %(dir)s && LANG= svn log --verbose --limit=1'
                               % locals ())

    def get_diff_from_tag (self, tag):
        dir = self._checkout_dir ()
        source = self.source
        branch = self.branch
        module = self.module
        root = self._root ()
        return self.read_pipe ('cd %(dir)s && LANG= svn diff %(root)s/tags/%(tag)s %(source)s/%(branch)s/%(module)s/%(branchmodule)s' % locals ())

    # FIXME: use from_revision?  how to do play nicely with TagDb
    def get_diff_from_date (self, stamp):
#        date = tztime.format (stamp, self.patch_dateformat)
        date = tztime.format (stamp, self.diff_xmldateformat)
        dir = self._checkout_dir ()
        source = self.source
        branch = self.branch
        module = self.module
        root = self._root ()
        return self.read_pipe ('cd %(dir)s && TZ= LANG= svn diff -r "{%(date)s}"' % locals ())

    def _root (self):
        dir = self._checkout_dir ()
        ## UGH: should not parse user oriented output
        root = self.read_pipe ('cd %(dir)s && LANG= svn info' % locals ())
        m = re.search  ('.*Root: (.*)', root)
        assert m
        return m.group (1)

    def _source (self):
        dir = self._checkout_dir ()
        ## UGH: should not parse user oriented output
        s = self.read_pipe ('cd %(dir)s && LANG= svn info' % locals ())
        m = re.search  ('.*URL: (.*)', s)
        source = m.group (1)
        m = re.search  ('.*Repository Root: (.*)', s)
        root = m.group (1)
        assert source.startswith (root)
        branch = source[len (root)+1:]
        return root, branch

    def last_patch_date (self):
        dir = self._checkout_dir ()
        s = self.read_pipe ('cd %(dir)s && LANG= svn log --limit=1 --xml' % locals ())
#        m = re.search  ('Last Changed Date: (.*) \(', s)
        m = re.search  ('<date>(.*)\.[0-9]{6}Z</date>', s)
        date = m.group (1)
        return tztime.parse (date, self.patch_xmldateformat)
        
    def tag (self, name):
        source = self.source
        branch = self.branch
        module = self.module
        branchmodule = self.branchmodule
        revision = self.revision
        rev_opt = '-r %(revision)s ' % locals ()
        stamp = self.last_patch_date ()
        tag = name + '-' + tztime.format (stamp, self.tag_dateformat)
        root = self._root ()
        self.system ('svn cp -m "" %(rev_opt)s %(source)s/%(branch)s/%(module)s/%(branchmodule)s %(root)s/tags/%(tag)s''' % locals ())
        return tag

    def tag_list (self, tag):
        root = self._root ()
        lst = self.read_pipe ('LANG= svn ls %(root)s/tags' % locals ()).split (\
'\n')
        return [x for x in lst if x.startswith (tag)]

RepositoryProxy.register (Subversion)

class Bazaar (SimpleRepo):
    vc_system = '.bzr'

    @staticmethod
    def create (rety, dir, source, branch='', module='.', revision='', parameters=list ()):
        return Bazaar (dir, source, branch, module, revision)

    def __init__ (self, dir, source, branch='', module='.', revision='HEAD'):
        # FIXME: multi-branch repos not supported for now
        if not revision:
            revision = '0'
        SimpleRepo.__init__ (self, dir, source, branch, module, revision)

    def _current_revision (self):
        try:
            revno = self.bzr_pipe ('revno' % locals ())
        except:
            # FIXME: there's something nasty wrt version numbers, gub
            # wants to know about version before it downloads a repo.
            self.download ()
            revno = self.bzr_pipe ('revno' % locals ())
        assert revno
        return revno[:-1]

    def _checkout (self):
        dir = self.dir
        source = self.source
        revision = self.revision
        rev_opt = '-r %(revision)s ' % locals ()
        self.system ('''cd %(dir)s && bzr branch %(rev_opt)s %(source)s %(revision)s'''
                         % locals ())
        
    def _update (self, revision):
        rev_opt = '-r %(revision)s ' % locals ()
        source = self.source
        if not source:
            source = ''
        self.bzr_system ('pull %(rev_opt)s %(source)s' % locals ())

    def bzr_pipe (self, cmd):
        dir = self._checkout_dir ()
        return self.read_pipe ('cd %(dir)s && bzr %(cmd)s' % locals ())

    def bzr_system (self, cmd):
        dir = self._checkout_dir ()
        return self.system ('cd %(dir)s && bzr %(cmd)s' % locals ())

    def get_revision_description (self):
        return self.bzr_pipe ('log --verbose -r-1')

RepositoryProxy.register (Bazaar)

get_repository_proxy = RepositoryProxy.get_repository

def test ():
    import unittest

    for proxy in RepositoryProxy.repositories:
        printf (proxy, proxy.vc_system)

    # This is not a unittest, it only serves as a smoke test mainly as
    # an aid to get rid safely of the global non-oo repository_proxy
    # stuff
    global get_repository_proxy

    class Test_get_repository_proxy (unittest.TestCase):
        def setUp (self):
            os.system ('rm -rf downloads/test')
        def testCVS (self):
            repo = get_repository_proxy ('downloads/test/', 'cvs::pserver:anonymous@cvs.savannah.gnu.org:/sources/emacs')
            self.assertEqual (repo.__class__, CVS)
        def testTarBall (self):
            repo = get_repository_proxy ('downloads/test/', 'http://ftp.gnu.org/pub/gnu/hello/hello-2.3.tar.gz')
            self.assertEqual (repo.__class__, TarBall)
        def testGit (self):
            repo = get_repository_proxy ('downloads/test/', 'git://git.kernel.org/pub/scm/git/git')
            self.assertEqual (repo.__class__, Git)
        def testLocalGit (self):
            os.system ('mkdir -p downloads/test/git')
            os.system ('cd downloads/test/git && git init')
            repo = get_repository_proxy ('downloads/test/', 'file://' + os.getcwd () + '/downloads/test/git')
            self.assertEqual (repo.__class__, Git)
        def testBazaar (self):
            repo = get_repository_proxy ('downloads/test/', 'bzr:http://bazaar.launchpad.net/~yaffut/yaffut/yaffut.bzr')
            self.assertEqual (repo.__class__, Bazaar)
        def testBazaarGub (self):
            os.system ('mkdir -p downloads/test/bzr')
            cwd = os.getcwd ()
            os.chdir ('downloads/test/bzr')
            repo = get_repository_proxy ('.', 'bzr:http://bazaar.launchpad.net/~yaffut/yaffut/yaffut.bzr')
            self.assert_ (os.path.isdir ('.gub.bzr'))
            os.chdir (cwd)
        def testGitSuffix (self):
            # FIXME: this is currently used to determine flavour of
            # downloads/lilypond.git.  But is is ugly and fragile;
            # what if I do brz branch foo foo.git?

            # This is now broken, with SimpleGit; good riddance
            pass
# #            repo = get_repository_proxy ('/foo/bar/barf/i-do-not-exist-or-possibly-am-of-bzr-flavour.git', '')
# #            self.assertEqual (repo.__class__, Git)
        def testPlusSsh (self):
            repo = get_repository_proxy ('downloads/test/', 'bzr+ssh://bazaar.launchpad.net/~yaffut/yaffut/yaffut.bzr')
            self.assertEqual (repo.__class__, Bazaar)
            repo = get_repository_proxy ('downloads/test/', 'git+ssh://git.sv.gnu.org/srv/git/lilypond.git')
            self.assertEqual (repo.__class__, Git)
            repo = get_repository_proxy ('downloads/test/', 'svn+ssh://svn.gnome.org/svn/gnome-hello', 'trunk', '')
            self.assertEqual (repo.__class__, Subversion)
        def testGitTagAndDiff (self):
            os.system ('mkdir -p downloads/test/git')
            os.system ('cd downloads/test/git && git init')
# # FIXME Git/SimpleGit mixup: git-dir vs checkout-dir
# #           repo = Git (os.getcwd () + '/downloads/test/git/.git')
            repo = Git (os.getcwd () + '/downloads/test/git/')
            os.system ('cd downloads/test/git && echo one >> README')
            os.system ('cd downloads/test/git && git add .')
            os.system ('cd downloads/test/git && git commit -m "1"')
            time.sleep (1)
            repo.tag ('success-test')
            os.system ('cd downloads/test/git && echo two >> README')
            os.system ('cd downloads/test/git && git add .')
            os.system ('cd downloads/test/git && git commit -m "2"')
            repo.tag ('success-test')
            os.system ('cd downloads/test/git && echo three >> README')
            os.system ('cd downloads/test/git && git add .')
            os.system ('cd downloads/test/git && git commit -m "3"')
            patch = '''
@@ -1,2 +1,3 @@
 one
 two
+three
'''
            diff = repo.get_diff_from_tag_base ('success-test')
            self.assert_ (diff.find (patch) >=0)
        def testSnvTagAndDiff (self):
            os.system ('mkdir -p downloads/test/svn')
            os.system ('cd downloads/test/svn && svnadmin create .repo')
            os.system ('cd downloads/test/svn && svn co file://localhost$(pwd)/.repo root')
            repo = Subversion (os.getcwd () + '/downloads/test/svn/root')
            tag_db = TagDb ('downloads/test')
            os.system ('cd downloads/test/svn/root && mkdir trunk tags')
            os.system ('cd downloads/test/svn/root && svn add trunk tags')
            os.system ('cd downloads/test/svn/root && svn commit -m "init"')
            os.system ('cd downloads/test/svn && svn co file://localhost$(pwd)/.repo/trunk trunk')
            repo = Subversion (os.getcwd () + '/downloads/test/svn/trunk')
            os.system ('cd downloads/test/svn/trunk && echo one >> README')
            os.system ('cd downloads/test/svn/trunk && svn add README')
            os.system ('cd downloads/test/svn/trunk && svn commit -m "1"')
            os.system ('cd downloads/test/svn/trunk && svn up')
            repo.tag ('success-test')
            tag_db.tag ('success-test', repo)
            os.system ('cd downloads/test/svn/trunk && echo two >> README')
            os.system ('cd downloads/test/svn/trunk && svn commit -m "2"')
            os.system ('cd downloads/test/svn/trunk && svn up')
            os.system ('cd downloads/test/svn/trunk && svn info > info')
            repo.tag ('success-test')
            tag_db.tag ('success-test', repo)
            os.system ('cd downloads/test/svn/trunk && echo three >> README')
            os.system ('cd downloads/test/svn/trunk && svn commit -m "3"')
            patch = '''
@@ -1,2 +1,3 @@
 one
 two
+three
'''
            diff = repo.get_diff_from_tag_base ('success-test')
            self.assert_ (diff.find (patch) >=0)
            diff = tag_db.get_diff_from_tag_base ('success-test', repo)
            self.assert_ (diff.find (patch) >=0)

    suite = unittest.makeSuite (Test_get_repository_proxy)
    unittest.TextTestRunner (verbosity=2).run (suite)

def get_cli_parser ():
    p = optparse.OptionParser ()

    p.usage = '''repository.py [OPTION]...

Repository helper.

'''
    p.description = 'Grand Unified Builder.  Repository helper.'

    p.add_option ('-t', '--test',
                  action='store_true',
                  dest='test',
                  default=False,
                  help='run repository tests')
    p.add_option ('--full-branch-name',
                  action='store_true',
                  dest='full_branch_name',
                  default=False,
                  help='print full branch name')
    p.add_option ('--branch-dir',
                  action='store_true',
                  dest='branch_dir',
                  default=False,
                  help='print branch dir')
    return p

def main ():
    cli_parser = get_cli_parser ()
    (options, files) = cli_parser.parse_args ()
    if 0:
        pass
    elif options.test:
        test ()
    elif options.full_branch_name:
        repo = get_repository_proxy ('.', files[0])
        printf (repo.full_branch_name ())
    elif options.branch_dir:
        repo = get_repository_proxy ('.', files[0])
        printf (repo.branch_dir ().replace ('//', '/'))

if __name__ == '__main__':
    main ()
