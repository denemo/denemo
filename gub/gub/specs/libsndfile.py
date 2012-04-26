from gub import target

class Libsndfile (target.AutoBuild):
    source = 'http://pkgs.fedoraproject.org/repo/pkgs/libsndfile/libsndfile-1.0.21.tar.gz/880a40ec636ab2185b97f8927299b292/libsndfile-1.0.21.tar.gz'
    #permissions are incorrect on server
    #source = 'http://www.mega-nerd.com/libsndfile/files/libsndfile-1.0.21.tar.gz'
    dependencies = [
        'tools::automake',
        'tools::pkg-config',
        'libtool',
        'sqlite'
        ]

class Libsndfile__darwin__x86 (Libsndfile):
    dependencies = [x for x in Libsndfile.dependencies
                if x.replace ('-devel', '') not in [
                'sqlite', # Included in darwin-x86 SDK, hmm?
                ]]
