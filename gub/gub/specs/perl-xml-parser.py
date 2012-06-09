from gub import tools

class Perl_xml_parser (tools.CpanBuild):
    source = 'http://search.cpan.org/CPAN/authors/id/M/MS/MSERGEANT/XML-Parser-2.34.tar.gz'
    dependencies = ['expat']
