from gub import misc
from gub import repository
from gub import target

class Portaudio (target.AutoBuild):
    #source = 'http://www.portaudio.com/archives/pa_stable_v19_20071207.tar.gz'
    # useless, changes every night
    source = 'http://www.portaudio.com/archives/pa_snapshot.tgz'
    #source = 'svn:https://subversion.assembla.com/svn/portaudio/portaudio/trunk&revision=1428'
    dependencies = [
        'tools::automake',
        'tools::libtool',
        'tools::pkg-config',
        ]
    def __init__ (self, settings, source):
        target.AutoBuild.__init__ (self, settings, source)
        if isinstance (source, repository.Subversion):
            source.version = misc.bind_method (lambda x: repository.Repository.version_from_pc_in (x, 'portaudio-2.0.pc.in'), source)
        if 'snapshot' in Portaudio.source:
            # version_from_* does not work with tar ball, hardcode for now
            source.version = misc.bind_method (lambda x: '19', source)

class only_for_stable_Portaudio__mingw (Portaudio):
    def patch (self):
        Portaudio.patch (self)
        for i in ['%(srcdir)s/configure.in',
                  '%(srcdir)s/configure']:
            self.file_sub ([('((src/os/win)/pa_win_util.o)',
                             r'\1 \2/pa_win_waveformat.o',)],
                           i)




'''

libtool: link: i686-apple-darwin8-gcc  -dynamiclib  -o lib/.libs/libportaudio.2.dylib  src/common/.libs/pa_allocation.o src/common/.libs/pa_converters.o src/common/.libs/pa_cpuload.o src/common/.libs/pa_dither.o src/common/.libs/pa_debugprint.o src/common/.libs/pa_front.o src/common/.libs/pa_process.o src/common/.libs/pa_skeleton.o src/common/.libs/pa_stream.o src/common/.libs/pa_trace.o src/os/mac_osx/.libs/pa_mac_hostapis.o src/os/unix/.libs/pa_unix_util.o src/hostapi/coreaudio/.libs/pa_mac_core.o src/hostapi/coreaudio/.libs/pa_mac_core_utilities.o src/hostapi/coreaudio/.libs/pa_mac_core_blocking.o src/common/.libs/pa_ringbuffer.o   -framework CoreAudio -framework AudioToolbox -framework AudioUnit -framework Carbon  -Wl,-headerpad_max_install_names -isysroot /Developer/SDKs/MacOSX10.4u.sdk -mmacosx-version-min=10.3   -framework CoreAudio -framework AudioToolbox -framework AudioUnit -framework Carbon -install_name  /usr/lib/libportaudio.2.dylib -compatibility_version 3 -current_version 3.0 -Wl,-exported_symbols_list,lib/.libs/libportaudio-symbols.expsym
/home/janneke/vc/gub/target/darwin-x86/root/usr/cross/bin/i686-apple-darwin8-ld: can't locate framework for: -framework CoreAudio


or

/home/janneke/vc/gub/target/darwin-x86/root/usr/cross/bin/i686-apple-darwin8-ld: Undefined symbols:
_AudioConverterDispose
_AudioConverterFillBuffer
_AudioConverterNew
_AudioConverterReset
_AudioConverterSetProperty
_AudioDeviceAddPropertyListener
_AudioDeviceGetCurrentTime
_AudioDeviceGetProperty
_AudioDeviceGetPropertyInfo
_AudioDeviceRemovePropertyListener
_AudioHardwareGetProperty
_AudioHardwareGetPropertyInfo
_AudioOutputUnitStart
_AudioOutputUnitStop
_AudioUnitAddPropertyListener
_AudioUnitGetProperty
_AudioUnitInitialize
_AudioUnitRender
_AudioUnitReset
_AudioUnitSetProperty
_AudioUnitUninitialize
_CFRelease
_CFStringCreateWithFormat
_CFStringGetCString
_CFStringGetLength
_CloseComponent
_FindNextComponent
_OpenAComponent
___CFStringMakeConstantString
_AudioDeviceSetProperty
collect2: ld returned 1 exit status

'''

class Portaudio__darwin (Portaudio):
    configure_variables = (Portaudio.configure_variables
                           + ''' CFLAGS='-DMACH_KERNEL=1 -Wno-multichar' ''')
    def patch (self):
        Portaudio.patch (self)
        for i in ['%(srcdir)s/configure.in',
                  '%(srcdir)s/configure']:
            self.file_sub ([(' -Werror', ''),
                            ('mac_sysroot=".*"', 'mac_sysroot=""')
                            ], i)
        # FIXME: this can't be right.  Move to darwin-sdk?
        self.system ('mkdir -p %(builddir)s/include')
        self.dump ('''
typedef int decform;
typedef int decimal;
typedef int CGEventFilterMask;
typedef int CGEventSuppressionState;
''', '%(builddir)s/include/ansi_fp.h')
        for framework in [
            'AE',
            'DiskArbitration',
            'IOKit',
#            'OSServices',
            ]:
            self.system ('''
#ln -sf ../../System/Library/Frameworks/Kernel.framework/Headers/%(framework)s %(srcdir)s/usr/include/%(framework)s
mkdir -p %(builddir)s/include/kernel
ln -sf %(system_root)s/System/Library/Frameworks/Kernel.framework/Headers/%(framework)s %(builddir)s/include/kernel/%(framework)s
''', locals ())
        for framework in [
            'AE',
            'ATS',
            'CFNetwork',
            'CarbonCore',
            'CarbonSound',
            'ColorSync',
            'CommonPanels',
            'CoreGraphics',
            'FindByContent',
            'HIServices',
            'HIToolbox',
            'HTMLRendering',
            'Help',
            'IOKit',
            'ImageCapture',
            'ImageIO',
            'Ink',
            'LangAnalysis',
            'LaunchServices',
            'Metadata',
            'NavigationServices',
            'OpenScripting',
            'OSServices',
            'Print',
            'PrintCore',
            'QD',
            'SearchKit',
            'SecurityHI',
            'SpeechRecognition',
            'SpeechSynthesis',
            'WebServicesCore',
            ]:
            self.system ('''
#ln -sf ../../Developer/Headers/CFMCarbon/%(framework)s %(srcdir)s/usr/include/%(framework)s
mkdir -p %(builddir)s/include
ln -sf %(system_root)s/Developer/Headers/CFMCarbon/%(framework)s %(builddir)s/include/%(framework)s
''', locals ())
        for framework in [
            'ApplicationServices',
            'AudioToolbox',
            'AudioUnit',
            'Carbon',
            'CoreAudio',
            'CoreFoundation',
            'CoreMIDI',
            'CoreServices',
            'DiskArbitration',
            ]:
            self.system ('''
#ln -sf ../../System/Library/Frameworks/%(framework)s.framework/Headers %(srcdir)s/usr/include/%(framework)s
mkdir -p %(builddir)s/include
ln -sf %(system_root)s/System/Library/Frameworks/%(framework)s.framework/Headers %(builddir)s/include/%(framework)s
''', locals ())
        for i in ['%(srcdir)s/configure.in',
                  '%(srcdir)s/configure']:
            self.file_sub ([('-arch i386 -arch ppc', '-I%(system_prefix)s/include -I%(builddir)s/include -I%(builddir)s/include/kernel'),], i)
        self.file_sub ([
                ('^(typedef.*)',
'''/* \1 */
#include <kernel/IOKit/IOTypes.h>
''')],
                       '%(builddir)s/include/IOKit/IOKitLib.h')

class Portaudio__darwin__ppc (Portaudio__darwin):
    patches = Portaudio__darwin.patches + [
        'portaudio-darwin-ppc-pthread-cancelled.patch',
        ]
    def patch (self):
        Portaudio__darwin.patch (self)
        self.system ('mkdir -p %(builddir)s/include/libkern')
        self.dump ('''
#define OSMemoryBarrier() __sync_synchronize()
#define OSAtomicOr32( a, b ) ( (*(b)) |= (a) )
#define OSAtomicAnd32( a, b ) ( (*(b)) &= (a) )
''', '%(builddir)s/include/libkern/OSAtomic.h')
