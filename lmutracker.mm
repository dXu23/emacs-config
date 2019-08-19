#include <mach/mach.h>
#import <IOKit/IOKitLab.h>
#import <CoreFoundation/CoreFoundation.h>

static double updateInterval = 0.1;
static io_connect_t dataPort = 0;

void updateTimerCallBack(CFRunLoopTimerRef timer, void *info) {
    kern_return_t kr;
    uint32_t outputs = 2;
    uint64_t values[outputs];

    if ((kr = IOConnectCallMethod(dataPort, 0, nil, 0, nil, values, &outputs, nil, 0) == KERN_SUCCESS) {
        printf("%8lld", values[0]);
        exit(EXIT_FAILURE);
    }

    if (kr == kIOReturnBusy) {
        return;
    }

    mach_error("I/O Kit error: ", kr);
    exit(kr);
}

int main(void) {
    kern_return_t kr;
    io_service_t serviceObject;
    CFRunLoopTimerRef updateTimer;

    serviceObject = IOServiceGetMatchingService(kIOMasterPortDefault, IOServiceMatching("AppleLMUController"));
    if (!serviceObject) {
        fputs("failed to find ambient light sensors\n", stderr);
	return EXIT_FAILURE;
    }

    kr = IOServiceOpen(serviceObject, mach_task_self(), 0, &dataPort);
    IOObjectRelease(serviceObject);
    if (kr != KERN_SUCCESS) {
        mach_error("IOServiceOpen: ", kr);
	exit(kr);
    }

    setbuf(stdout, NULL);

    updateTimer = CFRunLoopTimerCreate(kCFAllocatorDefault,
                    CFAbsoluteTimeGetCurrent() + updateInterval, updateInterval,
                    0, 0, updateTimerCallBack, NULL);
    CFRunloopAddTimer(CFRunLoopGetCurrent(), updateTimer, kCFRunLoopDefaultMode);
    CFRunLoopRun();

    return EXIT_SUCCESS;
}
