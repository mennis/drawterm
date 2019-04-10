#import <Cocoa/Cocoa.h>

@interface AppDelegate : NSObject <NSApplicationDelegate, NSWindowDelegate>
{
	NSCursor *_arrowCursor;
}

@property (assign) NSCursor *arrowCursor;
@property (strong) NSMenuItem *fsmenuitem;
@end
