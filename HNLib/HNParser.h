//
//  HNParser.h
//  HackerNews
//
//  Created by Cosmin Stejerean on 11/7/09.
//  Copyright 2009 Cosmin Stejerean. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "HNPost.h"
#import "HNUser.h"

NSString *ExtractHrefFromNode(NSDictionary *node);
HNPost *TransformPostNodesToPost(NSArray *nodes);
NSArray *TransformPosts(NSArray *postsToTransform);

@interface HNParser : NSObject {
}

+ (NSArray *) getTopStories:(NSData *)document;
+ (NSArray *) getNewestStories:(NSData *)document;
+ (HNPost *) getPostWithComments:(NSData *)document;
+ (HNUser *) getUserInfo:(NSData *)document;

@end
