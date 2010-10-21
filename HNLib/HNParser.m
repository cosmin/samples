//
//  HNParser.m
//  HackerNews
//
//  Created by Cosmin Stejerean on 11/7/09.
//  Copyright 2009 Cosmin Stejerean. All rights reserved.
//

#import "HNParser.h"
#import "XPathQuery.h"

#import "NSString+PythonicString.h"

#import "HNPost.h"
#import "HNUser.h"

#define HN_XPATH_STORIES @"/html/body/center/table/tr[3]/td/table/tr/*/*"

NSNumber *ExtractNumberFromNode(NSDictionary *node) {
    NSString *text = [node objectForKey:@"text"];
    NSString *numericPart = [[text componentsSeparatedByString:@" "] objectAtIndex:0];
    return [NSNumber numberWithInt:[numericPart integerValue]];
}

NSString *ExtractHrefFromNode(NSDictionary *node) {
    return [[node objectForKey:@"attributes"] objectForKey:@"href"];
}

HNPost *TransformPostNodesToPost(NSArray *nodes) {
    HNPost *post = [[HNPost alloc] init];
    [post autorelease];
    int currentIndex = 0;

    NSDictionary *titleAndLink = [nodes objectAtIndex:currentIndex++];
    post.title = [titleAndLink objectForKey:@"text"];
    post.url = ExtractHrefFromNode(titleAndLink);
    
    if ([post.url startsWith:@"item"]) {
        post.sourceUrl = ExtractHrefFromNode(titleAndLink);
    } else {
        currentIndex++; // skip over the short url bit
    }
    
    post.score = ExtractNumberFromNode([nodes objectAtIndex:currentIndex++]);

    NSDictionary *authorAndLink = [nodes objectAtIndex:currentIndex++];
    HNUser *user = [[HNUser alloc] init];
    user.username = [authorAndLink objectForKey:@"text"];
    user.sourceUrl = ExtractHrefFromNode(authorAndLink);
    post.author = user;
    [user release];
    
    if ([nodes count] > currentIndex) {
        NSDictionary *comments = [nodes objectAtIndex:currentIndex];
        post.numberOfComments = ExtractNumberFromNode(comments);
        post.sourceUrl = ExtractHrefFromNode(comments);
    }
    
    return post;
}

NSArray *TransformPosts(NSArray *postsToTransform) {
    NSMutableArray *posts = [NSMutableArray array];

    NSEnumerator *enumerator = [postsToTransform objectEnumerator];
    NSArray *element;
    
    while (element = [enumerator nextObject]) {
        [posts addObject:TransformPostNodesToPost(element)];
    }
    return [NSArray arrayWithArray:posts];
}

@implementation HNParser


+ (NSArray *) getTopStories:(NSData *)document {
    NSArray *nodes = PerformHTMLXPathQuery(document, HN_XPATH_STORIES);
    NSEnumerator *enumerator = [nodes objectEnumerator];
    NSDictionary *element;
    
    NSMutableArray *posts = [NSMutableArray array];
    NSMutableArray *storyNodes = NULL;
    
    while (element = [enumerator nextObject]) {
        if ([@"center" isEqualToString:[element objectForKey:@"name"]]) {
            if (storyNodes != NULL) {
                [posts addObject:storyNodes];
                [storyNodes release];
            }
            storyNodes = [[NSMutableArray alloc] initWithCapacity:5];
            continue;
        }
        [storyNodes addObject:element];
    }
    
    [posts addObject:storyNodes];
    [storyNodes release];
    return TransformPosts(posts);
}

+ (NSArray *) getNewestStories:(NSData *)document {
    return NULL;
}

+ (HNPost *) getPostWithComments:(NSData *)document {
    return NULL;
}

+ (HNUser *) getUserInfo:(NSData *)document {
    return NULL;
}


@end
