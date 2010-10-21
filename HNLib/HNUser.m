//
//  User.m
//  HackerNews
//
//  Created by Cosmin Stejerean on 11/7/09.
//  Copyright 2009 Cosmin Stejerean. All rights reserved.
//

#import "HNUser.h"


@implementation HNUser

@synthesize sourceUrl;

@synthesize username;
@synthesize about;
@synthesize since;
@synthesize karma;

- (NSArray *) getMostRecentComments {
    return NULL;
}

- (NSArray *) getMostRecentPosts {
    return NULL;
}

@end
