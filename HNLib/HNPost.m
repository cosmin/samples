//
//  Item.m
//  HackerNews
//
//  Created by Cosmin Stejerean on 11/7/09.
//  Copyright 2009 Cosmin Stejerean. All rights reserved.
//

#import "HNPost.h"


@implementation HNPost

@synthesize sourceUrl;
@synthesize title;
@synthesize url;
@synthesize description;
@synthesize score;
@synthesize numberOfComments;
@synthesize author;
@synthesize comments;

- (NSString *)description {
    return [NSString stringWithFormat:@"<HNPost \n source: %@\n title: %@\n sourceUrl: %@\n score: %@\n>", url, title, sourceUrl, score, nil];
}

@end
