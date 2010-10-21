//
//  HNComment.h
//  HackerNews
//
//  Created by Cosmin Stejerean on 11/7/09.
//  Copyright 2009 Cosmin Stejerean. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "HNUser.h"
#import "HNPost.h"

@interface HNComment : NSObject {
    HNUser *author;
    HNPost *post;
    HNComment *parent;
    NSNumber *score;
    NSArray *comments;
}

@property (nonatomic, retain) HNUser *author;
@property (nonatomic, retain) HNPost *post;
@property (nonatomic, retain) HNComment *parent;
@property (nonatomic, retain) NSNumber *score;
@property (nonatomic, retain) NSArray *comments;

@end
