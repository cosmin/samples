//
//  Item.h
//  HackerNews
//
//  Created by Cosmin Stejerean on 11/7/09.
//  Copyright 2009 Cosmin Stejerean. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "HNUser.h"

@interface HNPost : NSObject {
    NSString *sourceUrl;
    
    NSString *title;
    NSString *url;
    NSString *description;
    NSNumber *score;
    NSNumber *numberOfComments;
    HNUser *author;
    
    NSArray *comments;
}

@property (nonatomic, retain) NSString *sourceUrl;
@property (nonatomic, retain) NSString *title;
@property (nonatomic, retain) NSString *url;
@property (nonatomic, retain) NSString *description;
@property (nonatomic, retain) NSNumber *score;
@property (nonatomic, retain) NSNumber *numberOfComments;
@property (nonatomic, retain) HNUser *author;
@property (nonatomic, retain) NSArray *comments;


@end
