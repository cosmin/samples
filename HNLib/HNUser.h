//
//  User.h
//  HackerNews
//
//  Created by Cosmin Stejerean on 11/7/09.
//  Copyright 2009 Cosmin Stejerean. All rights reserved.
//

#import <Foundation/Foundation.h>


@interface HNUser : NSObject {
    NSString *sourceUrl;
    
    NSString *username;
    NSString *about;
    NSString *since;
    NSNumber *karma;
}

@property (nonatomic, retain) NSString *sourceUrl;
@property (nonatomic, retain) NSString *username;
@property (nonatomic, retain) NSString *about;
@property (nonatomic, retain) NSString *since;
@property (nonatomic, retain) NSNumber *karma;

- (NSArray *) getMostRecentComments;
- (NSArray *) getMostRecentPosts;

@end
