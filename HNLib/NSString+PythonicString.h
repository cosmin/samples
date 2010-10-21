//
//  NSString+PythonicString.h
//  HackerNews
//
//  Created by Cosmin Stejerean on 11/8/09.
//  Copyright 2009 Cosmin Stejerean. All rights reserved.
//

#import <Foundation/Foundation.h>


@interface NSString (PythonicString) 

- (BOOL) startsWith:(NSString *)other;
- (BOOL) equalsIgnoreCase:(NSString *)other;

@end

