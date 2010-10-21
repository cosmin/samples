//
//  NSString+PythonicString.m
//  HackerNews
//
//  Created by Cosmin Stejerean on 11/8/09.
//  Copyright 2009 Cosmin Stejerean. All rights reserved.
//

#import "NSString+PythonicString.h"


@implementation NSString (PythonicString)

- (BOOL) startsWith:(NSString *)other {
    return [[self substringToIndex:[other length]] isEqualToString:other];    
}

- (BOOL) equalsIgnoreCase:(NSString *)other {
    return [[self lowercaseString] isEqualToString:[other lowercaseString]];
}

@end
