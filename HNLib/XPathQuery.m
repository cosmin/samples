//
//  XPathQuery.m
//  HackerNews
//
//  Created by Cosmin Stejerean on 11/8/09
//  based on code by Matt Gallagher with the following license
//
//  Permission is given to use this source code file, free of charge, in any
//  project, commercial or otherwise, entirely at your risk, with the condition
//  that any redistribution (in part or whole) of source code must retain
//  this copyright and permission notice. Attribution in compiled projects is
//  appreciated but not required.
//

#import "XPathQuery.h"

#import <libxml/tree.h>
#import <libxml/parser.h>
#import <libxml/HTMLparser.h>
#import <libxml/xpath.h>
#import <libxml/xpathInternals.h>

NSDictionary *DictionaryForNode(xmlNodePtr currentNode, NSMutableDictionary *parentResult)
{
	NSMutableDictionary *resultForNode = [NSMutableDictionary dictionary];
	
	if (currentNode->name)
	{
		NSString *currentNodeContent =
			[NSString stringWithCString:(const char *)currentNode->name encoding:NSUTF8StringEncoding];
		[resultForNode setObject:[currentNodeContent lowercaseString] forKey:@"name"];
	}
	
	if (currentNode->content && currentNode->type != XML_DOCUMENT_TYPE_NODE)
	{
		NSString *currentNodeContent =
			[NSString stringWithCString:(const char *)currentNode->content encoding:NSUTF8StringEncoding];
		
		if ([[resultForNode objectForKey:@"name"] isEqual:@"text"] && parentResult)
		{
			currentNodeContent = [currentNodeContent
				stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]];
			
			NSString *existingContent = [parentResult objectForKey:@"text"];
			NSString *newContent;
			if (existingContent)
			{
				newContent = [existingContent stringByAppendingString:currentNodeContent];
			}
			else
			{
				newContent = currentNodeContent;
			}

			[parentResult setObject:newContent forKey:@"text"];
			return nil;
		}
		
		[resultForNode setObject:currentNodeContent forKey:@"text"];
	}
	
	xmlAttr *attribute = currentNode->properties;
	if (attribute)
	{
        NSMutableDictionary *attributeDictionary = [NSMutableDictionary dictionary];
		while (attribute)
		{
			NSString *attributeName = [NSString stringWithCString:(const char *)attribute->name encoding:NSUTF8StringEncoding];
			if (attributeName && attribute->children)
			{
                NSString *attributeValue = [NSString stringWithCString:(const char *)attribute->children->content encoding:NSUTF8StringEncoding];
                if (attributeValue) {
                    [attributeDictionary setObject:attributeValue forKey:attributeName];
                }
            }
			attribute = attribute->next;
		}
		
		if ([attributeDictionary count] > 0)
		{
			[resultForNode setObject:attributeDictionary forKey:@"attributes"];
		}
	}

	xmlNodePtr childNode = currentNode->children;
	if (childNode)
	{
		NSMutableArray *childContentArray = [NSMutableArray array];
		while (childNode)
		{
			NSDictionary *childDictionary = DictionaryForNode(childNode, resultForNode);
			if (childDictionary)
			{
				[childContentArray addObject:childDictionary];
			}
			childNode = childNode->next;
		}
		if ([childContentArray count] > 0)
		{
			[resultForNode setObject:childContentArray forKey:@"children"];
		}
	}
	
	return resultForNode;
}

NSArray *PerformXPathQuery(xmlDocPtr doc, NSString *query)
{
    xmlXPathContextPtr xpathCtx; 
    xmlXPathObjectPtr xpathObj; 

    /* Create xpath evaluation context */
    xpathCtx = xmlXPathNewContext(doc);
    if(xpathCtx == NULL)
	{
		NSLog(@"Unable to create XPath context.");
		return nil;
    }
    
    /* Evaluate xpath expression */
    xpathObj = xmlXPathEvalExpression((xmlChar *)[query cStringUsingEncoding:NSUTF8StringEncoding], xpathCtx);
    if(xpathObj == NULL) {
		NSLog(@"Unable to evaluate XPath.");
		return nil;
    }
	
	xmlNodeSetPtr nodes = xpathObj->nodesetval;
	if (!nodes)
	{
		NSLog(@"Nodes was nil.");
		return nil;
	}
	
	NSMutableArray *resultNodes = [NSMutableArray array];
	for (NSInteger i = 0; i < nodes->nodeNr; i++)
	{
		NSDictionary *nodeDictionary = DictionaryForNode(nodes->nodeTab[i], nil);
		if (nodeDictionary)
		{
			[resultNodes addObject:nodeDictionary];
		}
	}

    /* Cleanup */
    xmlXPathFreeObject(xpathObj);
    xmlXPathFreeContext(xpathCtx); 
    
    return resultNodes;
}

NSArray *PerformHTMLXPathQuery(NSData *document, NSString *query)
{
    xmlDocPtr doc;

    /* Load XML document */
	doc = htmlReadMemory([document bytes], [document length], "", NULL, HTML_PARSE_NOWARNING | HTML_PARSE_NOERROR);
	
    if (doc == NULL)
	{
		NSLog(@"Unable to parse.");
		return nil;
    }
	
	NSArray *result = PerformXPathQuery(doc, query);
    xmlFreeDoc(doc); 
	
	return result;
}

NSArray *PerformXMLXPathQuery(NSData *document, NSString *query)
{
    xmlDocPtr doc;
	
    /* Load XML document */
	doc = xmlReadMemory([document bytes], [document length], "", NULL, XML_PARSE_RECOVER);
	
    if (doc == NULL)
	{
		NSLog(@"Unable to parse.");
		return nil;
    }
	
	NSArray *result = PerformXPathQuery(doc, query);
    xmlFreeDoc(doc); 
	
	return result;
}
