#!/usr/bin/env python2.5

import urllib2
import os
import re
import csv
import sys
import logging
from mechanize import Browser
from decimal import Decimal

LOGIN_PAGE_URL = "https://signin.ebay.com/ws/eBayISAPI.dll?SignIn"

LOGIN_FORM_USERNAME = "userid"
LOGIN_FORM_PASSWORD = "pass"
LOGIN_FORM_NAME = "SignInForm"
LOGIN_FORM_URL = "https://scgi.ebay.com/ws/eBayISAPI.dll?RegisterEnterInfo&amp;siteid=0&amp;co_partnerid=2&amp;UsingSSL=1"
ITEM_BASE = "http://cgi.ebay.com/ws/eBayISAPI.dll?ViewItem&viewitem=&item="

LISTING_BID_FORM_NAME = "PlaceBidForm1"

BID_FORM_NAME = "PlaceBid"
BID_FORM_MAXBID = "maxbid"

USERNAME = 'YOURUSERNAMEHERE'
PASSWORD = 'YOURPASSWORDHERE'

class EbayBidder(object):
    def __init__(self):
        self.br = Browser()
        self.logger = logging.getLogger("EbayBidder")
        logger = self.logger
        logger.setLevel(logging.DEBUG)
        #create console handler and set level to debug
        ch = logging.StreamHandler()
        ch.setLevel(logging.DEBUG)
        #create formatter
        formatter = logging.Formatter("%(asctime)s - %(name)s - %(levelname)s - %(message)s")
        #add formatter to ch
        ch.setFormatter(formatter)
        #add ch to logger
        logger.addHandler(ch)
        self.amount_re = re.compile('<input type=\"text\" name=\"maxbid\".+?US \$(\d+?\.\d\d)')
        try:
            self.setup()
        except Exception, e:
            self.logger.error("Error while trying to initialize ebay bidder: %s" % e)
        
    def setup(self):
        br = self.br
        self.logger.info("Attempting to initialize ebay bidder")
        self.logger.debug("Connecting to %s " % LOGIN_PAGE_URL)
        br.open(LOGIN_PAGE_URL)
        br.select_form(name=LOGIN_FORM_NAME)
        br[LOGIN_FORM_USERNAME] = USERNAME
        br[LOGIN_FORM_PASSWORD] = PASSWORD
        br.submit()
        self.logger.info("Ebay bidder initialized.")
        return True
        
    def get_itemurl(self, itemnumber):
        return ITEM_BASE + itemnumber
        
    def bid(self, item):
        self.logger.info('Bidding on %s' % item)
        self.br.open(self.get_itemurl(item))
        self.br.select_form(name=LISTING_BID_FORM_NAME)
        self.br.submit()
        self.logger.debug('Got the bid page %s' % self.br.response().geturl())
        data = self.br.response().read()
        self.br.select_form(name=BID_FORM_NAME)
        amount = Decimal(self.amount_re.findall(data.replace('\n','').replace('\r',''))[0])
        amount = amount + Decimal('0.50')
        self.logger.info('About to bid %s' % str(amount))
        self.br[BID_FORM_MAXBID] = str(amount)
        self.br.submit()
        self.logger.debug("Confirming bid")
        self.br.select_form(name=BID_FORM_NAME)
        self.br.submit()
        self.logger.info("Dont bidding for %s" % item)

def main(itemid = None):
    
    if not itemid:
        try:
            itemid = sys.argv[1]
        except:
            print "USAGE: ./bidder.py <itemid>"
            return -1

    e = EbayBidder()
    e.bid(itemid)
    return 0
        
if __name__ == '__main__':
    main()
