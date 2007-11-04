#!/usr/bin/env python
# encoding: utf-8
"""
downloader.py

Created by Cosmin Stejerean on 2007-06-02.
Copyright (c) 2007 Cosmin Stejerean. All rights reserved.
"""

import sys
import os
import urllib2
import re
import pickle

BASE_URL = 'http://chicago.menupages.com/'
CITY = 'Chicago, IL'

detail_re = re.compile('href=\'(' + 'restaurantdetails.asp' + '.+?)\'>(.+?)</a>.*?<br>(.+?)</td>.+?(\$+)</font>')

chicago_urls = {
  'downtown' : 'http://chicago.menupages.com/restaurants.asp?areaid=25',
  'north' : 'http://chicago.menupages.com/restaurants.asp?areaid=26',
  'northwest' : 'http://chicago.menupages.com/restaurants.asp?areaid=27',
  'west' : 'http://chicago.menupages.com/restaurants.asp?areaid=28',
  'southwest' : 'http://chicago.menupages.com/restaurants.asp?areaid=30',
  'south' : 'http://chicago.menupages.com/restaurants.asp?areaid=29'
}
    
 
def get_restaurant_location(restaurant):
    return restaurant[2].replace('\t','').split('|')[0]
    #potentially add logic here to retrieve zip code as well
    
def get_area_restaurants(url):
    data = urllib2.urlopen(url).read().replace('\r','').replace('\n', '')
    return detail_re.findall(data)
    
def prepare_data(restaurant,geocoder = get_default_geocoder(), city = CITY, base_url = BASE_URL):
    r = restaurant
    print "     -> %s" % r[1]
    street = get_restaurant_location(r)
     
    data = {'name': r[1], 
            'url' : base_url + r[0],
            'price' : len(r[3]),
            'address' : "",
            'original' : street,
            'lat' : None, 
            'lng' : None}
    return data
    
def fetch_all():
    restaurants = []
    print "Initializing geocoder..."
    print "Fetching restaurant information..."
    for area in chicago_urls.keys():
        print "==> Fetching restaurant list for area %s" % area
        url = chicago_urls[area] 
        for r in get_area_restaurants(url):
            try:
                restaurants.append(prepare_data(r))
            except Exception, e:
                print "ERROR when processing restaurant %s" % r
                continue
                
    return restaurants
    
def save_data(data, filename ='restaurants.lst'):
    out = open(filename, 'w')
    pickle.dump(data, out)
    out.flush()
    print "SAVED results to %s" % filename
    out.close()
    
def main():
    save_data(fetch_all())

if __name__ == '__main__':
    main()

