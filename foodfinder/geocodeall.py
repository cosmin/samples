import os, pickle, sys
from sqlobject import *
from geopy import geocoders
import thread

sqlhub.processConnection = connectionForURI('sqlite:%s' % os.path.abspath('restaurants.db'))

from restaurant import Restaurant

def get_address(start_index, count):
    g = geocoders.GeocoderDotUS()
    num = Restaurant.select().count()
    x = start_index
    while x < start_index + count and x <= num:
        r = Restaurant.selectBy(id = x).getOne()
        try:
            print "Geocoding %s located at %s" % (r.id, r.original)
            place, (lat, lng) = g.geocode(r.original + ', Chicago, IL')
            r.address = place
            r.lat = lat
            r.lng = lng
        except Exception, e:
            print "Error: %s" % e
        x = x + 1

if __name__ == '__main__':
  get_address(int(sys.argv[1]), int(sys.argv[2]))
