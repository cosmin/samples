import urllib2
import re
from sqlobject import *
import os
sqlhub.processConnection = connectionForURI('sqlite:%s' % os.path.abspath('restaurants.db'))

from restaurant import Restaurant

cuisine = re.compile('\$+?.+?<font.+?\>(\S+?)<br>')
phone = re.compile('Phone:\s</b>(\S+?)<br>')
hours = re.compile('Hours.+?<pre.+?>(.+?)</pre>')

def getdata(data, retype):
    d = retype.findall(data)
    if len(d) > 0:
        return d[0]
    else:
        return None

for r in Restaurant.select():
    data =  urllib2.urlopen(r.url).read().replace('\n', '').replace('\r','')
    print "Currently processing restaurant id %s" % r.id
    r.cuisine = getdata(data, cuisine) #.findall(data)[0]
    r.phone = getdata(data, phone) #.findall(data)[0]
    r.hours = getdata(data, hours) #.findall(data)[0]
