import re
import os
import web
import simplejson
from sqlobject import *

#Initialize connection
sqlhub.processConnection = connectionForURI('sqlite:%s' % os.path.abspath('restaurants.db'))

import sys
sys.path.append('./lib/')
from restaurant import Restaurant

urls = (
  '/index.html', 'Index',
  '/search/q=(.*)', 'Search',
  '/localsearch\?query=(\S+)top=(\S*)&bot=(\S*)&left=(\S*)&right=(\S*)', 'LocalSearch'
)

class Index:
    def GET(self):
        data = open('index.html', 'r')
        print data.read()
        data.close()
        
class Search:
    def display_results(self, restaurants):
        data = []
        query = "chop"
        like_string = "%" + query + "%"
        rq = Restaurant.q
        
        for r in restaurants:
            if not r.address or len(r.address) == 0 or r.lat == None or r.lng == None:
                continue
                
            data.append({'name'         : r.name,
                         'url'          : r.url,
                         'price'        : r.price,
                         'phone'        : r.phone,
                         'lat'          : str(r.lat),
                         'lng'          : str(r.lng),
                         'address'      : r.address,
                         'cuisine'      : r.cuisine,
                         'hours'        : r.hours,
                        })
        return simplejson.dumps(data)

    def GET(self, q):
        #TODO: this really ought to make a lookup into an index, but for now let's just search the database
        like_string = "%" + q + "%"
        print "\n"
        rq = Restaurant.q
        print self.display_results(Restaurant.select(LIKE(rq.name, like_string)).limit(50))

    
class LocalSearch(Search):
    def GET(self, query, s_top, s_bot, s_left, s_right):
        top = Decimal(s_top)
        bot = Decimal(s_bot)
        left = Decimal(s_left)
        right = Decimal(s_right)
        
        like_string = "%" + query + "%"
        
        rq = Restaurant.q
        results = Restaurant.select(AND(
            rq.lat >= bot,
            rq.lat <= top,
            rq.lng >= left,
            rq.lng <= right,
            LIKE(rq.name, like_string)  
        ))
        print self.display_results(results)
    

web.webapi.internalerror = web.debugerror
if __name__ == "__main__": web.run(urls, globals())
