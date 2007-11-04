from sqlobject import *

class Restaurant(SQLObject):
    name = StringCol()
    url = StringCol()
    address = StringCol()
    original = StringCol()
    price = IntCol()
    phone = StringCol()
    cuisine = StringCol()
    hours = StringCol()
    lat = DecimalCol(size=17,precision=15)
    lng = DecimalCol(size=17,precision=15)

