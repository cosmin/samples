from constants import *

class EmailAddress(object):
    def __init__(self, email = None, typ = WORK_TYPE):
        if email:
            self.full_emailaddress = email
            self.type = typ