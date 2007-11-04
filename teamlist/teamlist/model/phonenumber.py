from constants import *

class PhoneNumber(object):
    def __init__(self, phone = None, typ = WORK_TYPE):
        if phone:
            self.full_phonenumber = phone
            self.type = typ