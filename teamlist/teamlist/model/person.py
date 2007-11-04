from emailaddress import EmailAddress
from phonenumber import PhoneNumber
from constants import *

class Person(object):
    
    def _phone_from_type(self, phone_type):
        for phone in self.phonenumbers:
            if phone.type == phone_type:
                return phone.full_phonenumber
        return ""
        
    def update_email(self, typ, value):
        if (typ == WORK_TYPE and not self.work_email):
            self.emailaddresses.append(EmailAddress(value, typ))
            return True
        else:
            for ph in self.phonenumbers:
                if ph.type == typ:
                    ph.full_phonenumber = value
                    return True
        return False
    def update_phone(self, typ, value):
        if (typ == WORK_TYPE and not self.work_phone) or (typ == CELL_TYPE and not self.cell_phone):
            self.phonenumbers.append(PhoneNumber(value, typ))
            return True
        else:
            for ph in self.phonenumbers:
                if ph.type == typ:
                    ph.full_phonenumber = value
                    return True
        return False
    
    @property
    def work_phone(self):
        """While the data model is extensible and can support many tymes of phone the current implementation
        uses hard coded values for work and cell phone types, the only ones we care about so far"""
        return self._phone_from_type(WORK_TYPE)
                
    @property
    def cell_phone(self):
        """Get the cellphone number as entered by the user"""
        return self._phone_from_type(CELL_TYPE)
        
    @property
    def work_email(self):
        """While the data model can support many email types currently only work email is implemented"""
        for email in self.emailaddresses:
            if email.type == WORK_TYPE:
                return email.full_emailaddress