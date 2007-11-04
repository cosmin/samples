from pylons import config
from sqlalchemy import Column, MetaData, Table, types
from sqlalchemy.orm import mapper, relation
from sqlalchemy.orm import scoped_session, sessionmaker
from sqlalchemy import ForeignKey

from constants import *
from person import Person
from phonenumber import PhoneNumber
from emailaddress import EmailAddress

# Global session mapper
Session = scoped_session(sessionmaker(autoflush = True, transactional = True, bind=config['pylons.g'].sa_engine))

# Global metadata
metadata = MetaData()

tbl_person = Table('people', metadata,
    Column('id', types.Integer, primary_key=True),
    Column('first_name', types.String),
    Column('last_name', types.String),
    Column('full_name', types.String), # name as entered by the user
    Column('picture', types.String),
    )
    
#tbl_streetaddress = Table('streetaddresses', metadata,
#    Column('id', types.Integer, primary_key=True),
#    Column('person_id', types.Integer, ForeignKey('people.id')),
#    Column('full_streetaddress', types.String), # address as entered by the user
#    Column('type', types.String),
#    Column('street1', types.String),
#    Column('street2', types.String),
#    Column('city', types.String),
#    Column('state', types.String),
#    Column('zipcode', types.String)
#    )
    
tbl_emailaddress = Table('emailaddresses', metadata,
    Column('id', types.Integer, primary_key=True),
    Column('person_id', types.Integer, ForeignKey('people.id')),
    Column('full_emailaddress', types.String), # email address as entered by the user
    Column('type', types.String),
    Column('user', types.String),
    Column('domain', types.String),
    Column('internal', types.Boolean),
    )
    
tbl_phonenumber = Table('phonenumbers', metadata,
    Column('id', types.Integer, primary_key=True),
    Column('person_id', types.Integer, ForeignKey('people.id')),
    Column('full_phonenumber', types.String), # phonenumber as entered by the user
    Column('type', types.String),
    Column('areacode', types.String),
    Column('phonenumer', types.String),
    Column('extension', types.String),
    )
    
#class StreetAddress(object):
#    pass

mapper(EmailAddress, tbl_emailaddress)
mapper(PhoneNumber, tbl_phonenumber)
#mapper(StreetAddress, tbl_streetaddress)

mapper(Person, tbl_person, properties = {
    #'streetaddresses' : relation(StreetAddress),
    'phonenumbers' : relation(PhoneNumber),
    'emailaddresses' : relation(EmailAddress),
    })
    