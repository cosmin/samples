import logging
import simplejson

import random
import string
import os
import shutil

from teamlist.lib.base import *
from teamlist.model import *
import teamlist.model
log = logging.getLogger(__name__)

permanent_storage = os.path.join(os.path.dirname(os.path.dirname(__file__)), 'public/uploads')
allowed_extensions = ['jpg', 'jpeg', 'gif', 'bmp', 'png']

class PeopleController(BaseController):
    """Controller for managing people"""
    def _random_filename(self, input_name):
        ext = input_name.rsplit(os.path.extsep)[-1]
        if ext.lower() not in allowed_extensions:
            log.info('User supplied extension of %s from input name of %s is not allowed' % (ext, input_name))
            return None
        chars = string.letters + string.digits
        
        for x in range(1,5):
            filename = ''
            for i in range(12):
                filename=  filename + random.choice(chars)
            filename = filename + "." + ext.lower()
            full_file = self._full_file_path(filename)
            if not os.path.exists(full_file):
                return filename
            else:
                log.warn('File with random name %s already exists.' % filename)
                    
        log.error('You must have really bad luck. Tried 5 random filenames and they all exist.')
        return None
            
    def _full_file_path(self, filename):
        return os.path.join(permanent_storage, filename)

    def _file_url(self, filename):
        return '/uploads/' + filename

    def index(self, format='html'):
        person_q = Session.query(Person)
        
        if format == 'html':
            people = list(person_q)
            return render('people.listpeople', people = people)
        elif format == 'json':
            # return a json list of all the person ids
            return simplejson.dumps([p.id for p in person_q])
        else:
            return "ERROR: Unrecognized format!"
            
        Session.rollback()

    def create(self):
        person = Person()
        try:
            pic = request.POST['picture']
            pic_filename = self._random_filename(pic.filename)
            if not pic_filename:
                # hard coded temporary hack to display an error message instead of redirecting back to edit form
                return redirect_to('/error/document/?message="Cannot use uploaded image, most likely bad image type. Must be jpg, jpeg, gif, bmp or png. User your back button to go back and try again"')
            else:
                pic_out = open(self._full_file_path(pic_filename), 'wb')
                shutil.copyfileobj(pic.file, pic_out)
                pic.file.close()
                pic_out.close()
                person.picture = self._file_url(pic_filename)
        except:
            log.warn("not uploading a file")
        person.full_name = request.POST['full_name']
        person.update_phone(model.constants.WORK_TYPE, request.POST['work_phone'])
        person.update_phone(model.constants.CELL_TYPE, request.POST['cell_phone'])
        person.update_email(model.constants.WORK_TYPE, request.POST['work_email'])
        Session.save(person)
        Session.commit()
        id = person.id
        return redirect_to(h.url_for(controller='person', action='show', id=id))

    def new(self, format='html'):
        return render('people.newperson')

    def update(self, id):
        person = Session.query(Person).filter_by(id=id).one()
        try:
            pic = request.POST['picture']
            pic_filename = self._random_filename(pic.filename)
            if not pic_filename:
                # hard coded temporary hack to display an error message instead of redirecting back to edit form
                return redirect_to('/error/document/?message="Cannot use uploaded image, most likely bad image type. Must be jpg, jpeg, gif, bmp or png. User your back button to go back and try again"')
            else:
                pic_out = open(self._full_file_path(pic_filename), 'wb')
                shutil.copyfileobj(pic.file, pic_out)
                pic.file.close()
                pic_out.close()
                person.picture = self._file_url(pic_filename)
        except:
            log.warn("not uploading a file")
        person.full_name = request.POST['full_name']
        person.update_phone(model.constants.WORK_TYPE, request.POST['work_phone'])
        person.update_phone(model.constants.CELL_TYPE, request.POST['cell_phone'])
        person.update_email(model.constants.WORK_TYPE, request.POST['work_email'])
        Session.save(person)
        Session.commit()
        return redirect_to(h.url_for(controller='people'))
 
    def delete(self, id):
        person = Session.query(Person).filter_by(id=id).one()
        Session.delete(person)
        Session.commit()
        return redirect_to(h.url_for(controller='people'))

    def show(self, id, format='html'):
        person = Session.query(Person).filter_by(id=id).one()
        Session.rollback()
        return render('people.viewperson', person = person)

    def edit(self, id, format='html'):
        person = Session.query(Person).filter_by(id=id).one()
        Session.rollback()
        return render('people.editperson', person = person)





