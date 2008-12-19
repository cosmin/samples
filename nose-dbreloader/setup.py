#from distutils.core import setup

from setuptools import setup, find_packages

setup(name='nose_dbreloader',
      version='0.2.1',
      packages = ['nose_dbreloader'],
      author = "Cosmin Stejerean",
      author_email = 'cosmin@offbytwo.com',
      entry_points = {
        'nose.plugins.0.10': [
            'nose_dbreloader = nose_dbreloader.dbreloader:DatabaseSnapshotPlugin',
         ]
      },
)

