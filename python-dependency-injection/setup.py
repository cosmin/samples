from setuptools import setup, find_packages
import sys, os

version = '0.2'

setup(name='IoC',
      version=version,
      description="Python dependency injection",
      long_description="""\
This module provides a simple dependency injection container for Python which allows lazy resolution of dependencies""",
      classifiers=[], # Get strings from http://www.python.org/pypi?%3Aaction=list_classifiers
      keywords='ioc inversion control loose coupling dependency dependencies injection',
      author='Cosmin Stejerean',
      author_email='cosmin@offbytwo.com',
      url='http://github.com/offbytwo/pydi',
      license='MIT',
      packages=find_packages(exclude=['ez_setup', 'examples', 'tests']),
      include_package_data=True,
      zip_safe=False,
      install_requires=[
          # -*- Extra requirements: -*-
      ],
      entry_points="""
      # -*- Entry points: -*-
      """,
      )
