try:
    from setuptools import setup, find_packages
except ImportError:
    from ez_setup import use_setuptools
    use_setuptools()
    from setuptools import setup, find_packages

setup(
    name='teamlist',
    version="",
    description='TeamList',
    author='Cosmin Stejerean',
    author_email='cosmin@offbytwo.com',
    #url='',
    install_requires=["Pylons>=0.9.6"],
    packages=find_packages(exclude=['ez_setup']),
    include_package_data=True,
    test_suite='nose.collector',
    package_data={'teamlist': ['i18n/*/LC_MESSAGES/*.mo']},
    #message_extractors = {'teamlist': [
    #        ('**.py', 'python', None),
    #        ('templates/**.mako', 'mako', None),
    #        ('public/**', 'ignore', None)]},
    entry_points="""
    [paste.app_factory]
    main = teamlist.config.middleware:make_app

    [paste.app_install]
    main = pylons.util:PylonsInstaller
    """,
)
