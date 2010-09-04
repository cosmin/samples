from setuptools import setup, find_packages
setup(
    name = "classmocks",
    version = "0.2",
    packages = find_packages(),
    #scripts = ['say_hello.py'],
    # Project uses reStructuredText, so ensure that the docutils get
    # installed or upgraded on the target machine
    # install_requires = ['docutils>=0.3'],
    #package_data = {
    # If any package contains *.txt or *.rst files, include them:
    #'': ['*.txt', '*.rst'],
    # And include any *.msg files found in the 'hello' package, too:
    #'hello': ['*.msg'],
    #}

    # metadata for upload to PyPI
    author = "Cosmin Stejerean",
    author_email = "cstejerean@gmail.com",
    description = "Something similar to python-mocks but meant for testing parts of a class",
    license = "MIT",
    keywords = "python testing",
    url = "http://github.com/cosmin/classmocks/",
    )
