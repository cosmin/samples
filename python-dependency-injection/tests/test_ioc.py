from IoC import component, Inject, inject
import unittest

class Greeter(object):
    greeting = Inject('greeting')

    @inject(person='person')
    def greet(self, person):
        return "%s %s" % (self.greeting.value, person.name)

@component('greeting')
class Greeting(object):
    value = "Hello"

@component('person')
class Person(object):
    def __init__(self, name='world'):
        self.name = name

class TestIOC(unittest.TestCase):
    def test_injection(self):
        greeter = Greeter()
        self.assertEquals("Hello world", greeter.greet())

    def test_injection_override(self):
        greeter = Greeter()
        self.assertEquals("Hello bob", greeter.greet(person=Person('bob')))
