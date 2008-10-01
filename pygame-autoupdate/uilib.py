import pygame
from pygame import Color, Rect

# this code requires python 2.5, is that bad?
import functools

pygame.init()

def set_dirty_to(value):
    """decorator to mark widget dirty after fn is called"""
    def dec(fn):
        @functools.wraps(fn)
        def wrapper(self, *args, **kw):
            ret = fn(self, *args, **kw)
            self._dirty = value
            return ret
        return wrapper

    return dec


class DisplayManager(object):
    widgets = []
    named_widgets = {}
    
    def __init__(self, width, height):
        self.screen = pygame.display.set_mode((width, height))

    def add_widget(self, widget, left = None, top = None, name = None):

        if left is None or top is None:
            (l, t) = self.center_coords(widget)
            if top is None:
                top = t
            if left is None:
                left = t
            
        widget.top = top
        widget.left = left
        self.widgets.append(widget)

        if isinstance(name, str):
            self.named_widgets[name] = widget

    def center_coords(self, widget):
        l = (self.width - widget.width) / 2.0
        t = (self.height - widget.height) / 2.0

        return (int(round(l)), int(round(t)))

    def by_name(self, widget_name):
        return self.named_widgets.get(widget_name)

    def quit(self):
        pygame.display.quit()

    @property
    def dirty_widgets(self):
        for w in self.widgets:
            if w.needs_update:
                yield w

    def refresh(self):
        for w in self.dirty_widgets:
            self.screen.blit(w.render(), (w.left, w.top))

        pygame.display.update()

class DisplayWidget(object):
    width = None
    height = None

    # don't touch these, used by DisplayManager for now
    top = None
    left = None

    _dirty = True

    def __init__(self, width, height):
        self.width = width
        self.height = height

    @property
    def needs_update(self):
        return self._dirty

    @set_dirty_to(False)
    def render(self):
        """This method should return a surface to be rendered onto the screen"""
        return self._render()

    def _render(self):
        return pygame.Surface(width, height)

class ProgressBar(DisplayWidget):
    """Widget representing a progress bar"""
    
    def __init__(self, maxvalue = 100):
        """for now the sizes are hardcoded"""
        DisplayWidget.__init__(self, 600, 40)
        self.value = self.minvalue = 0
        self.maxvalue = maxvalue
        self._value = self.minvalue
        self.bgcolor = Color("black")
        self._create_background()

    @set_dirty_to(True)
    def set_value(self, val):
        self._value = val
        self._dirty = True

    def get_value(self):
        return self._value

    value = property(get_value, set_value)

    def _create_background(self):
        s = pygame.Surface((self.width, self.height))
        s.fill(self.bgcolor)
        pygame.draw.rect(s, Color("white"), Rect(0,0,self.width,self.height), 2)
        self._background = s

    def _render(self):
        s = self._background.copy()
        fill_width = (self.width - 4) * (1.0 * self.value / self.maxvalue)
        pygame.draw.rect(s, Color("blue"), Rect(2,2,fill_width, self.height - 4))
        return s

class MessageWidget(DisplayWidget):
    """Widget responsible for rendering text"""

    def __init__(self, width, height, color = "white", message = " ", font_size = 12):
        self.message = message
        self.color = Color(color)
        self.width = width
        self.height = height
        self.font = pygame.font.Font(pygame.font.get_default_font(), font_size)
        self._create_background()

    def _create_background(self):
        self._background = pygame.Surface((self.width, self.height))

    @set_dirty_to(True)
    def set_message(self, val):
        self._message = val

    def get_message(self):
        return self._message

    message = property(get_message, set_message)

    def _render(self):
        s = self._background.copy()
        s.blit(self.font.render(self.message, True, self.color), (0,0))
        return s
        

