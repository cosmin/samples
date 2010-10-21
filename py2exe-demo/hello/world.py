import wx

class HelloWorldFrame(wx.Frame):
    def __init__(self, parent, id, title):
        wx.Frame.__init__(self, parent, id, title,
                          pos=(150,150), size=(480,320))
        panel = wx.Panel(self)
        self.lbl_message1 = wx.StaticText(panel, -1, "Hello")
        self.lbl_message2 = wx.StaticText(panel, -1, "World")

        space = 6

        sizer = wx.FlexGridSizer(cols=2, hgap=space, vgap=space)
        sizer.AddMany([self.lbl_message1, self.lbl_message2])
        border = wx.BoxSizer(wx.VERTICAL)
        border.Add(sizer, 0, wx.ALL, 25)

        panel.SetSizer(border)
        panel.Layout()

class HelloWorldApp(wx.App):
    def OnInit(self):
        self.frame = HelloWorldFrame(None, -1, "HelloWorld")
        self.frame.Show(True)
        self.SetTopWindow(self.frame)
        return True

if __name__ == "__main__":
    app = HelloWorldApp(False)
    app.MainLoop()
