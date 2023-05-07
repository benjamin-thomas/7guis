import wx


class MainFrame(wx.Frame):
    counter: int = 0

    def __init__(self, *args, **kw):
        super(MainFrame, self).__init__(*args, **kw)

        panel = wx.Panel(self)

        self.input = wx.TextCtrl(panel, value=str(self.counter))
        self.input.Enable(False)

        btn = wx.Button(panel, label="Counter")

        v_sizer = wx.BoxSizer(wx.VERTICAL)
        h_sizer = wx.BoxSizer(wx.HORIZONTAL)

        h_sizer.Add(self.input, 40, wx.ALIGN_CENTER)
        h_sizer.AddStretchSpacer(1)
        h_sizer.Add(btn, 20, wx.ALIGN_CENTER)

        v_sizer.AddStretchSpacer()
        v_sizer.Add(h_sizer, 0, wx.ALIGN_CENTER)
        v_sizer.AddStretchSpacer()

        self.Bind(wx.EVT_BUTTON, self.on_click)

        panel.SetSizer(v_sizer)

    def on_click(self, _evt):
        self.counter += 1
        self.input.SetValue(str(self.counter))
