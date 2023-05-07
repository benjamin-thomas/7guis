# Setup steps:
#   source venv/bin/activate
#   pip install -f https://extras.wxpython.org/wxPython4/extras/linux/gtk3/ubuntu-20.04 wxPython
import wx

from main_frame import MainFrame

if __name__ == '__main__':
    app = wx.App()

    f = MainFrame(None, title="Counter Example")
    f.SetClientSize(400,300)
    f.Center()
    f.Show()

    app.MainLoop()
