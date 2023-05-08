# To set up stubs properly for the IDE, see:
#    https://pypi.org/project/PyGObject-stubs
#
# pip install pygobject-stubs --no-cache-dir --config-settings=config=Gtk3,Gdk3,Soup
#
# To install a package:
#   source venv/bin/activate
#   pip install package_name
#   pip freeze > requirements.tx
#
# Documentation is here:
#   https://python-gtk-3-tutorial.readthedocs.io/en/latest/button_widgets.html
#   https://lazka.github.io/pgi-docs/Gtk-3.0/classes/Entry.html#properties
#   https://lazka.github.io/pgi-docs/Gtk-3.0/classes/Widget.html#Gtk.Widget.set_sensitive

import gi

gi.require_version("Gtk", "3.0")
from gi.repository import Gtk


class Window(Gtk.Window):
    counter = 0
    entry = Gtk.Entry()

    def __init__(self):
        super().__init__(title="Counter App")

        self.entry.set_text(str(self.counter))
        self.entry.set_sensitive(False)  # A widget is sensitive if the user can interact with it (grayed out otherwise)

        btn = Gtk.Button.new_with_label("Counter")
        btn.connect("clicked", self.on_btn_click)

        h_box = Gtk.Box(spacing=4)
        h_box.pack_start(self.entry, False, False, 0)
        h_box.pack_start(btn, False, False, 0)
        h_box.set_halign(Gtk.Align.CENTER)  # center horizontally

        v_box = Gtk.VBox()
        v_box.pack_start(h_box, False, False, 0)
        v_box.set_valign(Gtk.Align.CENTER)  # center vertically

        self.add(v_box)
        self.set_default_size(400, 300)

    def on_btn_click(self, _btn):
        self.counter += 1
        self.entry.set_text(str(self.counter))


win = Window()
win.connect("destroy", Gtk.main_quit)
win.show_all()
Gtk.main()
