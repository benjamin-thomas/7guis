#ifndef COUNTER_MAINFRAME_H
#define COUNTER_MAINFRAME_H

#include <wx/wx.h>

class MainFrame : public wxFrame {
public:
    explicit MainFrame(const wxString &title);

private:
    void onBtnClick(wxCommandEvent &);

    int counter = 0;
    wxTextCtrl *txtCtrl;

wxDECLARE_EVENT_TABLE();
};


#endif //COUNTER_MAINFRAME_H
