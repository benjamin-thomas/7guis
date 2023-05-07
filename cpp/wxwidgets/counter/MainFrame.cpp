#include "MainFrame.h"

// Custom IDs must be > 1 and < 4999 (wxID_LOWEST)
enum IDs {
    ID_BTN = 2
};

wxBEGIN_EVENT_TABLE(MainFrame, wxFrame)
                EVT_BUTTON(ID_BTN, MainFrame::onBtnClick)
wxEND_EVENT_TABLE()

MainFrame::MainFrame(const wxString &title) : wxFrame(nullptr, wxID_ANY, title) {
    auto *pPanel = new wxPanel(this);

    const wxString &initVal = std::to_string(counter);
    txtCtrl = new wxTextCtrl(pPanel, wxID_ANY, initVal);
    txtCtrl->Enable(false);

    auto pButton = new wxButton(pPanel, ID_BTN, "Counter");

    auto *vSizer = new wxBoxSizer(wxVERTICAL);
    auto *hSizer = new wxBoxSizer(wxHORIZONTAL);

    hSizer->Add(txtCtrl, 2, wxALIGN_CENTER);
    hSizer->AddSpacer(5);
    hSizer->Add(pButton, 1, wxALIGN_CENTER);

    vSizer->AddStretchSpacer();
    vSizer->Add(hSizer, 0, wxALIGN_CENTER);
    vSizer->AddStretchSpacer();

    pPanel->SetSizer(vSizer);
}

void MainFrame::onBtnClick(wxCommandEvent &) {
    counter++;
    txtCtrl->SetValue(std::to_string(counter));
}
