#include "App.h"
#include "MainFrame.h"

bool App::OnInit() {
    auto *pMainFrame = new MainFrame("Counter Example");

    pMainFrame->SetClientSize(400, 300);
    pMainFrame->Center();
    pMainFrame->Show();

    return true;
}

wxIMPLEMENT_APP(App);