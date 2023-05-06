package com.example.counter;

import javafx.fxml.FXML;
import javafx.scene.control.TextField;

public class HelloController {
    private int counter = 0;

    @FXML
    private TextField counterText;

    public void initialize() {
        setCounterText();
    }

    @FXML
    protected void onClick() {
        counter++;
        setCounterText();
    }

    private void setCounterText() {
        counterText.setText(String.valueOf(counter));
    }
}