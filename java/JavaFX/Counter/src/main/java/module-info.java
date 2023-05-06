module com.example.javafxcounter {
    requires javafx.controls;
    requires javafx.fxml;


    opens com.example.counter to javafx.fxml;
    exports com.example.counter;
}