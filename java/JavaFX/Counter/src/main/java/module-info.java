module com.example.counter {
    requires javafx.controls;
    requires javafx.fxml;


    opens com.example.counter to javafx.fxml;
    exports com.example.counter;
}