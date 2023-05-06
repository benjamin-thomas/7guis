package org.example;

import javax.swing.*;

public class App {
    private JPanel mainPanel;
    private JButton btn;
    private JTextField counterField;

    private int counter = 0;

    public App() {
        btn.addActionListener(e -> {
            counter++;
            counterField.setText(String.valueOf(counter));
        });
    }

    public static void run() {
        try {
            String lookAndFeel = UIManager.getSystemLookAndFeelClassName();
            UIManager.setLookAndFeel(lookAndFeel);
        } catch (ClassNotFoundException | InstantiationException | IllegalAccessException |
                 UnsupportedLookAndFeelException e) {
            throw new RuntimeException(e);
        }

        JFrame frame = new JFrame("Counter Example");

        App app = new App();
        frame.setContentPane(app.mainPanel);
        frame.pack();

        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setVisible(true);
    }
}
