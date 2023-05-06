package org.example;

import javax.swing.*;
import java.awt.*;
import java.util.concurrent.atomic.AtomicInteger;

public class App {
    private final JPanel mainPanel;

    public App() {
        AtomicInteger counter = new AtomicInteger();
        mainPanel = new JPanel();
        mainPanel.setLayout(new GridBagLayout());
        mainPanel.setPreferredSize(new Dimension(400, 300));

        JPanel line = new JPanel();
        line.setLayout(new BoxLayout(line, BoxLayout.X_AXIS));

        var counterField = new JTextField();
        int commonHeight = counterField.getPreferredSize().height;
        counterField.setText(String.valueOf(counter.get()));
        counterField.setEnabled(false);
        counterField.setPreferredSize(new Dimension(150, commonHeight));
        counterField.setMaximumSize(new Dimension(Integer.MAX_VALUE, commonHeight)); // Set maximum height

        line.add(counterField);
        line.add(Box.createRigidArea(new Dimension(5, 0)));

        var btn = new JButton("Count");
        btn.setMnemonic('c');
        btn.setFocusPainted(false);
        btn.addActionListener(e -> {
            int count = counter.incrementAndGet();
            counterField.setText(String.valueOf(count));
        });

        // Work around weird height issue related to the native look and field (btn height is off by 1-2 px)
        btn.setMaximumSize(
                new Dimension(
                        btn.getMaximumSize().width,
                        commonHeight + 2
                )
        );
        line.add(btn);

        mainPanel.add(line);
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
