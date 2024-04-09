package polyglot.a01b;

import javax.swing.*;
import java.util.*;
import java.awt.*;
import java.awt.event.ActionListener;
import javax.swing.event.MouseInputAdapter;
import javax.swing.event.MouseInputListener;
import java.awt.event.MouseEvent;
import polyglot.Pair;
public class GUI extends JFrame {

    private static final long serialVersionUID = -6218820567019985015L;
    private final Map<JButton,Pair<Integer,Integer>> buttons = new HashMap<>();
    private final Logics logics;

    public GUI(int size, int mines) {
        this.logics = new LogicsImpl(size,mines);
        this.setDefaultCloseOperation(EXIT_ON_CLOSE);
        this.setSize(100*size, 100*size);

        JPanel panel = new JPanel(new GridLayout(size,size));
        this.getContentPane().add(BorderLayout.CENTER,panel);

        ActionListener al = (e)->{
            final JButton bt = (JButton)e.getSource();
            final Pair<Integer,Integer> p = buttons.get(bt);
            //System.out.println("hit "+p);
            Optional<Integer> result = logics.hit(p.getX(), p.getY());
            if (result.isPresent() && !logics.won()) {
                bt.setText(String.valueOf(result.get()));
                bt.setEnabled(false);
            } else {
                JOptionPane.showMessageDialog(this, (logics.won() ? "You won!" : "You lost!"));
                System.exit(0);
            }
        };

        MouseInputListener onRightClick = new MouseInputAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                final JButton bt = (JButton)e.getSource();
                if (bt.isEnabled()){
                    if (bt.getText().equals("")) {
                        bt.setText("F");
                    } else {
                        bt.setText("");
                    }
                }
            }
        };

        for (int i=0; i<size; i++){
            for (int j=0; j<size; j++){
                final JButton jb = new JButton(" ");
                jb.addActionListener(al);
                jb.addMouseListener(onRightClick);
                this.buttons.put(jb,new Pair<>(j,i));
                panel.add(jb);
            }
        }
        this.setVisible(true);
    }

}

