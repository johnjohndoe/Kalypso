package de.tuhh.wb.javagis.view;

import javax.swing.JInternalFrame;
import javax.swing.JPanel;
//import javax.swing.JLabel;
import javax.swing.JProgressBar;
import java.awt.Color;
//import java.awt.BorderLayout;
//import java.awt.GridLayout;
import de.tuhh.wb.javagis.tools.I18n;
//import java.awt.event.WindowAdapter;
//import java.awt.event.WindowEvent;
import java.awt.Point;

public class WaitingDialog extends JInternalFrame {
	
	static WaitingDialog instance=null;

	public WaitingDialog() {
		super(I18n.get("WaitingDialog_Title"));
		setLocation(new Point(150,200));
		makeDialog();
		setVisible(true);
		setSize(200,60);
		show();
		/*addWindowListener(
			new WindowAdapter() {
				public void windowClosing(WindowEvent e) {
					Object frame_Object = e.getSource();
					((JFrame)frame_Object).dispose();
				}
			});*/
		
	}
	
	public static WaitingDialog getInstance()
		 {
		 if(instance==null)
		 instance=new WaitingDialog();
		 ViewManager.desktop.add(instance);
		 instance.moveToFront();
		 return instance;
		 }
		 
	public static void waitingDialogDispose(){
		if(instance!=null){
			instance.dispose();
			instance = null;
		}
	}

	private void makeDialog() {
		JPanel panel = new JPanel();
		panel.setBackground(Color.white);
		//panel.setLayout(new BorderLayout());
		JProgressBar progressBar = new JProgressBar();
		progressBar.setIndeterminate(true);
		//JLabel label = new JLabel(I18n.get("WaitingDialog_Label"));
		//label.setSize(10,10);
		//label.setForeground(Color.red);
		panel.add(progressBar);
		getContentPane().add(panel);
	}

}
