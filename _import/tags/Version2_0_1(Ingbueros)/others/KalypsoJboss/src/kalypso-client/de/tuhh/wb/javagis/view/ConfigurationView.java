package de.tuhh.wb.javagis.view;

import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.File;
import java.io.FileOutputStream;
import java.util.Properties;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;

import de.tuhh.wb.javagis.Main;
import de.tuhh.wb.javagis.tools.I18n;


public class ConfigurationView extends JFrame implements ActionListener,WindowListener
{
	static ConfigurationView instance=null;
	Properties props = Main.props;
	private boolean restart;
	
	private JPanel panel=new JPanel(new GridBagLayout());
	
	//JTextField tf_bce_driver = new JTextField("ca.edbc.jdbc.EdbcDriver");
	JTextField tf_bce_url= new JTextField(props.getProperty("bce_url"));//("jdbc:edbc://134.28.87.20:WB7/elbe::abwb/INGRES");
	JTextField tf_bce_user = new JTextField(props.getProperty("bce_user"));//("ingres");
	JTextField tf_bce_pass = new JTextField(props.getProperty("bce_pass"));//("ingres42");
	JTextField tf_jBoss_host = new JTextField(props.getProperty("jboss_host"));//("elbe.wb.tu-harburg.de");
	//JTextField tf_template_simulation = new JTextField("/tmp/kalypso_template");
	JButton button_template_simulation = new JButton(props.getProperty("template_simulation"));//("/tmp/kalypso_template");
	
	//JLabel lab_bce_driver = new JLabel(I18n.get("CV_Label_bce_driver"));
	JLabel lab_bce_url= new JLabel(I18n.get("CV_Label_bce_url"));
	JLabel lab_bce_user = new JLabel(I18n.get("CV_Label_bce_user"));
	JLabel lab_bce_pass = new JLabel(I18n.get("CV_Label_bce_pass"));
	JLabel lab_jBoss_host = new JLabel(I18n.get("CV_Label_jBoss_host"));
	JLabel lab_template_simulation = new JLabel(I18n.get("CV_Label_template_simulation"));
	
	JButton testButton1 = new JButton(I18n.get("CV_Button_Test1"));
	JButton testButton2 = new JButton(I18n.get("CV_Button_Test2"));
	JButton startButton = new JButton(I18n.get("CV_Button_Start"));
	
	JFileChooser fileChoo = new JFileChooser();
	
    public ConfigurationView(String title,boolean restart)
    {
		super(title);//,true,true,true,true);
		setLocation(new Point(200,200));
		initMask();
		setVisible(false);
		setSize(500,300);
		addWindowListener(this);
		this.restart = restart;
		
    }
    
    /**public static ConfigurationView getInstance()
	 {
	 if(instance==null)
	 instance=new ConfigurationView(I18n.get("CV_Title"));
	 return instance;
	 }*/
	
	private void initMask(){
		//Labels
		//add2View(lab_bce_driver,0,0,1,1);
		add2View(lab_bce_url,	0,0,1,1);
		add2View(lab_bce_user,	0,1,1,1);
		add2View(lab_bce_pass,	0,2,1,1);
		add2View(lab_jBoss_host,0,4,1,1);
		add2View(lab_template_simulation,0,6,1,1);
		
		//TextFields
		//add2View(tf_bce_driver,1,0,1,1);
		add2View(tf_bce_url,	1,0,1,1);
		add2View(tf_bce_user,	1,1,1,1);
		add2View(tf_bce_pass,	1,2,1,1);
		add2View(tf_jBoss_host,	1,4,1,1);
		//add2View(tf_template_simulation,1,6,1,1);
		add2View(button_template_simulation,1,6,1,1);
		
		//Buttons
		addButton2View(testButton1,0,3,1,1,GridBagConstraints.NORTHWEST);
		addButton2View(testButton2,0,5,1,1,GridBagConstraints.NORTHWEST);
		addButton2View(startButton,1,8,1,1,GridBagConstraints.SOUTHEAST);
		
		testButton1.addActionListener(this);
		testButton1.setActionCommand("test_bce");
		testButton2.addActionListener(this);
		testButton2.setActionCommand("test_host");
		startButton.addActionListener(this);
		startButton.setActionCommand("start");
		button_template_simulation.addActionListener(this);
		button_template_simulation.setActionCommand("tempsim");
		button_template_simulation.setBackground(Color.white);
		
		getContentPane().add(panel);
		//panel.setBackground(Color.blue);
	}
	
	private void add2View (JComponent component, int x, int y, int width, int height){
		GridBagLayout layout=(GridBagLayout)panel.getLayout();
		GridBagConstraints gbc = new GridBagConstraints();
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.gridx = x;
		gbc.gridy = y;
		gbc.gridwidth = width;
		gbc.gridheight = height;
		gbc.insets = new Insets(1,1,1,1);
		gbc.weightx =0.5;
		gbc.weighty = 0;
		layout.setConstraints(component, gbc);
		panel.add(component);
		
	}
	
	private void addButton2View (JComponent component, int x, int y, int width, int height, int anchor){
		GridBagLayout layout=(GridBagLayout)panel.getLayout();
		GridBagConstraints gbc = new GridBagConstraints();
		gbc.fill = GridBagConstraints.NONE;
		gbc.anchor = anchor;
		gbc.gridx = x;
		gbc.gridy = y;
		gbc.gridwidth = width;
		gbc.gridheight = height;
		gbc.insets = new Insets(1,1,1,1);
		gbc.weightx =0.5;
		gbc.weighty = 0;
		layout.setConstraints(component, gbc);
		panel.add(component);
		
	}
	
	// ActionListener
    public void actionPerformed(ActionEvent e)
    {
		String command=e.getActionCommand();
		//props=Main.props;
	    
		if("test_bce".equals(command))
	    {
			props.setProperty("bce_driver","ca.edbc.jdbc.EdbcDriver");
			props.setProperty("bce_url",tf_bce_url.getText());
			props.setProperty("bce_user",tf_bce_user.getText());
			props.setProperty("bce_pass",tf_bce_pass.getText());
			System.out.println(props.toString());
			try{
				Main.connectBCE();
				JOptionPane.showMessageDialog(this,
											  I18n.get("CV_bceConfirmMessage"),
											  I18n.get("CV_bceConfirmMessageTitle"),
											  JOptionPane.INFORMATION_MESSAGE);
				//System.out.println("connect successfull");
			}
			catch(Exception ex){
				JOptionPane.showMessageDialog(this,
											  I18n.get("CV_bceErrorMessage")+ex.getMessage(),
											  I18n.get("CV_bceErrorMessageTitle"),
											  JOptionPane.ERROR_MESSAGE);
				//System.out.println("Bce-Einstellungen fehlerhaft");
				//System.out.println(ex.getMessage());
			}
	    }
		
		if("test_host".equals(command))
	    {
			System.out.println("Dies ist ein host_Test");
	    }
		
		if("tempsim".equals(command))
		{
			fileChoo.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
			//fileChoo.setSelectedFile(new File(button_template_simulation.getText()));
			int returnVal = fileChoo.showOpenDialog(this);
			if (returnVal == JFileChooser.APPROVE_OPTION) {
				File file = fileChoo.getSelectedFile();
				button_template_simulation.setText(file.toString());
			} else {
				System.out.println("nothing");
			}
			
		}
		
		if("start".equals(command))
		{
			File configFile=new File("kalypsoMain.conf");
			//props=Main.props;
			props.setProperty("bce_driver","ca.edbc.jdbc.EdbcDriver");
			props.setProperty("bce_url",tf_bce_url.getText());
			props.setProperty("bce_user",tf_bce_user.getText());
			props.setProperty("bce_pass",tf_bce_pass.getText());
			props.setProperty("jboss_host",tf_jBoss_host.getText());
			props.setProperty("jboss_port","");
			props.setProperty("jboss_user","kalypso");
			props.setProperty("jboss_pass","kalypso");
			props.setProperty("template_simulation",button_template_simulation.getText());
			try{
				props.store(new FileOutputStream(configFile),"kalypso_config");
			}
			catch (Exception ex){
				System.out.println("Could not create Config-File.");
			}
			
			
			
			if(restart==false){
				System.out.println("Starte Programm...");
				Main.confView.hide();
				Main main=new Main();
			}
			else{
				System.out.println("Restart from Programm");
				ViewManager.confView.hide();
				JOptionPane.showMessageDialog(this,
											  I18n.get("CV_NeustartConfirmMessage"),
											  I18n.get("CV_NeustartConfirmMessageTitle"),
											  JOptionPane.INFORMATION_MESSAGE);
				Main.viewManager.dispose();
				System.exit(0);
				//Main main=new Main();
				
			}
		}
    }
	// WindowListener
    public void windowActivated(WindowEvent e)
    {}//          Invoked when the Window is set to be the active Window.
	
    public void windowClosed(WindowEvent e)
    {
		System.out.println("bye");
    }//       Invoked when a window has been closed as the result of calling dispose on the window.
	
	
    public   void windowClosing(WindowEvent e)
    {
		if(restart ==false){
			System.out.println("bye");
			System.exit(0);
		}
		else{
			this.dispose();
		}
    }//  Invoked when the user attempts to close the window from the window's system menu.
	
    public   void windowDeactivated(WindowEvent e)
    {}//          Invoked when a Window is no longer the active Window.
	
    public   void windowDeiconified(WindowEvent e)
    {}//          Invoked when a window is changed from a minimized to a normal state.
	
    public   void windowIconified(WindowEvent e)
    {}//          Invoked when a window is changed from a normal to a minimized state.
	
    public   void windowOpened(WindowEvent e)
    {}
	
}

