package de.tuhh.wb.javagis.view;

import java.util.Vector;
import java.util.Date;
import java.awt.GridBagLayout;
import java.awt.GridBagConstraints;
import javax.swing.JInternalFrame;
import javax.swing.*;
import javax.swing.JLabel;
import javax.swing.tree.*;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import java.awt.event.MouseListener;
import java.awt.event.MouseEvent;

import javax.swing.event.InternalFrameListener;
import javax.swing.event.InternalFrameEvent;

import de.tuhh.wb.javagis.data.VersionAccess;
import de.tuhh.wb.javagis.data.VersionAccessImpl;
import de.tuhh.wb.javagis.view.ViewManager;
import de.tuhh.wb.javagis.data.event.VersionListener;
import de.tuhh.wb.javagis.data.event.KalypsoEventManager;
import ejb.event.EJBEvent;
import java.awt.GridBagLayout;
import java.awt.GridBagConstraints;
import java.awt.Point;
import java.text.DateFormat;
import java.text.SimpleDateFormat;

public class SplashWindow extends JFrame implements ActionListener//, WindoFrameListener
{
    private static final Component dummyComponent=new Component(){};

    private SpashWindow(String title)
    {	
	super(title);//,true,true,true,true);
	//	setLocation(new Point(600,400));
	initMask();
	//	ViewManager.desktop.add(this);
	setVisible(true);
	show();
	setSize(300,300);
    }
    
    private void initMask()
    {
	Image image = Toolkit.getDefaultToolkit().getImage("image/spah.jpg");
	MediaTracker mt = new MediaTracker(dummyComponent);
	mt.addImage(symbol,0);
	try 
	    {
		mt.waitForAll();
	    }
	catch (InterruptedException e)
	    {
		//nothing
	    }		
	
	add2ViewLastInRow(new JLabel("logging..."));

	add2ViewLastInRow(new JLabel("logging..."));
	jTextArea.setText("");
	scroller.setViewportView(jTextArea);


	GridBagLayout layout=(GridBagLayout)panel.getLayout();
	GridBagConstraints layoutConstraints = new GridBagConstraints();
	layoutConstraints.fill = GridBagConstraints.BOTH;
	layoutConstraints.gridwidth = GridBagConstraints.REMAINDER;
	layoutConstraints.gridheight =1;
	layoutConstraints.weightx = 0;
 	layoutConstraints.weighty = 1;
	layout.setConstraints(scroller, layoutConstraints);
	panel.add(scroller);



	add2ViewLastInRow(jClear);	

	jClear.addActionListener(this);
	jClear.setActionCommand("clear");
	getContentPane().add(panel);
    }
    
    // ActionListener
    public void actionPerformed(ActionEvent e)
    {
	String command=e.getActionCommand();
	    
	if("clear".equals(command))
	    {
		jTextArea.setText("");
	    }
    }
    
    public void add2View(JComponent  component) 
    {
	GridBagLayout layout=(GridBagLayout)panel.getLayout();
	GridBagConstraints layoutConstraints = new GridBagConstraints();
	layoutConstraints.fill = GridBagConstraints.HORIZONTAL;
	layoutConstraints.gridwidth = 1;
	layoutConstraints.gridheight =1;
	layoutConstraints.weightx = 0.5;
 	layoutConstraints.weighty = 0;
	layout.setConstraints(component, layoutConstraints);
	panel.add(component);
    }

    public void add2ViewLastInRow(JComponent  component) 
    {
	GridBagLayout layout=(GridBagLayout)panel.getLayout();
	GridBagConstraints layoutConstraints = new GridBagConstraints();
	layoutConstraints.fill = GridBagConstraints.HORIZONTAL;
	layoutConstraints.gridwidth = GridBagConstraints.REMAINDER;
	layoutConstraints.gridheight =1;
	layoutConstraints.weightx = 0.5;
 	layoutConstraints.weighty = 0;
	layout.setConstraints(component, layoutConstraints);
	panel.add(component);
    }

    public static String format(Date date)
    {
	if(date!=null)
	    return dateFormat.format(date);
	else
	    return "null";
	    
    }
}
