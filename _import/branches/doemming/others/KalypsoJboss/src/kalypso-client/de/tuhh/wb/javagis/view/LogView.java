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

import java.awt.GridBagLayout;
import java.awt.GridBagConstraints;
import java.awt.Point;
import java.text.DateFormat;
import java.text.SimpleDateFormat;

public class LogView extends JFrame implements ActionListener//, WindoFrameListener
{
    private static DateFormat dateFormat=new SimpleDateFormat("dd.MM.yyyy HH:mm");
    private JPanel panel=new JPanel(new GridBagLayout());
    
    JTextArea jTextArea    = new JTextArea();
    JScrollPane scroller=new JScrollPane();
    JButton jClear=new JButton("clear");
    static LogView instance=null;

    private LogView(String title)
    {	
	super(title);//,true,true,true,true);
	setLocation(new Point(600,400));
	initMask();
	setVisible(false);
	setSize(300,300);
    }
    
    public static LogView getInstance()
    {
	if(instance==null)
	    instance=new LogView("logView");
	return instance;
    }

    public synchronized static void print(String text)
    {
	LogView.getInstance().log(text);
    }

    public static void println(String text)  
    {	
	LogView.getInstance().logln(text);
    }

    public void log(String text)
    {
	if(text!=null)
	    {
		System.out.print(text);
		jTextArea.append(text);
		repaint();
	    }
    }

    public void logln(String text)
    {
	if(text!=null)
	    {
		System.out.println(text);
		if(jTextArea.getRows()>10)
		    jTextArea.setRows(10);
		jTextArea.append(text+"\n");
		repaint();
	    }
    }
    
    private void initMask()
    {
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
