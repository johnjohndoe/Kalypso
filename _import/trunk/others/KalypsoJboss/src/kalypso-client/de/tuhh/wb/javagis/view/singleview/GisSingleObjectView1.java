
package de.tuhh.wb.javagis.view.singleview;

import javax.swing.event.InternalFrameListener;
import javax.swing.event.InternalFrameEvent;
import javax.swing.JInternalFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.SwingConstants;
import javax.swing.JButton;
import javax.swing.JTable;

import java.util.Vector;

import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.GridBagLayout;
import java.awt.GridBagConstraints;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import de.tuhh.wb.javagis.Main;
import de.tuhh.wb.javagis.view.ViewManager;
import de.tuhh.wb.javagis.view.ValueEditor;
import de.tuhh.wb.javagis.data.GisElement;
import de.tuhh.wb.javagis.data.GisObject;
import de.tuhh.wb.javagis.data.GisRelation;

public class GisSingleObjectView1 extends JInternalFrame implements InternalFrameListener
{
 	private GisElement myGisElement;
    private JScrollPane scrollPane;
    private static GisSingleObjectView1 instance=null;

    public GisSingleObjectView1(String frameName)
    {
	super(frameName,true,true,true,true);
	scrollPane=null;
	ViewManager.desktop.add(this);
    }
	
	public static void load(String title,GisElement gisElement)
    {
	if(instance!=null && instance.isClosed())
	    instance=null;
	if(instance==null)
	    instance=new GisSingleObjectView1("Kalypso detailed View");

	/*
	  if(myGisElement!=null)
	  myGisElement.getGisElementClass().unRegister(this);
	*/
	instance.setTitle(title);
	instance.myGisElement=gisElement;
	//	myGisElement.getGisElementClass().register(this);
	instance.rebuildView();
	instance.setVisible(true);
	instance.moveToFront();
	instance.show();
    }
	
	private void rebuildView()
    {
	getContentPane().removeAll();
	setTitle(myGisElement.getLabel());//Name()+" #"+myGisElement.getId().toString());
	GridLayout layout=new GridLayout();
		
	JPanel myPanel = new JPanel(layout);
		
	GSOViewTableModel tableModel = new GSOViewTableModel(myGisElement);
	JTable singleTable = new JTable(tableModel);
	myPanel.add(singleTable);
	singleTable.getColumn("SimplePropertyValue").setCellRenderer(new MultiRenderer());
    singleTable.getColumn("SimplePropertyValue").setCellEditor(new MultiEditor());
		
	//scrollPane=new JScrollPane(myPanel);
		
	getContentPane().add(myPanel);
	//myPanel.add(new JLabel("Hallo"));
		
	Dimension prefSize=getPreferredSize();
	if(prefSize.getHeight()>500)
	    setSize((int)getPreferredSize().getWidth(),500);
	else
	    setSize(getPreferredSize());
	 }
	
	// internalFrameListener:

    //          Invoked when an internal frame is activated.
    public void internalFrameActivated(InternalFrameEvent e)
    {}

    //          Invoked when an internal frame has been closed.
    public void internalFrameClosed(InternalFrameEvent e)
    {}

    //          Invoked when an internal frame is in the process of being closed.
    public void internalFrameClosing(InternalFrameEvent e)
    {
	/*
	  if(myGisElement!=null)
	  myGisElement.getGisElementClass().unRegister(this);
	*/
    }

    //          Invoked when an internal frame is de-activated.
    public void internalFrameDeactivated(InternalFrameEvent e)
    {
	
    }

    //          Invoked when an internal frame is de-iconified.
    public void internalFrameDeiconified(InternalFrameEvent e)
    {}
    
    //          Invoked when an internal frame is iconified.
    public void internalFrameIconified(InternalFrameEvent e)
    {}
    
    public void internalFrameOpened(InternalFrameEvent e)
    {}

}

