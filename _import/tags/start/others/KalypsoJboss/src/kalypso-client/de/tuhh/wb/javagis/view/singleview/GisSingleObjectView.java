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

import java.util.Vector;

import java.awt.Dimension;
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


public class GisSingleObjectView extends JInternalFrame implements InternalFrameListener
{
    private GisElement myGisElement;
    private JScrollPane scrollPane;
    private static GisSingleObjectView instance=null;

    public GisSingleObjectView(String frameName)
    {
	super(frameName,true,true,true,true);
	scrollPane=null;	
	ViewManager.desktop.add(this);
    }

    /*    private static GisSingleObjectViewogView getInstance()
	  {
	  if(instance==null)
	  instance=new LogView("logView");
	  return instance;
	  }
    */
    
    public static void load(String title,GisElement gisElement)
    {	
	if(instance!=null && instance.isClosed())
	    instance=null;
	if(instance==null)
	    instance=new GisSingleObjectView("Kalypso detailed View");

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
	GridBagLayout layout=new GridBagLayout();
	GridBagConstraints layoutConstraints = new GridBagConstraints();
	layoutConstraints.fill = GridBagConstraints.BOTH;

	layoutConstraints.gridwidth = 1;
	layoutConstraints.gridheight =1;
	layoutConstraints.weightx = 0.5;
 	layoutConstraints.weighty = 0.7;
	JPanel myPanel = new JPanel(layout);

	scrollPane=new JScrollPane(myPanel);
	getContentPane().add(scrollPane);

	layoutConstraints.gridwidth = GridBagConstraints.REMAINDER;
	layoutConstraints.anchor=GridBagConstraints.CENTER;
	add2View(myPanel,layout,layoutConstraints,new JLabel());
	
	layoutConstraints.gridwidth = GridBagConstraints.REMAINDER;
	layoutConstraints.anchor=GridBagConstraints.WEST;

	layoutConstraints.anchor=GridBagConstraints.CENTER;
	JLabel label;
	// simpleProperties:
	for(int row=0;row<myGisElement.getSimplePropertySize();row++)
	    {
		layoutConstraints.gridwidth = 1;
		
		layoutConstraints.anchor=GridBagConstraints.EAST;
		label=new JLabel(myGisElement.getSimplePropertyName(row)+": ",SwingConstants.RIGHT);
		label.setToolTipText(myGisElement.getSimplePropertyDescription(row));
		add2View(myPanel,layout,layoutConstraints,label);
		
		layoutConstraints.anchor=GridBagConstraints.WEST;
		layoutConstraints.gridwidth = GridBagConstraints.REMAINDER;
		ValueEditor valueEditor=new ValueEditor(myGisElement,row);
		add2View(myPanel,layout,layoutConstraints,valueEditor.getRepresentation());


	    }
	if(myGisElement instanceof GisObject)
	    {
 		GisObject myGisObject=(GisObject)myGisElement;
		if(myGisObject.getForwardRelationSize()+myGisObject.getBackwardRelationSize()>0)
		    {
			layoutConstraints.gridwidth = GridBagConstraints.REMAINDER;
			layoutConstraints.anchor=GridBagConstraints.CENTER;
			add2View(myPanel,layout,layoutConstraints,new JLabel("--Navigation--",SwingConstants.CENTER));
		    }

		// forwardRelations:
		for(int row=0;row<myGisObject.getForwardRelationSize();row++)
		    {
 			layoutConstraints.anchor=GridBagConstraints.EAST;
 			layoutConstraints.gridwidth = 1;
			//			add2View(myPanel,layout,layoutConstraints,new JLabel("forwardRelation",SwingConstants.RIGHT));
			GisElement linkToGisElement=(GisElement)myGisObject.getForwardRelatedGisObject(row);
 			layoutConstraints.anchor=GridBagConstraints.CENTER;
			layoutConstraints.gridwidth = GridBagConstraints.REMAINDER;
			add2View(myPanel,layout,layoutConstraints,new GisSingleObjectLink(this,myGisObject.getForwardRelationLabel(row),linkToGisElement));
			//			add2View(myPanel,layout,layoutConstraints,new JLabel("--> Relation"));
		    }
		// backwardRelations:
		for(int row=0;row<myGisObject.getBackwardRelationSize();row++)
		    {
 			layoutConstraints.anchor=GridBagConstraints.EAST;
 			layoutConstraints.gridwidth = 1;
			//			add2View(myPanel,layout,layoutConstraints,new JLabel("backwardRelation",SwingConstants.RIGHT));
			GisElement linkToGisElement=(GisElement)myGisObject.getBackwardRelatedGisObject(row);
 			layoutConstraints.anchor=GridBagConstraints.CENTER;
			layoutConstraints.gridwidth = GridBagConstraints.REMAINDER;

			add2View(myPanel,layout,layoutConstraints,new GisSingleObjectLink(this,myGisObject.getBackwardRelationLabel(row),linkToGisElement));
			//			add2View(myPanel,layout,layoutConstraints,new JLabel("--> Relation"));
		    }
	    }
	else if(myGisElement instanceof GisRelation)
	    {}	
	
	Dimension prefSize=getPreferredSize();
	if(prefSize.getHeight()>500)
	    setSize((int)getPreferredSize().getWidth(),500);
	else
	    setSize(getPreferredSize());
    }
    
    public void add2View(JPanel panel,GridBagLayout gridbag,GridBagConstraints c,JComponent  component) 
    {
	gridbag.setConstraints(component, c);
	panel.add(component);
    }



    /*
    //GisElementListener
    public void onGisElementChanged(GisElementEvent event)
    {
	if(event.isAffected(myGisElement.getId()))
	    {
		switch(event.getEventType())
		    {
		    case GisElementEvent.Removed:
			try
			    {
				setClosed(true);
			    }
			catch(java.beans.PropertyVetoException e)
			    {
				System.out.println(e);
			    }
			break;
		    case GisElementEvent.SimplePropertyChanged:
			rebuildView();
			break;
		    default:
			break;
		    }
	    }
    }
    */

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

