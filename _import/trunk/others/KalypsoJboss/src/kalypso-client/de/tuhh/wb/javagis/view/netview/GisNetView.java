package de.tuhh.wb.javagis.view.netview;



import javax.swing.table.AbstractTableModel;
import javax.swing.*;
import de.tuhh.wb.javagis.tools.I18n;



//import de.tuhh.wb.javagis.model.ElementSession;

import de.tuhh.wb.javagis.view.ViewManager;

import de.tuhh.wb.javagis.view.singleview.GisSingleObjectView;
import de.tuhh.wb.javagis.view.projectview.ProjectView;



import java.util.Hashtable;

import java.util.Vector;

import java.util.Enumeration;



import java.awt.geom.Point2D;

import java.awt.Image;

import java.awt.Graphics;

import java.awt.event.ComponentEvent;

import java.awt.event.ComponentListener;

import javax.swing.JFrame;

import javax.swing.JScrollPane;

import javax.swing.JPanel;

import javax.swing.JCheckBoxMenuItem;



import javax.swing.JInternalFrame;
import javax.swing.event.InternalFrameListener;
import javax.swing.event.InternalFrameEvent;

import java.awt.event.MouseListener;

import java.awt.event.MouseEvent;

import java.awt.event.MouseMotionListener;

import java.awt.event.ActionListener;

import java.awt.event.ActionEvent;

import java.awt.Color;

import java.awt.Font;

import java.awt.Graphics2D;

import java.awt.BasicStroke;

import java.awt.GridLayout;



import de.tuhh.wb.javagis.data.*;

import javax.ejb.ObjectNotFoundException;

import java.awt.HeadlessException;



public class GisNetView extends JInternalFrame implements ComponentListener, MouseListener,MouseMotionListener,ActionListener,InternalFrameListener
	
{
	
    private static final int MOVE_MODE=0;
	
    private static final int CREATE_MODE=1;
	
    private static final int RELATION_MODE=2;
	
    private static final int REMOVE_OBJECT=3;
	
    private static final int REMOVE_RELATION=4;
	
    private static final int ZOOMIN_MODE=5;
	
    private static final int ZOOMOUT_MODE=6;
	
    //private static final int PAN_MODE=7;
	
	private static final int PAN2_MODE=7;
    
	
    private static final int DEFAULT_MODE=PAN2_MODE;
	
	
	
    private int mode;
	
    private GisNetModel netModel;
	
    private Vector gisObjectClasses;
	
    private Vector gisRelationClasses;
	
	
	
    private GisObject selectedGisObject=null;
	
    private GisRelation selectedGisRelation=null;
	
	
	
    private GisObject movingGisObject;
	
    private GisPoint movingGisPoint;
	
    private GisPoint createGisPoint;
	
    
	
    private GisObject startRelationGisObject=null;
	
    private GisObject   endRelationGisObject=null;
	
	
	
    private GisPoint  startRelationGisPoint=null;
	
    private GisPoint    endRelationGisPoint=null;
	
	
	
    private GisPoint startZoomGisPoint=null;
	
    private GisPoint endZoomGisPoint=null;
	
    private GisBox   zoomGisBox=null;
	
	
	private GisPoint pan2Point1=null;
	
	private GisPoint pan2Point2=null;
	
	
    private JTextField tfAction;
	
	
	
    //    public static GisMap gisMap;
	
    public GisMap gisMap;
	
	
	
    public GisNetView(String title, GisNetModel netModel)
		
    {
		
		super(title,true,true,true,true);
		
		this.addInternalFrameListener(this);
		
		this.movingGisObject=null;
		
		this.movingGisPoint=null;
		
		this.selectedGisObject=null;
		
		this.selectedGisRelation=null;
		
		this.createGisPoint=null;
		
		this.mode=DEFAULT_MODE;
		
		this.netModel=netModel;
		
		this.gisObjectClasses   = netModel.myGisObjectClasses;
		
		this.gisRelationClasses = netModel.myGisRelationClasses;
		
		this.gisMap=new GisMap(netModel,this);
		
		createFileMenu();
		
		getContentPane().add(gisMap);
		
		//this.gisMap.repaint();
		
		setVisible(true);
		
		gisMap.noZoom();
		
		this.addComponentListener(this);
		
		gisMap.addMouseMotionListener(this);
		
		gisMap.addMouseListener(this);
		
		gisMap.repaint();
		
		pack();
		
		netModel.setGisMap(gisMap);
		
		
    }
	
    
	
    private String getModeLabel(int mode)
		
    {
		
		switch(mode)
			
	    {
			
			case MOVE_MODE:
				
				//return "move";
				return I18n.get("netViewLabel_Move");
				
			case CREATE_MODE:
				
				//return "create Object";
				return I18n.get("netViewLabel_createObject");
				
			case RELATION_MODE:
				
				//return "create Relation";
				return I18n.get("netViewLabel_createRelation");
				
			case REMOVE_OBJECT:
				
				//return "remove Object";
				return I18n.get("netViewLabel_removeObject");
				
			case REMOVE_RELATION:
				
				//return "remove Relation";
				return I18n.get("netViewLabel_removeRelation");
				
			case ZOOMIN_MODE:
				
				//return "zoom In";
				return I18n.get("netViewLabel_zoomIn");
				
			case ZOOMOUT_MODE:
				
				//return "zoom Out";
				return I18n.get("netViewLabel_zoomOut");
				
				/*case PAN_MODE:
				 
				 //return "pan";
				 return I18n.get("netViewLabel_pan");
				 */
			case PAN2_MODE:
				
				//return "pan";
				return I18n.get("netViewLabel_pan");
				
			default:
				
				//return "unknown";
				return I18n.get("netViewLabel_unknown");
	    }
		
    }
	
	
	
	
	
    private void setMode(int mode)
		
    {
		
		this.mode=mode;
		
		tfAction.setText(getModeLabel(mode));
		
    }
	
	
	
	
	
    
	
    //ComponentListener:
	
    //          Invoked when the component has been made invisible.
	
    public void componentHidden(ComponentEvent e)
		
    {}
	
    
	
    //    Invoked when the component's position changes.
	
    public void componentMoved(ComponentEvent e)
		
    {}
	
    
	
    //   Invoked when the component's size changes.
	
    public void componentResized(ComponentEvent e)
		
    {
		
		System.out.println("component resized...");
		
		if(getJMenuBar()!=null)
			
	    {
			
			this.setSize(getSize().width,(int)(getSize().width/gisMap.ratioWtoH+getJMenuBar().getHeight()));
			
			gisMap.setScreenBox(new ScreenBox(0,0,this.getSize().width,this.getSize().height-getJMenuBar().getHeight()));
			
	    }
			
		else
			
	    {
			
			this.setSize(getSize().width,(int)(getSize().width/gisMap.ratioWtoH));
			
			gisMap.setScreenBox(new ScreenBox(0,0,this.getSize().width,this.getSize().height));
			
	    }
		
		gisMap.updateImage();
		
    }
	
    
	
    public void componentShown(ComponentEvent e)
		
    {
		
		System.out.println("component shown...");
		
		this.setSize(getSize().width,(int)(getSize().width/gisMap.ratioWtoH));
		
		gisMap.setScreenBox(new ScreenBox(0,0,getSize().width,getSize().height));
		
    }
	
    
	
    // MouseListener:
	
    public void mouseClicked(MouseEvent e)
		
    {
		
		GisPoint gisPoint = gisMap.trafo.convert(e);
		
		switch(mode)
			
	    {
			
			case ZOOMOUT_MODE:
				
				gisMap.zoomOut();
				
				clearModes();
				
				break;
				
				/*case PAN_MODE:
				 
				 if(e.getModifiers()!=MouseEvent.BUTTON3_MASK)
				 
				 {
				 
				 gisMap.panTo(gisPoint);
				 
				 clearModes();
				 
				 }
				 
				 break;*/
				
	    }
		
    }
	
    
	
    public void mouseEntered(MouseEvent e)
		
    {
		
    }
	
    
	
    public void mouseExited(MouseEvent e)
		
    {
		
    }
	
    
	
    public void mousePressed(MouseEvent e)
		
    {
		
		GisPoint gisPoint = gisMap.trafo.convert(e);
		
		switch(e.getModifiers())
			
	    {
			
			case MouseEvent.BUTTON1_MASK: // left
				
				switch(mode)
					
				{
					
					case MOVE_MODE:
						
						//select Object
						
						movingGisObject=netModel.snap(gisPoint);
						
						movingGisPoint=gisPoint;
						
						break;
						
					case PAN2_MODE:
						
						pan2Point1 = gisPoint;
						
						break;
						
						
					case RELATION_MODE:
						
						try
							
						{
							
							if((startRelationGisObject=netModel.snap(gisPoint))!=null)
								
								startRelationGisPoint=startRelationGisObject.getBasePoint();
							
							System.out.println("snaped for Relation..."+startRelationGisObject);
							
						}
						
						catch(ObjectNotFoundException ex)
							
						{
							
							startRelationGisObject=null;
							
						}
						
						break;
						
					case REMOVE_OBJECT:
						
						selectedGisObject = netModel.snap(gisPoint);
						
						break;
						
					case REMOVE_RELATION:
						
						selectedGisRelation = netModel.snapRelation(gisPoint);
						
						break;
						
					case ZOOMIN_MODE:
						
						startZoomGisPoint = gisPoint;
						
						endZoomGisPoint = gisPoint;
						
						zoomGisBox=gisMap.getZoomGisBox(startZoomGisPoint,endZoomGisPoint);
						
						break;
						
					default:
						
						break;
						
				}
				
				break;
				
			case MouseEvent.BUTTON3_MASK:  // right
				
				break;
				
			default:
				
				break;
				
	    }
		
		if(e.isPopupTrigger())
			
			showPopupMenu(e);
		
		repaint();
		
    }
	
    
	
    public void mouseReleased(MouseEvent e)
		
    {
		
		GisPoint gisPoint = gisMap.trafo.convert(e);
		
		switch(mode)
			
	    {
			
			case MOVE_MODE:
				
				//select Object
				
				if(movingGisObject!=null)
					
				{
					
					movingGisObject.setBasePoint(gisPoint);
					
					gisMap.updateImage();
					
					clearModes();
					
				}
				
				break;
				
			case PAN2_MODE:
				
				pan2Point2 = gisPoint;
				
				//lastscale = scale;
				
				gisMap.panTo2(pan2Point1,pan2Point2);//,scale);
				
				clearModes();
				
				break;
				
			case RELATION_MODE:
				
				if(startRelationGisObject!=null)
					
				{
					
					endRelationGisObject=netModel.snap(gisPoint);
					
					if(endRelationGisObject!=null && !startRelationGisObject.equals(endRelationGisObject))
						
					{
						
						System.out.println("snaped for Relation...(END)"+endRelationGisObject);
						
						netModel.createRelation(startRelationGisObject,endRelationGisObject);
						
						gisMap.updateImage();
						
						clearModes();
						
					}
					
				}
				
				break;
				
			case CREATE_MODE:
				
				createGisPoint = gisPoint;
				
				maybeShowPopupCreate(e);
				
				clearModes();
				
				break;
				
			case REMOVE_OBJECT:
				
				if(selectedGisObject!=null)
					
				{
					
					try
						
					{
						
						switch(JOptionPane.showConfirmDialog(this,//Component parentComponent,
															 
															 I18n.get("netView_removeObjectDialog_message")+selectedGisObject.getGisElementClass()+selectedGisObject.getId(),//Object message,
															 
															 I18n.get("netView_removeObjectDialog_message"),//String title,
															 
															 JOptionPane.YES_NO_CANCEL_OPTION,//int optionType,
															 
															 JOptionPane.QUESTION_MESSAGE))//int messageType)
							
							//Icon icon)
							
						{
							
							case JOptionPane.YES_OPTION:
								
								((GisObjectClass)selectedGisObject.getGisElementClass()).remove(selectedGisObject.getId());
								
								break;
								
							case JOptionPane.CANCEL_OPTION:
								
								break;
								
						}
						
					}
					
					catch(HeadlessException ex)
						
					{}
					
					clearModes();
					
				}
				
				break;
				
			case REMOVE_RELATION:
				
				if(selectedGisRelation!=null)
					
				{
					
					try
						
					{
						
						switch(JOptionPane.showConfirmDialog(this,//Component parentComponent,
															 
															 I18n.get("netView_removeRelationDialog_message")+selectedGisRelation.getName()+"#"+selectedGisRelation.getId(),//Object message,
															 
															 I18n.get("netView_removeRelationDialog_title"),//String title,
															 
															 JOptionPane.YES_NO_CANCEL_OPTION,//int optionType,
															 
															 JOptionPane.QUESTION_MESSAGE))//int messageType)
							
							//Icon icon)
							
						{
							
							case JOptionPane.YES_OPTION:
								
								((GisRelationClass)selectedGisRelation.getGisElementClass()).remove(selectedGisRelation.getId());
								
								break;
								
							case JOptionPane.CANCEL_OPTION:
								
								break;
								
						}
						
					}
					
					catch(HeadlessException ex)
						
					{}
					
					clearModes();
					
				}
				
				break;
				
		    case ZOOMIN_MODE:
				
				if(zoomGisBox!=null)
					
				{
					
					gisMap.zoomTo(zoomGisBox);
					
					zoomGisBox=null;
					
					clearModes();
					
					/*
					 
					 //endZoomGisPoint=gisPoint;
					 
					 //double zoomInWidth = endZoomGisPoint.getX()-startZoomGisPoint.getX();
					 
					 //double zoomInHeight = zoomInWidth/gisMap.ratioWtoH;
					 
					 //endZoomGisPoint.setLocation(startZoomGisPoint.getX()+zoomInWidth,startZoomGisPoint.getY()+zoomInHeight);
					 
					 
					 
					 if(endZoomGisPoint!=null && startZoomGisPoint!=endZoomGisPoint)
					 
					 {
					 
					 GisBox zoomGisBox=new GisBox(startZoomGisPoint,endZoomGisPoint);
					 
					 gisMap.zoomTo(zoomGisBox);
					 
					 
					 
					 }
					 
					 */
					
				}
				
				break;
				
			default:
				
				break;
				
	    }
		
		if(e.isPopupTrigger())
			
			showPopupMenu(e);
		
		//	repaint();
		
    }
	
    
	
    public void clearModes()
		
    {
		
		//Move_Mode
		
		movingGisObject=null;
		
		movingGisPoint=null;
		
		//Relation_Mode
		
		startRelationGisObject=null;
		
		startRelationGisPoint=null;
		
		endRelationGisObject=null;
		
		endRelationGisPoint=null;
		
		//ZoomIn_Mode
		
		startZoomGisPoint=null;
		
		endZoomGisPoint=null;
		
		zoomGisBox=null;
		
		selectedGisObject=null;
		
		selectedGisRelation=null;
		
    }
	
	
	
    private void maybeShowPopupCreate(MouseEvent e)
		
    {
		
		
		
		if (e.isPopupTrigger())
			
	    {
			
			JPopupMenu popup = new JPopupMenu();
			
			for(int i=0;i<gisObjectClasses.size();i++)
				
		    {
				
				GisObjectClass gisObjectClass = (GisObjectClass)gisObjectClasses.elementAt(i);
				
				JMenuItem menuItem = new JMenuItem(gisObjectClass.getName());
				
				menuItem.addActionListener(this);
				
				popup.add(menuItem);
				
		    }
			
			
			
			popup.show(e.getComponent(),
					   
					   e.getX(), e.getY());
			
	    }
		
    }
	
	
	
    /*    private void maybeShowPopupRemove(MouseEvent e)
	 
	 {
	 
	 if (e.isPopupTrigger())
	 
	 {
	 
	 JPopupMenu popup = new JPopupMenu();
	 
	 JMenuItem menuItem = new JMenuItem("remove "+selectedGisObject.getName());
	 
	 menuItem.addActionListener(this);
	 
	 popup.add(menuItem);
	 
	 popup.show(e.getComponent(),
	 
	 e.getX(), e.getY());
	 
	 }
	 
	 }
	 
	 */
	
    // MouseMotionListener:
	
    public void mouseMoved(MouseEvent e)
		
    {
		
    }
	
    
	
    public void mouseDragged(MouseEvent e)
		
    {
		
		GisPoint gisPoint = gisMap.trafo.convert(e);
		
		switch(mode)
			
	    {
			
			case MOVE_MODE:
				
				if(movingGisObject!=null)
					
					movingGisPoint=gisPoint;
				
				break;
				
			case RELATION_MODE:
				
				if(startRelationGisObject!=null)
					
				{
					
					try
						
					{
						
						GisObject tmpGO=netModel.snap(gisPoint);
						
						if(tmpGO!=null && !tmpGO.equals(startRelationGisObject))
							
						{
							
							if(netModel.isAllowedRelation(startRelationGisObject,tmpGO))
								
								endRelationGisPoint=tmpGO.getBasePoint();
								
							else
								
								endRelationGisPoint=null;
							
						}
							
						else
							
							endRelationGisPoint=null;
						
					}
					
					catch(ObjectNotFoundException ex)
						
					{
						
						endRelationGisPoint=null;
						
					}
					
				}
				
				break;
				
			case ZOOMIN_MODE:
				
				if(startZoomGisPoint!=null)
					
				{
					
					endZoomGisPoint=gisPoint;
					
					
					
					//double zoomInWidth = endZoomGisPoint.getX()-startZoomGisPoint.getX();
					
					//double zoomInHeight = zoomInWidth/gisMap.ratioWtoH;
					
					
					
					//endZoomGisPoint.setLocation(startZoomGisPoint.getX()+zoomInWidth,startZoomGisPoint.getY()+zoomInHeight);
					
					zoomGisBox=gisMap.getZoomGisBox(startZoomGisPoint,endZoomGisPoint);
					
					
					
					/*
					 
					 if(endZoomGisPoint==startZoomGisPoint)
					 
					 endZoomGisPoint = null;
					 
					 */
					
				}
				
				break;
				
			default:
				
				break;
				
	    }
		
		repaint();
		
    }
	
    
	
    public void createFileMenu()
		
    {
		
		JMenu edit = new JMenu (I18n.get("netView_JMenu_edit"));
		
		JMenu view = new JMenu (I18n.get("netView_JMenu_view"));
		
		
		
		JMenu menuObjectProps = new JMenu (I18n.get("netView_JMenu_ObjectProperties"));
		
		JMenu menuRelationProps = new JMenu (I18n.get("netView_JMenu_RelationProperties"));
		
		JMenu subMenu=null;
		
		
		
		
		
		JCheckBoxMenuItem cb;
		
		
		
		for(int i=0;i<gisObjectClasses.size();i++)
			
	    {
			
			GisObjectClass gisObjectClass=(GisObjectClass)gisObjectClasses.elementAt(i);
			
			
			
			String key=gisObjectClass.getKey();
			
			String name=gisObjectClass.getName();
			
			Image symbol=gisObjectClass.getSymbol();
			
			
			
			subMenu=new JMenu(name);
			
			subMenu.setIcon(new ImageIcon(gisObjectClass.getSymbol()));
			
			menuObjectProps.add(subMenu);
			
			
			
			cb = new JCheckBoxMenuItem(name,new ImageIcon(symbol),true);
			
			cb.setHorizontalTextPosition(SwingConstants.RIGHT);
			
			cb.addActionListener(netModel);
			
			cb.setActionCommand(gisObjectClass.getKey());
			
			subMenu.add(cb);
			
			
			
			// ID
			
			String action="show_"+key+"_ID";
			
			cb = new JCheckBoxMenuItem("ID",netModel.isVisibleProp(action));
			
			cb.setHorizontalTextPosition(SwingConstants.RIGHT);
			
			cb.addActionListener(netModel);
			
			cb.setActionCommand(action);
			
			subMenu.add(cb);
			
			
			
			for(int n=0;n<gisObjectClass.getSimplePropertySize();n++)
				
		    {
				
				String propKey=gisObjectClass.getSimplePropertyKey(n);
				
				String propName=gisObjectClass.getSimplePropertyName(n);
				
				action="show_"+key+"_"+propKey;
				
				cb = new JCheckBoxMenuItem(propName,netModel.isVisibleProp(action));
				
				cb.setHorizontalTextPosition(SwingConstants.RIGHT);
				
				cb.addActionListener(netModel);
				
				cb.setActionCommand(action);
				
				subMenu.add(cb);
				
		    }
			
	    }
		
		
		
		subMenu.addSeparator();
		
		//
		
		for(int i=0;i<gisRelationClasses.size();i++)
			
	    {
			
			GisRelationClass gisRelationClass=(GisRelationClass)gisRelationClasses.elementAt(i);
			
			
			
			String key=gisRelationClass.getKey();
			
			String name=gisRelationClass.getName();
			
			Image symbol=gisRelationClass.getSymbol();
			
			
			
			subMenu=new JMenu(name);
			
			subMenu.setIcon(new ImageIcon(gisRelationClass.getSymbol()));
			
			menuObjectProps.add(subMenu);
			
			
			
			cb = new JCheckBoxMenuItem(name,new ImageIcon(symbol),true);
			
			cb.setHorizontalTextPosition(SwingConstants.RIGHT);
			
			cb.addActionListener(netModel);
			
			cb.setActionCommand(gisRelationClass.getKey());
			
			subMenu.add(cb);
			
			
			
			// ID
			
			String action="show_"+key+"_ID";
			
			cb = new JCheckBoxMenuItem("ID",netModel.isVisibleProp(action));
			
			cb.setHorizontalTextPosition(SwingConstants.RIGHT);
			
			cb.addActionListener(netModel);
			
			cb.setActionCommand(action);
			
			subMenu.add(cb);
			
			
			
			for(int n=0;n<gisRelationClass.getSimplePropertySize();n++)
				
		    {
				
				String propKey=gisRelationClass.getSimplePropertyKey(n);
				
				String propName=gisRelationClass.getSimplePropertyName(n);
				
				
				
				action="show_"+key+"_"+propKey;
				
				cb = new JCheckBoxMenuItem(propName,netModel.isVisibleProp(action));
				
				cb.setHorizontalTextPosition(SwingConstants.RIGHT);
				
				cb.addActionListener(netModel);
				
				cb.setActionCommand(action);
				
				subMenu.add(cb);
				
		    }
			
			
			
	    }
		
		subMenu.addSeparator();
		
		
		
		cb = new JCheckBoxMenuItem(I18n.get("netViewShowVerbose"),netModel.beVerbose());
		
		cb.setHorizontalTextPosition(SwingConstants.RIGHT);
		
		cb.addActionListener(netModel);
		
		cb.setActionCommand("verboseView");
		
		menuObjectProps.add(cb);
		
		
		
		
		
		JMenuItem mi;
		
		mi = new JMenuItem(I18n.get("netView_JMenuItem_newRelation"));
		
		mi.setActionCommand("createRelation");
		
		mi.addActionListener(this);
		
		edit.add(mi);
		
		/*
		 
		 mi = new JMenuItem("new object");
		 
		 mi.setActionCommand("createObject");
		 
		 mi.addActionListener(this);
		 
		 edit.add(mi);
		 
		 */
		
		mi = new JMenuItem(I18n.get("netView_JMenuItem_removeRelation"));
		
		mi.setActionCommand("removeRelation");
		
		mi.addActionListener(this);
		
		edit.add(mi);
		
		mi = new JMenuItem(I18n.get("netView_JMenuItem_removeObject"));
		
		mi.setActionCommand("removeObject");
		
		mi.addActionListener(this);
		
		edit.add(mi);
		
		
		
		mi = new JMenuItem(I18n.get("netView_JMenuItem_zoom in"));
		
		mi.setActionCommand("zoomIn");
		
		mi.addActionListener(this);
		
		view.add(mi);
		
		mi = new JMenuItem(I18n.get("netView_JMenuItem_zoom out"));
		
		mi.setActionCommand("zoomOut");
		
		mi.addActionListener(this);
		
		view.add(mi);
		
		mi = new JMenuItem(I18n.get("netView_JMenuItem_pan to"));
		
		mi.setActionCommand("panTo");
		
		mi.addActionListener(this);
		
		view.add(mi);
		
		mi = new JMenuItem(I18n.get("netView_JMenuItem_move object"));
		
		mi.setActionCommand("moveObject");
		
		mi.addActionListener(this);
		
		view.add(mi);
		
		
		
		edit.addSeparator();
		
		view.addSeparator();
		
		
		
		JButton plus = new JButton(I18n.get("netView_JButton_Symbols+"));
		
		plus.setActionCommand("incSymbolSize");
		
		plus.addActionListener(this);
		
		JButton minus = new JButton(I18n.get("netView_JButton_Symbols-"));
		
		minus.setActionCommand("decSymbolSize");
		
		minus.addActionListener(this);
		
		
		
		JButton fullExtent = new JButton(I18n.get("netView_JButton_full extent"));
		
		fullExtent.setActionCommand("fullExtent");
		
		fullExtent.addActionListener(this);
		
		//JButton previousView = new JButton("Previous View");
		
		//previousView.addActionListener(this);
		
		JButton legend = new JButton(I18n.get("netView_JButton_Show Legend"));
		
		legend.setActionCommand("showLegend");
		
		legend.addActionListener(this);
		
		
		
		tfAction = new JTextField("MODE");
		
		tfAction.setEditable(false);
		
		tfAction.setForeground(Color.red);
		
		
		
		JMenuBar menubar= new JMenuBar();
		
		menubar.add(edit);
		
		menubar.add(view);
		
		menubar.add(menuObjectProps);
		
		menubar.add(plus);
		
		menubar.add(minus);
		
		menubar.add(fullExtent);
		
		//menubar.add(previousView);
		
		menubar.add(legend);
		
		//menubar.add(legend);
		
		menubar.add(tfAction);
		
		setJMenuBar(menubar);
		
    }
	
    
	
    public void actionPerformed(ActionEvent e)
		
    {
		
		String action = e.getActionCommand();
		
		System.out.println(action);
		
		
		
		if(action.equals("createRelation"))
			
	    {
			
			setMode(RELATION_MODE);
			
	    }
		
		if(action.equals("createObject"))
			
	    {
			
			setMode(CREATE_MODE);
			
	    }
		
		if(action.equals("removeRelation"))
			
	    {
			
			setMode(REMOVE_RELATION);
			
	    }
		
		if(action.equals("removeObject"))
			
			setMode(REMOVE_OBJECT);
		
		
		
		if(action.equals("zoomIn"))
			
			setMode(ZOOMIN_MODE);
		
		
		
		if(action.equals("zoomOut"))
			
			gisMap.zoomOut();
		
		//	    setMode(ZOOMOUT_MODE);
		
		
		
		if(action.equals("panTo"))
			
			//setMode(PAN_MODE);
			setMode(PAN2_MODE);
		
		
		if(action.equals("moveObject"))
			
			setMode(MOVE_MODE);
		
		
		
		if(action.equals("incSymbolSize"))
			
			gisMap.scaleplus();
		
		
		
		if(action.equals("decSymbolSize"))
			
			gisMap.scaleminus();
		
		
		
		if(action.equals("fullExtent"))
			
			gisMap.zoomToFullExtent();
		
		
		
		if(action.equals("showLegend"))
			
			showLegend();
		
		
		
		
		
		/*
		 
		 for(int i = 0; i<gisObjectClasses.size(); i++)
		 
		 {
		 
		 GisObjectClass gisObjectClass = (GisObjectClass)gisObjectClasses.elementAt(i);
		 
		 if(action.equals(gisObjectClass.getName()))
		 
		 {
		 
		 System.out.println("Neues Object!"+gisObjectClass.getName());
		 
		 GisObject newObject;
		 
		 try
		 
		 {
		 
		 //newObject = (GisObject)gisObjectClass.createObject();
		 
		 //				    newObject.setBasePoint(createGisPoint);
		 
		 //				    createGisPoint=null;
		 
		 //				    gisMap.updateImage();
		 
		 }
		 
		 catch(Exception ex)
		 
		 {
		 
		 System.out.println(ex.getMessage());
		 
		 }
		 
		 }
		 
		 }
		 
		 if(selectedGisObject!=null && action.equals("remove "+selectedGisObject.getName()))
		 
		 {
		 
		 System.out.println("Remove "+selectedGisObject.getName());
		 
		 }
		 
		 */
		
    }
	
    
	
    public void drawHighlighted(Graphics g)
		
    {
		
		g.setColor(Color.red);
		
		if(selectedGisObject!=null)
			
	    {
			
			try
				
		    {
				
				GisPoint gp=selectedGisObject.getBasePoint();
				
				Object oId=selectedGisObject.getId();
				
				GisObjectClass gisObjectClass=(GisObjectClass)selectedGisObject.getGisElementClass();
				
				Image symbol=gisObjectClass.getSymbol();
				
				int symbolHeight=symbol.getHeight(null);
				
				int	yOffset=symbolHeight/2;
				
				ScreenPoint sp=gisMap.trafo.convert(gp);
				
				//		Font font = new Font("SansSerif",Font.PLAIN,(int)gisMap.getScale());
				
				//		g.setFont(font);
				
				g.drawString("#"+oId.toString(),(int)sp.getX(),(int)sp.getY()-yOffset);
				
		    }
			
			catch(ObjectNotFoundException e)
				
		    {
				
				//
				
		    }
			
	    }
		
		g.setColor(Color.black);
		
    }
	
    
	
    public void drawMovingObjects(Graphics g)
		
    {
		
		g.setColor(Color.red);
		
		if(movingGisObject!=null && movingGisPoint!=null)
			
	    {
			
			ScreenPoint sp=gisMap.trafo.convert(movingGisPoint);
			
			//		Font font = new Font("SansSerif",Font.PLAIN,(int)gisMap.getScale());
			
			//		g.setFont(font);
			
			g.drawString(movingGisObject.getName()+"#"+movingGisObject.getId().toString(),(int)sp.getX(),(int)sp.getY());
			
	    }
		
    }
	
    
	
    public void drawCreatingRelation(Graphics g)
		
    {
		
		Graphics2D g2=(Graphics2D)g;
		
		g2.setColor(Color.red);
		
		if(startRelationGisPoint!=null && endRelationGisPoint!=null)
			
	    {
			
			ScreenPoint spa=gisMap.trafo.convert(startRelationGisPoint);
			
			ScreenPoint spe=gisMap.trafo.convert(endRelationGisPoint);
			
			BasicStroke stroke = new BasicStroke((float)gisMap.getScale());
			
			g2.setStroke(stroke);
			
			g2.drawLine((int)spa.getX(),(int)spa.getY(),(int)spe.getX(),(int)spe.getY());
			
	    }
		
    }
	
    
	
    public void drawZoomInBox(Graphics g)
		
    {
		
		if(zoomGisBox!=null)
			
	    {
			
			g.setColor(Color.red);
			
			ScreenBox box=gisMap.trafo.convert(zoomGisBox);
			
			g.drawRect((int)box.getX(),    (int)box.getY(),
					   
						   (int)box.getWidth(),(int)box.getHeight());
			
			
			
			//		ScreenPoint spa = gisMap.trafo.convert(startZoomGisPoint);
			
			//		ScreenPoint spe = gisMap.trafo.convert(endZoomGisPoint);
			
			//		g.drawRect((int)spa.getX(), (int)spa.getY(), (int)(spe.getX()-spa.getX()), (int)(spe.getY()-spa.getY()));
			
	    }
		
    }
	
	
	
	
	
    public void showLegend()
		
    {
		
		JInternalFrame legend = new JInternalFrame("Legend",true,true,true,true);
		
		legend.getContentPane().setLayout(new GridLayout(gisObjectClasses.size(),2));
		
		for(int i=0;i<gisObjectClasses.size();i++)
			
	    {
			
			GisObjectClass gisObjectClass=(GisObjectClass)gisObjectClasses.elementAt(i);
			
			JLabel j1 = new JLabel(gisObjectClass.getName());
			
			j1.setToolTipText(gisObjectClass.getDescription());
			
			Image symbol=gisObjectClass.getSymbol();
			
			ImageIcon imIc = new ImageIcon(symbol);
			
			JLabel j2 = new JLabel(imIc);
			
			j2.setToolTipText(gisObjectClass.getDescription());
			
			legend.getContentPane().add(j2);
			
			legend.getContentPane().add(j1);
			
	    }
		
		legend.pack();
		
		legend.setVisible(true);
		
		legend.setSize(300,300);
		
		ViewManager.desktop.add(legend);
		
		legend.moveToFront();
		
    }
	
	
	
    public void showPopupMenu(MouseEvent e)
		
    {
		GisPoint gisPoint=gisMap.trafo.convert(e);
		gisMap.setLastClick(gisPoint);
		if (e.isPopupTrigger())
	    {
			GisObject snapedGisObject=netModel.snap(gisPoint);
			JPopupMenu popup = new JPopupMenu();
			JMenu subMenu=new JMenu(I18n.get("netViewCreateObject"));
			if(snapedGisObject!=null)
		    {
				JMenuItem menuItem=new JMenuItem(I18n.get("netViewOpenDetailedView")+" "+snapedGisObject.getName()+"#"+snapedGisObject.getId());
				int elementTable=snapedGisObject.getGisElementClass().getElementTable();
				menuItem.setActionCommand("openDetailedView,"+elementTable+"#"+snapedGisObject.getId());
				menuItem.addActionListener(netModel);
				popup.add(menuItem);
		    }
			popup.add(subMenu);
			for(int i=0;i<gisObjectClasses.size();i++)
		    {
				GisObjectClass gisObjectClass = (GisObjectClass)gisObjectClasses.elementAt(i);
				JMenuItem menuItem = new JMenuItem(gisObjectClass.getName());
				menuItem.setIcon(new ImageIcon(gisObjectClass.getSymbol()));
				menuItem.setActionCommand("createObject_"+gisObjectClass.getKey());
				menuItem.addActionListener(netModel);
				subMenu.add(menuItem);
		    }
			
			/*
			 GisElementClass gisElementClass=snapedGisObject.getGisElementClass();
			 
			 subMenu=new JMenu(I18n.get("netViewShowElement"));
			 
			 subMenu.setIcon(new ImageIcon(gisElementClass.getSymbol()));
			 
			 popup.add(subMenu);
			 
			 String key=gisElementClass.getKey();
			 
			 String name=gisElementClass.getName();
			 
			 JCheckBoxMenuItem cb;
			 
			 for(int i=0;i<gisElementClass.getSimplePropertySize();i++)
			 
			 {
			 
			 String propKey=gisElementClass.getSimplePropertyKey(i);
			 
			 String propName=gisElementClass.getSimplePropertyName(i);
			 
			 cb = new JCheckBoxMenuItem(propName,true);
			 
			 cb.setHorizontalTextPosition(SwingConstants.RIGHT);
			 
			 cb.addActionListener(netModel);
			 
			 //cb.setActionCommand(key);
			 
			 subMenu.add(cb);
			 
			 }
			 
			 }
			 
			 */
			
			
			
			popup.show(e.getComponent(),
					   
					   e.getX(), e.getY());
			
	    }
		
    }
    
//InternalFrameListener

public void internalFrameActivated(InternalFrameEvent e) {

}

public void internalFrameClosed(InternalFrameEvent e) {
	ProjectView.removeViewFromList(e);
}

public void internalFrameClosing(InternalFrameEvent e) {
}

public void internalFrameDeactivated(InternalFrameEvent e) {

}

public void internalFrameDeiconified(InternalFrameEvent e) {

}

public void internalFrameIconified(InternalFrameEvent e) {

}

public void internalFrameOpened(InternalFrameEvent e) {
	ProjectView.addViewToList(e);
}
	
}

