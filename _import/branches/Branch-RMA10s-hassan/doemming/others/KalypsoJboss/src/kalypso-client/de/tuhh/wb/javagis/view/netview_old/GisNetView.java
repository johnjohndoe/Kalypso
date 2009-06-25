package de.tuhh.wb.javagis.view.netview;

import javax.swing.table.AbstractTableModel;
import javax.swing.*;

import de.tuhh.wb.javagis.model.ElementSession;
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

import javax.swing.JInternalFrame;
import java.awt.event.MouseListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionListener;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.Color;
import java.awt.Font;


import de.tuhh.wb.javagis.data.*;

public class GisNetView extends JInternalFrame implements ComponentListener, MouseListener,MouseMotionListener,ActionListener
{
    private static final int MOVE_MODE=0;
    private static final int CREATE_MODE=1;
    private static final int RELATION_MODE=2;
    private static final int REMOVE_MODE=3;
    private static final int ZOOMIN_MODE=4;
    private static final int ZOOMOUT_MODE=5;
    private static final int PAN_MODE=6;
    
    private int mode;
    private GisNetModel netModel;
    private Vector gisObjectClasses;
    
    private GisObject selectedGisObject;
    private GisObject movingGisObject;
    private GisPoint movingGisPoint;
    private GisPoint createGisPoint;
    
    private GisObject startRelationGisObject=null;
    private GisObject   endRelationGisObject=null;
    private GisPoint  startRelationGisPoint=null;
    private GisPoint    endRelationGisPoint=null;
    private GisPoint startZoomGisPoint=null;
    private GisPoint endZoomGisPoint=null;
    
    //    public static GisMap gisMap;
    public GisMap gisMap;
    
    public GisNetView(GisNetModel netModel)
    {
	super("Net View",true,true,true,true);

	createFileMenu();
	this.movingGisObject=null;
	this.movingGisPoint=null;
	this.selectedGisObject=null;
	this.createGisPoint=null;
	this.mode=MOVE_MODE;
	this.netModel=netModel;
	this.gisObjectClasses =	netModel.myGisObjectClasses;
	
	this.gisMap=new GisMap(netModel,this);
	getContentPane().add(gisMap);
	//this.gisMap.repaint();
	setVisible(true);
	gisMap.noZoom();
	this.addComponentListener(this);
	gisMap.addMouseMotionListener(this);
	gisMap.addMouseListener(this);
	gisMap.repaint();
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
	this.setSize((int)gisMap.ratioWtoH*getSize().height,getSize().height);
	gisMap.setScreenBox(new ScreenBox(0,0,getSize().width,getSize().height));
    }
    
    public void componentShown(ComponentEvent e)
    {
    }
    
    // MouseListener:
    public void mouseClicked(MouseEvent e)
    {
	GisPoint gisPoint = gisMap.trafo.convert(e);
	switch(mode)
	    {
	    case ZOOMOUT_MODE:
		gisMap.zoomOut();
		double heightZoom = (double)gisMap.getHeight();
		//gisMap.updateImage(heightZoom/200);
		clearModes();
		break;
	    case PAN_MODE:
		gisMap.panTo(gisPoint);
		double heightPan = (double)gisMap.getHeight();
		//gisMap.updateImage(heightPan/200);
		clearModes();
		break;
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
		    case RELATION_MODE:
			if((startRelationGisObject=netModel.snap(gisPoint))!=null)
			    startRelationGisPoint=startRelationGisObject.getBasePoint();
			System.out.println("snaped for Relation..."+startRelationGisObject);
			break;
		    case REMOVE_MODE:
			selectedGisObject = netModel.snap(gisPoint);
			break;
		    case ZOOMIN_MODE:
			startZoomGisPoint = gisPoint;
			System.out.println("start Zoom in");
			break;
		    default:
			break;
		    }
		break;
		
	    case MouseEvent.BUTTON3_MASK:  // right
		//switch(mode)
		//{
		//case MOVE_MODE:
		//select Object
		if (mode!=CREATE_MODE&&mode!=REMOVE_MODE){
		    this.mode= MOVE_MODE;
		    System.out.println("toggleMode: MoveMode");
		}
		//break;
		//case RELATION_MODE:
		//select Object
		//this.mode= MOVE_MODE;
		//System.out.println("toggleMode: MoveMode");
		//break;
		//default:
		//break;
		//}
		break;
	    default:
		break;
	    }
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
			double heightMove = gisMap.getHeight();
			gisMap.updateImage(heightMove/200);
			clearModes();
		    }
		break;
	    case RELATION_MODE:
		if(startRelationGisObject!=null)
		    {
			endRelationGisObject=netModel.snap(gisPoint);
			if(endRelationGisObject!=null && startRelationGisObject!=endRelationGisObject)
			    {
				System.out.println("snaped for Relation...(END)"+endRelationGisObject);
				netModel.createRelation(startRelationGisObject,endRelationGisObject);
				double heightRelation = gisMap.getHeight();
				gisMap.updateImage(heightRelation/200);
				clearModes();
			    }
		    }
		break;
	    case CREATE_MODE:
		createGisPoint = gisPoint;
		maybeShowPopupCreate(e);
		clearModes();
		break;
	    case REMOVE_MODE:
		maybeShowPopupRemove(e);
		clearModes();
		break;
	    case ZOOMIN_MODE:
		if(startZoomGisPoint!=null)
		    {
			endZoomGisPoint=gisPoint;
			if(endZoomGisPoint!=null && startZoomGisPoint!=endZoomGisPoint)
			    {
				GisBox zoomGisBox=new GisBox(startZoomGisPoint,endZoomGisPoint);
				gisMap.zoomTo(zoomGisBox);
				double heightZoom = zoomGisBox.getHeight();
				//gisMap.updateImage(heightZoom/50);
				clearModes();
			    }
		    }
		break;
	    default:
		break;
	    }
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

    private void maybeShowPopupRemove(MouseEvent e)
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
    
    // MouseMotionListener:
    public void mouseMoved(MouseEvent e)
    {
	//	repaint();
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
			GisObject tmpGO=netModel.snap(gisPoint);
			if(tmpGO!=null && tmpGO!=startRelationGisObject) //ToDo: GisObject: equal-methode implementieren
			    {
				if(netModel.isAllowedRelation(startRelationGisObject,tmpGO))
				    endRelationGisPoint=tmpGO.getBasePoint();
				else
				    endRelationGisPoint=null;
			    }
			else
			    endRelationGisPoint=null;
		    }
		break;
	    case ZOOMIN_MODE:
		if(startZoomGisPoint!=null)
		    {
			endZoomGisPoint=gisPoint;
			if(endZoomGisPoint==startZoomGisPoint)
			    endZoomGisPoint = null;
		    }
		break;
	    default:
		break;
	    }
	repaint();
    }
    
    public void createFileMenu()
    {
	JMenu edit = new JMenu ("Edit");
	JMenu view = new JMenu ("View");
	
	JMenuItem mi;
	
	mi = new JMenuItem("new relation");
	mi.addActionListener(this);
	edit.add(mi);
	mi = new JMenuItem("new object");
	mi.addActionListener(this);
	edit.add(mi);
	mi = new JMenuItem("remove relation");
	mi.addActionListener(this);
	edit.add(mi);
	mi = new JMenuItem("remove object");
	mi.addActionListener(this);
	edit.add(mi);
		
	mi = new JMenuItem("zoom in");
	mi.addActionListener(this);
	view.add(mi);
	mi = new JMenuItem("zoom out");
	mi.addActionListener(this);
	view.add(mi);
	mi = new JMenuItem("pan to");
	mi.addActionListener(this);
	view.add(mi);
	
	edit.addSeparator();
	view.addSeparator();
	
	JMenuBar menubar= new JMenuBar();
	menubar.add(edit);
	menubar.add(view);
	setJMenuBar(menubar);
    }
    
    public void actionPerformed(ActionEvent e)
    {
	String action = e.getActionCommand();
	System.out.println(action);
	
	if(action.equals("new relation"))
	    {
		System.out.println("create new relation");
		this.mode = RELATION_MODE;
	    }
	if(action.equals("new object"))
	    {
		System.out.println("create new object");
		this.mode = CREATE_MODE;
	    }
	if(action.equals("remove relation"))
	    {
		System.out.println("remove relation");
		//this.mode = REMOVE_MODE;
	    }
	if(action.equals("remove object"))
	    {
		System.out.println("remove object");
		this.mode = REMOVE_MODE;
	    }
	
	if(action.equals("zoom in"))
	    {
		System.out.println("zoom in");
		this.mode = ZOOMIN_MODE;
	    }
	if(action.equals("zoom out"))
	    {
		System.out.println("zoom out");
		this.mode = ZOOMOUT_MODE;
	    }
	if(action.equals("pan to"))
	    {
		System.out.println("pan");
		this.mode = PAN_MODE;
	    }
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
				//				    gisMap.updateImage(1.4d);
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
    }
    
    public void drawHighlighted(Graphics g)
    {
	g.setColor(Color.red);
	if(selectedGisObject!=null)
	    {
		GisPoint gp=selectedGisObject.getBasePoint();
		Object oId=selectedGisObject.getId();
		ScreenPoint sp=gisMap.trafo.convert(gp);
		//Font font = new Font("SansSerif",Font.PLAIN,size);
		//g.setFont(font);
		g.drawString("#"+oId.toString(),(int)sp.getX(),(int)sp.getY());
	    }
	g.setColor(Color.black);
    }
    
    public void drawMovingObjects(Graphics g)
    {
	g.setColor(Color.red);
	if(movingGisObject!=null && movingGisPoint!=null)
	    {
		ScreenPoint sp=gisMap.trafo.convert(movingGisPoint);
		//Font font = new Font("SansSerif",Font.PLAIN,size);
		//g.setFont(font);
		g.drawString(movingGisObject.getName()+"#"+movingGisObject.getId().toString(),(int)sp.getX(),(int)sp.getY());
	    }
    }
    
    public void drawCreatingRelation(Graphics g)
    {
	g.setColor(Color.red);
	if(startRelationGisPoint!=null && endRelationGisPoint!=null)
	    {
		ScreenPoint spa=gisMap.trafo.convert(startRelationGisPoint);
		ScreenPoint spe=gisMap.trafo.convert(endRelationGisPoint);
		g.drawLine((int)spa.getX(),(int)spa.getY(),(int)spe.getX(),(int)spe.getY());
	    }
    }
    
    public void drawZoomInBox(Graphics g)
    {
	g.setColor(Color.red);
	if(startZoomGisPoint!=null && endZoomGisPoint!=null)
	    {
		ScreenPoint spa = gisMap.trafo.convert(startZoomGisPoint);
		ScreenPoint spe = gisMap.trafo.convert(endZoomGisPoint);
		g.drawRect((int)spa.getX(), (int)spa.getY(), (int)(spe.getX()-spa.getX()), (int)(spe.getY()-spa.getY()));
	    }
    }
}

