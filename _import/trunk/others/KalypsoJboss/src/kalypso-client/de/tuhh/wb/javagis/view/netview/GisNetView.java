package de.tuhh.wb.javagis.view.netview;

import javax.swing.table.AbstractTableModel;
import javax.swing.*;
import de.tuhh.wb.javagis.tools.I18n;

//import de.tuhh.wb.javagis.model.ElementSession;
import de.tuhh.wb.javagis.view.ViewManager;
import de.tuhh.wb.javagis.view.singleview.GisSingleObjectView;

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

public class GisNetView extends JInternalFrame implements ComponentListener, MouseListener,MouseMotionListener,ActionListener
{
    private static final int MOVE_MODE=0;
    private static final int CREATE_MODE=1;
    private static final int RELATION_MODE=2;
    private static final int REMOVE_MODE=3;
    private static final int ZOOMIN_MODE=4;
    private static final int ZOOMOUT_MODE=5;
    private static final int PAN_MODE=6;

    private static final int DEFAULT_MODE=PAN_MODE;

    private int mode;
    private GisNetModel netModel;
    private Vector gisObjectClasses;
    private Vector gisRelationClasses;

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
    private GisBox   zoomGisBox=null;

    private JTextField tfAction;
	
    //    public static GisMap gisMap;
    public GisMap gisMap;

    public GisNetView(GisNetModel netModel)
    {
	super("Net View",true,true,true,true);

	this.movingGisObject=null;
	this.movingGisPoint=null;
	this.selectedGisObject=null;
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
		return "move";
	    case CREATE_MODE:
		return "create Object";
	    case RELATION_MODE:
		return "create Relation";
	    case REMOVE_MODE:
		return "remove Object";
	    case ZOOMIN_MODE:
		return "zoom In";
	    case ZOOMOUT_MODE:
		return "zoom Out";
	    case PAN_MODE:
		return "pan";
	    default:
		return "unknown";
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
	this.setSize(getSize().width,(int)(getSize().width/gisMap.ratioWtoH));
	gisMap.setScreenBox(new ScreenBox(0,0,this.getSize().width,this.getSize().height));
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
	    case PAN_MODE:
		if(e.getModifiers()!=MouseEvent.BUTTON3_MASK)
		    {
			gisMap.panTo(gisPoint);
			clearModes();
		    }
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
			/* disabled
			   if((startRelationGisObject=netModel.snap(gisPoint))!=null)
			   startRelationGisPoint=startRelationGisObject.getBasePoint();
			   System.out.println("snaped for Relation..."+startRelationGisObject);
			*/
			break;
		    case REMOVE_MODE:
			// disabled			selectedGisObject = netModel.snap(gisPoint);
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
		showPopupMenu(e);
		//switch(mode)
		//{
		//case MOVE_MODE:
		//select Object

		/*		if(mode==PAN_MODE || mode==MOVE_MODE)
		    {
			System.out.println("singleObjectView...");
			GisObject singleObject=netModel.snap(gisPoint);
			GisSingleObjectView.load("selected Element",singleObject);
		    }
		if (mode!=PAN_MODE&&mode!=CREATE_MODE&&mode!=REMOVE_MODE)
		    {
			setMode(DEFAULT_MODE);
		    }
		*/
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
			gisMap.updateImage();
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
	    case REMOVE_MODE:
		maybeShowPopupRemove(e);
		clearModes();
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
	JMenu edit = new JMenu ("Edit");
 	JMenu view = new JMenu ("View");

 	JMenu menuObjects = new JMenu ("Objects");
 	JMenu menuRelations = new JMenu ("Relations");

	for(int i=0;i<gisObjectClasses.size();i++)
	    {
		JCheckBoxMenuItem cb;
		GisObjectClass gisObjectClass=(GisObjectClass)gisObjectClasses.elementAt(i);
		Image symbol=gisObjectClass.getSymbol();
		String Name=gisObjectClass.getName();
		cb = new JCheckBoxMenuItem(gisObjectClass.getName(),new ImageIcon(symbol),true);
		cb.setHorizontalTextPosition(SwingConstants.RIGHT);
		cb.addActionListener(netModel);
		cb.setActionCommand(gisObjectClass.getKey());
		menuObjects.add(cb);
	    }

	for(int i=0;i<gisRelationClasses.size();i++)
	    {
		JCheckBoxMenuItem cb;
		GisRelationClass gisRelationClass=(GisRelationClass)gisRelationClasses.elementAt(i);
		Image symbol=gisRelationClass.getSymbol();
		String Name=gisRelationClass.getName();
		System.out.println("Relation:Key="+gisRelationClass.getKey());
		if("wc2objects".equals(gisRelationClass.getKey()) ||
		   "wc2nodes".equals(gisRelationClass.getKey()))
		    {
			cb = new JCheckBoxMenuItem(gisRelationClass.getName(),new ImageIcon(symbol),false);
			cb.setEnabled(false);
		    }
		else
		    {
			cb = new JCheckBoxMenuItem(gisRelationClass.getName(),new ImageIcon(symbol),true);
		    }
		cb.setHorizontalTextPosition(SwingConstants.RIGHT);
		cb.addActionListener(netModel);
		cb.setActionCommand(gisRelationClass.getKey());
		menuRelations.add(cb);
	    }
	


	//JMenu legend = new JMenu("Legend");
	
	JMenuItem mi;
	
	/*
	  for(int i=0;i<gisObjectClasses.size();i++)
	  {
	  GisObjectClass gisObjectClass=(GisObjectClass)gisObjectClasses.elementAt(i);
	  Image symbol=gisObjectClass.getSymbol();
	  mi = new JMenuItem(gisObjectClass.getName(),new ImageIcon(symbol));
	  mi.setHorizontalTextPosition(JMenuItem.RIGHT);
	  mi.addActionListener(this);
	  mi.setActionCommand(gisObjectClass.getName()+"legend");
	  legend.add(mi);
	  }
	*/
	
	mi = new JMenuItem("new relation");
	mi.setActionCommand("createRelation");
	mi.addActionListener(this);
	edit.add(mi);
	mi = new JMenuItem("new object");
	mi.setActionCommand("createObject");
	mi.addActionListener(this);
	edit.add(mi);
	mi = new JMenuItem("remove relation");
	mi.setActionCommand("removeRelation");
	mi.addActionListener(this);
	edit.add(mi);
	mi = new JMenuItem("remove object");
	mi.setActionCommand("removeObject");
	mi.addActionListener(this);
	edit.add(mi);
	
	mi = new JMenuItem("zoom in");
	mi.setActionCommand("zoomIn");
	mi.addActionListener(this);
	view.add(mi);
	mi = new JMenuItem("zoom out");
	mi.setActionCommand("zoomOut");
	mi.addActionListener(this);
	view.add(mi);
	mi = new JMenuItem("pan to");
	mi.setActionCommand("panTo");
	mi.addActionListener(this);
	view.add(mi);
	mi = new JMenuItem("move object");
	mi.setActionCommand("moveObject");
	mi.addActionListener(this);
	view.add(mi);
	
	edit.addSeparator();
	view.addSeparator();
	
	JButton plus = new JButton("Symbols+");
	plus.setActionCommand("incSymbolSize");
	plus.addActionListener(this);
	JButton minus = new JButton("Symbols-");
	minus.setActionCommand("decSymbolSize");
	minus.addActionListener(this);
	
	JButton fullExtent = new JButton("full extent");
	fullExtent.setActionCommand("fullExtent");
	fullExtent.addActionListener(this);
	//JButton previousView = new JButton("Previous View");
	//previousView.addActionListener(this);
	JButton legend = new JButton("Show Legend");
	legend.setActionCommand("showLegend");
	legend.addActionListener(this);
		
	tfAction = new JTextField("MODE");
	tfAction.setEditable(false);
	tfAction.setForeground(Color.red);
		
	JMenuBar menubar= new JMenuBar();
	menubar.add(edit);
	menubar.add(view);
	menubar.add(menuObjects);
	menubar.add(menuRelations);
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
		//setMode(REMOVE_MODE);
	    }
	if(action.equals("removeObject"))
	    setMode(REMOVE_MODE);

	if(action.equals("zoomIn"))
	    setMode(ZOOMIN_MODE);
	
	if(action.equals("zoomOut"))
	    gisMap.zoomOut();
	//	    setMode(ZOOMOUT_MODE);

	if(action.equals("panTo"))
	    setMode(PAN_MODE);

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
	gisMap.setLastClick(gisMap.trafo.convert(e));
        if (e.isPopupTrigger())
	    {
		JPopupMenu popup = new JPopupMenu();
		JMenu subMenu=new JMenu(I18n.get("netViewCreateObject"));
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
		popup.show(e.getComponent(),
			   e.getX(), e.getY());
	    }
    }
}
