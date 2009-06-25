package de.tuhh.wb.javagis.view.netview;

import javax.swing.table.AbstractTableModel;
import javax.swing.*;

import de.tuhh.wb.javagis.model.ElementSession;
import de.tuhh.wb.javagis.view.ViewManager;
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
import java.awt.Graphics2D;
import java.awt.BasicStroke;
import java.awt.GridLayout;
import java.awt.Cursor;
import java.awt.Component;

import de.tuhh.wb.javagis.data.*;

public class GisNetView extends JInternalFrame implements ComponentListener, MouseListener,MouseMotionListener,ActionListener
{
    private static final int MOVE_MODE=0;
    private static final int CREATE_MODE=1;
    private static final int RELATION_MODE=2;
    private static final int REMOVE_MODE=3;
    private static final int ZOOMIN_MODE=4;
    private static final int ZOOMOUT_MODE=5;
    //private static final int PAN_MODE=6;
	private static final int PAN2_MODE=7;
    
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
	//private GisPoint panPoint=null;
	private GisPoint pan2Point1=null;
	private GisPoint pan2Point2=null;
	
	private Cursor myCursor = new Cursor(Cursor.HAND_CURSOR);
	
	private JTextField tfAction;
	
	private double scale = 1.0d;
	private double lastscale = 1.0d;
    
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
	this.mode=PAN2_MODE;
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
	this.setSize((int)getSize().width,(int)(getSize().width/gisMap.ratioWtoH));
	//this.setSize((int)(gisMap.ratioWtoH*getSize().height),(int)getSize().height);
	gisMap.setScreenBox(new ScreenBox(0,0,this.getSize().width,this.getSize().height),scale);
    }
    
    public void componentShown(ComponentEvent e)
    {
	System.out.println("component shown...");
	this.setSize((int)getSize().width,(int)(getSize().width/gisMap.ratioWtoH));
	//this.setSize((int)(gisMap.ratioWtoH*getSize().height),(int)getSize().height);
	gisMap.setScreenBox(new ScreenBox(0,0,getSize().width,getSize().height),scale);
    }
    
    // MouseListener:
    public void mouseClicked(MouseEvent e)
    {
	GisPoint gisPoint = gisMap.trafo.convert(e);
	switch(mode)
	    {
	    case ZOOMOUT_MODE:
		lastscale = scale;
		scale = scale-0.3;
		if(scale<=0)
			{
			scale =0.1;
			}
		gisMap.zoomOut(scale);
		clearModes();
		break;
				/**case PAN_MODE:
		gisMap.panTo(gisPoint, scale);
		clearModes();
				 break;*/
	    }
    }
    
    public void mouseEntered(MouseEvent e)
    {
		if(myCursor!=null)
		{
			this.setCursor(myCursor);
		}
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
						/**case PAN_MODE:
			panPoint = gisPoint;
						 break;*/
			case PAN2_MODE:
			pan2Point1 = gisPoint;
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
			myCursor = new Cursor(Cursor.MOVE_CURSOR);
			this.setCursor(myCursor);
		    this.mode= MOVE_MODE;
		    System.out.println("toggleMode: MoveMode");
			tfAction.setText("MOVE_MODE");
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
			lastscale = scale;
			gisMap.updateImage(scale);
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
				lastscale = scale;
				gisMap.updateImage(scale);
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
			//double zoomInWidth = endZoomGisPoint.getX()-startZoomGisPoint.getX();
			//double zoomInHeight = zoomInWidth/gisMap.ratioWtoH;
			//endZoomGisPoint.setLocation(startZoomGisPoint.getX()+zoomInWidth,startZoomGisPoint.getY()+zoomInHeight);
			//repaint();
			if(endZoomGisPoint!=null && startZoomGisPoint!=endZoomGisPoint)
			    {
				GisBox zoomGisBox=new GisBox(startZoomGisPoint,endZoomGisPoint);
				double zoomInWidth = endZoomGisPoint.getX()-startZoomGisPoint.getX();
				double zoomInHeight = startZoomGisPoint.getY()-endZoomGisPoint.getY();
				System.out.println("Breite: "+zoomInWidth+", Höhe: "+zoomInHeight);
				lastscale = scale;
				scale = scale+0.3;
				gisMap.zoomTo(zoomGisBox, scale);
				clearModes();
			    }
		    }
		break;
			/**case PAN_MODE:
		gisMap.panTo(panPoint, scale);
		clearModes();
			 break;*/
		case PAN2_MODE:
		pan2Point2 = gisPoint;
		lastscale = scale;
		gisMap.panTo2(pan2Point1,pan2Point2,scale);
		clearModes();
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
	//Pan_MODE
	//panPoint=null;
	pan2Point1=null;
	pan2Point2=null;
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
		popup.show(e.getComponent(), e.getX(), e.getY());
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
		popup.show(e.getComponent(), e.getX(), e.getY());
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
			//double zoomInWidth = endZoomGisPoint.getX()-startZoomGisPoint.getX();
			//double zoomInHeight = zoomInWidth/gisMap.ratioWtoH;
			//endZoomGisPoint.setLocation(startZoomGisPoint.getX()+zoomInWidth,startZoomGisPoint.getY()+zoomInHeight);
			//repaint();
			if(endZoomGisPoint==startZoomGisPoint)
			    endZoomGisPoint = null;
		    }
		break;
		case PAN2_MODE:
		//pan2Point2 = gisPoint;
		//gisMap.panTo2(pan2Point1,pan2Point2,scale);
	    default:
		break;
	    }
	repaint();
    }
    
    public void createFileMenu()
    {
	JMenu edit = new JMenu ("Edit");
	JMenu view = new JMenu ("View");
	//JMenu legend = new JMenu("Legend");
	
	JMenuItem mi;
		
		/*for(int i=0;i<gisObjectClasses.size();i++)
	    {
		GisObjectClass gisObjectClass=(GisObjectClass)gisObjectClasses.elementAt(i);
		Image symbol=gisObjectClass.getSymbol();
		mi = new JMenuItem(gisObjectClass.getName(),new ImageIcon(symbol));
		mi.setHorizontalTextPosition(JMenuItem.RIGHT);
		mi.addActionListener(this);
		mi.setActionCommand(gisObjectClass.getName()+"legend");
		legend.add(mi);
		 }*/
	
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
		/**mi = new JMenuItem("pan to");
	mi.addActionListener(this);
		 view.add(mi);*/
	mi = new JMenuItem("pan to");
	mi.addActionListener(this);
	view.add(mi);
	mi = new JMenuItem("move mode");
	mi.addActionListener(this);
	view.add(mi);
	
	edit.addSeparator();
	view.addSeparator();
	
	JButton plus = new JButton("+");
	plus.addActionListener(this);
	JButton minus = new JButton("-");
	minus.addActionListener(this);
		
	JButton fullExtent = new JButton("Full Extent");
	fullExtent.addActionListener(this);
	JButton previousView = new JButton("Previous View");
	previousView.addActionListener(this);
	JButton legend = new JButton("Show Legend");
	legend.addActionListener(this);
		
	tfAction = new JTextField("PAN_MODE");
	tfAction.setEditable(false);
	tfAction.setForeground(Color.red);
		
	JMenuBar menubar= new JMenuBar();
	menubar.add(edit);
	menubar.add(view);
	menubar.add(plus);
	menubar.add(minus);
	menubar.add(fullExtent);
	menubar.add(previousView);
	menubar.add(legend);
	//menubar.add(legend);
	menubar.add(tfAction);
	setJMenuBar(menubar);
    }
    
    public void actionPerformed(ActionEvent e)
    {
	String action = e.getActionCommand();
	System.out.println(action);
	
	if(action.equals("new relation"))
	    {
		myCursor = new Cursor(Cursor.DEFAULT_CURSOR);
		this.setCursor(myCursor);
		System.out.println("create new relation");
		this.mode = RELATION_MODE;
		tfAction.setText("NEW RELATION_MODE");
	    }
	if(action.equals("new object"))
	    {
		myCursor = new Cursor(Cursor.DEFAULT_CURSOR);
		this.setCursor(myCursor);
		System.out.println("create new object");
		this.mode = CREATE_MODE;
		tfAction.setText("CREATE OBJECT_MODE");
	    }
	if(action.equals("remove relation"))
	    {
		myCursor = new Cursor(Cursor.DEFAULT_CURSOR);
		this.setCursor(myCursor);
		System.out.println("remove relation");
		//this.mode = REMOVE_MODE;
		tfAction.setText("REMOVE RELATION_MODE");
	    }
	if(action.equals("remove object"))
	    {
		myCursor = new Cursor(Cursor.DEFAULT_CURSOR);
		this.setCursor(myCursor);
		System.out.println("remove object");
		this.mode = REMOVE_MODE;
		tfAction.setText("REMOVE OBJECT_MODE");
	    }
	
	if(action.equals("zoom in"))
	    {
		myCursor = new Cursor(Cursor.CROSSHAIR_CURSOR);
		this.setCursor(myCursor);
		System.out.println("zoom in");
		this.mode = ZOOMIN_MODE;
		tfAction.setText("ZOOMIN_MODE");
	    }
	if(action.equals("zoom out"))
	    {
		myCursor = new Cursor(Cursor.DEFAULT_CURSOR);
		this.setCursor(myCursor);
		System.out.println("zoom out");
		this.mode = ZOOMOUT_MODE;
		tfAction.setText("ZOOMOUT_MODE");
	    }
		/**if(action.equals("pan to"))
	    {
		myCursor = new Cursor(Cursor.HAND_CURSOR);
		this.setCursor(myCursor);
		System.out.println("pan");
		this.mode = PAN_MODE;
		tfAction.setText("PAN_MODE");
		 }*/
	if(action.equals("pan to"))
	    {
		myCursor = new Cursor(Cursor.HAND_CURSOR);
		this.setCursor(myCursor);
		System.out.println("pan");
		this.mode = PAN2_MODE;
		tfAction.setText("PAN_MODE");
	    }
	if(action.equals("move mode"))
	    {
		myCursor = new Cursor(Cursor.MOVE_CURSOR);
		this.setCursor(myCursor);
		System.out.println("move mode");
		this.mode = MOVE_MODE;
		tfAction.setText("MOVE_MODE");
	    }
		
	if(action.equals("+"))
		{
		System.out.println("Vergrößern");
		scaleplus();
		}
	if(action.equals("-"))
		{
		System.out.println("Verkleinern");
		scaleminus();
		}
		
	if(action.equals("Full Extent"))
		{
		gisMap.zoomToFullExtent(scale);
		}
	if(action.equals("Previous View"))
		{
		gisMap.previousView(lastscale);
		 }
	if(action.equals("Show Legend"))
		{
		showLegend();
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
				//				    gisMap.updateImage(scaleplus);
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
		GisObjectClass gisObjectClass=(GisObjectClass)selectedGisObject.getGisElementClass();
		Image symbol=gisObjectClass.getSymbol();
		int symbolHeight=symbol.getHeight(null);
		int	yOffset=symbolHeight/2;
		ScreenPoint sp=gisMap.trafo.convert(gp);
		double size=scale*10;
		Font font = new Font("SansSerif",Font.PLAIN,(int)size);
		g.setFont(font);
		g.drawString("#"+oId.toString(),(int)sp.getX(),(int)sp.getY()-yOffset);
	    }
	g.setColor(Color.black);
    }
    
    public void drawMovingObjects(Graphics g)
    {
	g.setColor(Color.red);
	if(movingGisObject!=null && movingGisPoint!=null)
	    {
		ScreenPoint sp=gisMap.trafo.convert(movingGisPoint);
		double size=scale*10;
		Font font = new Font("SansSerif",Font.PLAIN,(int)size);
		g.setFont(font);
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
		BasicStroke stroke = new BasicStroke((float)scale);
		g2.setStroke(stroke);
		g2.drawLine((int)spa.getX(),(int)spa.getY(),(int)spe.getX(),(int)spe.getY());
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
	
	public void scaleplus()
	{
	lastscale = scale;
	scale=scale+0.1;
	System.out.println("Skalierungsfaktor: "+scale);
	gisMap.updateImage(scale);
	}
	
	public void scaleminus()
	{
	lastscale = scale;
	scale=scale-0.1;
	System.out.println("Skalierungsfaktor: "+scale);
		if(scale<=0)
		{
		JOptionPane jop = new JOptionPane();
		jop.showMessageDialog(this,(Object)"Keine Verkleinerung möglich+++!","Warnung",JOptionPane.WARNING_MESSAGE);
		scale=0.1;
		gisMap.updateImage(scale);
		}
		else{
		gisMap.updateImage(scale);
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
}

