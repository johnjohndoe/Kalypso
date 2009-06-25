package de.tuhh.wb.javagis.view.netview;

import javax.swing.table.AbstractTableModel;

import de.tuhh.wb.javagis.model.ElementSession;
import java.util.Hashtable;
import java.util.Vector;
import java.util.Enumeration;

import java.awt.geom.Point2D;
import java.awt.Image;
import java.awt.Graphics;
import java.awt.Component;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JPanel;

import javax.swing.JInternalFrame;
import java.awt.event.MouseListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionListener;
import java.awt.Color;
import java.awt.Font;

import de.tuhh.wb.javagis.data.*;


public class GisMap extends JPanel
{
    private Image mapBuffer;
   
    public Transformation trafo;
    private GisBox mapBox; // Sichtfenster in GK
    private ScreenBox screenBox; // Sichtfenster in Pixel
	private GisBox lastMapBox;
    public static final double ratioWtoH=1.5;
    private GisNetModel netModel;
    private GisNetView netView;
    private Font font;
	private Vector gisObjectClasses;
	private Vector objectIdListVector;
    
    public GisMap(GisNetModel netModel, GisNetView netView)
    {
	super();
	this.netModel=netModel;
	this.netView=netView;
	this.mapBuffer=null;
	this.screenBox = new ScreenBox(0,0,300,150);
	setSize(300,150);
	setVisible(true);
	this.gisObjectClasses = netModel.myGisObjectClasses;
	this.objectIdListVector = netModel.myObjectIdListVector;
	//noZoom();
    }
    
    
    public void noZoom()
    {
	this.mapBox = new GisBox(1,1,ratioWtoH*200,200);
	this.trafo=new Transformation(screenBox,mapBox,Transformation._UseGK);
	//this.mapBuffer=netModel.getBufferedMap((Component)this,trafo);
	//	refresh();
	//	updateImage(1.4d);
	lastMapBox = this.mapBox;
	repaint();
    }
    
    public void zoomOut(double scale)
    {
	lastMapBox = this.mapBox;
	double f=2; // factor;
	double h=mapBox.getHeight();
	double w=ratioWtoH*h;
	this.mapBox    = new GisBox(new GisPoint(mapBox.p1.x-(f*w-w)/2.0, mapBox.p1.y-(f*h-h)/2.0),
				    new GisPoint(mapBox.p2.x+(f*w-w)/2.0, mapBox.p2.y+(f*h-h)/2.0));
	this.trafo=new Transformation(screenBox,mapBox,Transformation._UseGK);
	updateImage(scale);
	repaint();
    }
    
    public void panTo(GisPoint m, double scale)
    {
	lastMapBox = this.mapBox;
	double h=mapBox.getHeight();
	double w=ratioWtoH*h;
	
	this.mapBox    = new GisBox(new GisPoint(m.x-w/2.0,m.y-h/2.0),new GisPoint(m.x+w/2.0,m.y+h/2.0));
	this.trafo=new Transformation(screenBox,mapBox,Transformation._UseGK);
	updateImage(scale);
	repaint();
    }
	
	public void panTo2(GisPoint m1,GisPoint m2, double scale)
	{
	lastMapBox = this.mapBox;
	double distX = m2.getX()-m1.getX();
	double distY = m2.getY()-m1.getY();
	//System.out.println("DistanzX: "+distX+" DistanzY: "+distY);
	GisPoint gp1 = mapBox.getp1();
	GisPoint gp2 = mapBox.getp2();
	if(distX<0 && distY<=0)
	{
	distY = -1*distY;
	distX = -1*distX;
	this.mapBox = new GisBox(new GisPoint(gp1.x+distX,gp1.y+distY),new GisPoint(gp2.x+distX,gp2.y+distY));
	this.trafo = new Transformation(screenBox,mapBox,Transformation._UseGK);
	}
	else{
	if(distX>0 && distY>=0)
	{
	this.mapBox = new GisBox(new GisPoint(gp1.x-distX,gp1.y-distY),new GisPoint(gp2.x-distX,gp2.y-distY));
	this.trafo = new Transformation(screenBox,mapBox,Transformation._UseGK);
	}
	else{
	if(distX<=0 && distY>0)
	{
	distX = -1*distX;
	this.mapBox = new GisBox(new GisPoint(gp1.x+distX,gp1.y-distY),new GisPoint(gp2.x+distX,gp2.y-distY));
	this.trafo = new Transformation(screenBox,mapBox,Transformation._UseGK);
	}
	else{
	if(distX>=0 && distY<0)
	{
	distY = -1*distY;
	this.mapBox = new GisBox(new GisPoint(gp1.x-distX,gp1.y+distY),new GisPoint(gp2.x-distX,gp2.y+distY));
	this.trafo = new Transformation(screenBox,mapBox,Transformation._UseGK);
	}
		}
			}
				}
	updateImage(scale);
	repaint();
	}
    
    public void zoomTo(GisBox gisBox, double scale)
    {
	lastMapBox = this.mapBox;
	this.mapBox    = gisBox;
	this.trafo=new Transformation(screenBox,mapBox,Transformation._UseGK);
	updateImage(scale);
	repaint();
    }
	
	public void zoomToFullExtent(double scale)
	{
	lastMapBox = this.mapBox;
	double minX = Double.MAX_VALUE;
	double minY = Double.MAX_VALUE;
	double maxX = 0-Double.MAX_VALUE;
	double maxY = 0-Double.MAX_VALUE;
	for(int i=0;i<gisObjectClasses.size();i++)
	{
	GisObjectClass gisObjectClass=(GisObjectClass)gisObjectClasses.elementAt(i);
	Vector idList=(Vector)objectIdListVector.elementAt(i);
		for(int n=0;n<idList.size();n++)
		    {
			Object oId=idList.elementAt(n);
			GisPoint gp=gisObjectClass.getBasePoint(oId);
			double cx=gp.getX();
			double cy=gp.getY();
				if(cx > maxX)
				{
				maxX = cx;
				}
				if(cx < minX)
				{
				minX = cx;
				}
				if(cy > maxY)
				{
				maxY = cy;
				}
				if(cy < minY)
				{
				minY = cy;
				}
			}
	}
	GisPoint gp1 = new GisPoint(maxX,maxY);
	GisPoint gp2 = new GisPoint(minX,minY);
	System.out.println("MaxX: "+maxX+"MaxY: "+maxY+"MinX: "+minX+"MinY: "+minY);
	ScreenPoint sp1 = this.trafo.convert(gp1);
	ScreenPoint sp2 = this.trafo.convert(gp2);
	System.out.println("MaxXsp1: "+sp1.getX()+"MaxYsp1: "+sp1.getY()+"MinXsp2: "+sp2.getX()+"MinYsp2: "+sp2.getY());
	double maxX1 = sp1.getX()-50;
	double maxY1 = sp1.getY()-50;
	double minX1 = sp2.getX()-50;
	double minY1 = sp2.getY()-50;
	ScreenPoint sp1neu = new ScreenPoint(maxX1,maxY1);
	ScreenPoint sp2neu = new ScreenPoint(minX1,minY1);
	GisPoint gp1neu = this.trafo.convert(sp1neu);
	GisPoint gp2neu = this.trafo.convert(sp2neu);
	System.out.println("MaxX1: "+gp1neu.getX()+"MaxY1: "+gp1neu.getY()+"MinX1: "+gp2neu.getX()+"MinY1: "+gp2neu.getY());
	this.mapBox = new GisBox(gp2neu,gp1neu);
	this.trafo=new Transformation(screenBox,mapBox,Transformation._UseGK);
	updateImage(scale);
	repaint();
	}
	
	public void previousView(double lastScale)
	{
	this.mapBox = lastMapBox;
	this.trafo = new Transformation(screenBox, mapBox,Transformation._UseGK);
	updateImage(lastScale);
	repaint();
	}
	
	
    
    public void paint(Graphics g)
    {
	//	Graphics g=getGraphics();
	if(mapBuffer!=null)
	    {
		g.drawImage(mapBuffer,0,0,this);
		netView.drawHighlighted(g);
		netView.drawMovingObjects(g);
		netView.drawCreatingRelation(g);
		netView.drawZoomInBox(g);
	    }
    }
    
    public void update(Graphics g)
    {
	if(mapBuffer!=null)
	    {
		g.drawImage(mapBuffer,0,0,this);
		netView.drawHighlighted(g);
		netView.drawMovingObjects(g);
		netView.drawCreatingRelation(g);
		netView.drawZoomInBox(g);
	    }
    }
    
    public void updateImage(double scale)
    {
	this.mapBuffer=netModel.getBufferedMap((Component)this,trafo,scale);
	//refresh();
	repaint();
    }

    public void setScreenBox(ScreenBox screenBox,double scale)
    {
	this.screenBox=screenBox;
	this.trafo=new Transformation(screenBox,mapBox,Transformation._UseGK);
	updateImage(scale);
    }

}
