package de.tuhh.wb.javagis.view.netview;

import javax.swing.table.AbstractTableModel;

//import de.tuhh.wb.javagis.model.ElementSession;
import java.util.Hashtable;
import java.util.Vector;
import java.util.Enumeration;
import java.awt.Dimension;
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
import javax.swing.JOptionPane;
import java.awt.event.MouseListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionListener;
import java.awt.Color;
import java.awt.Font;

import de.tuhh.wb.javagis.data.*;
import javax.ejb.ObjectNotFoundException;

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
    private double scale=1d;

    private GisPoint lastClick=null;

    public void setLastClick(GisPoint gp)
    {
	lastClick=gp;
    }

    public GisPoint getLastClick()
    {
	return lastClick;
    }

    public void setScale(double scale)
    {
	this.scale=scale;
    }
    
    public double getScale()
    {
	return this.scale;
    }

    public void scaleplus()
    {
	scale=scale/0.9;
	//	    System.out.println("Skalierungsfaktor: "+scale);
	updateImage();
    }
	
    public void scaleminus()
    {
	scale=0.9*scale;
	if(scale<=0)
	    {
		JOptionPane jop = new JOptionPane();
		jop.showMessageDialog(this,(Object)"Keine Verkleinerung möglich+++!","Warnung",JOptionPane.WARNING_MESSAGE);
	    }
	else{
	    updateImage();
	}
    }



    public GisMap(GisNetModel netModel, GisNetView netView)
    {
	super();
	this.netModel=netModel;
	this.netView=netView;
	this.mapBuffer=null;
	this.screenBox = new ScreenBox(0,0,300,150);
	setPreferredSize(new Dimension(300,150));
	setVisible(true);
	this.gisObjectClasses = netModel.myGisObjectClasses;
	this.objectIdListVector = netModel.myObjectIdListVector;
    }
    
    
    public void noZoom()
    {
	this.mapBox = new GisBox(1,1,ratioWtoH*200,200);
	this.trafo=new Transformation(screenBox,mapBox,Transformation._UseGK);
	repaint();
	//	updateImage();
    }
    
    public void zoomOut()
    {
	double f=2; // factor;
	double h=mapBox.getHeight();
	double w=ratioWtoH*h;
	this.mapBox    = new GisBox(new GisPoint(mapBox.p1.x-(f*w-w)/2.0, mapBox.p1.y-(f*h-h)/2.0),
				    new GisPoint(mapBox.p2.x+(f*w-w)/2.0, mapBox.p2.y+(f*h-h)/2.0));
	this.trafo=new Transformation(screenBox,mapBox,Transformation._UseGK);
	updateImage();
    }
    
    public void panTo(GisPoint m)
    {
	double h=mapBox.getHeight();
	double w=ratioWtoH*h;
	
	this.mapBox    = new GisBox(new GisPoint(m.x-w/2.0,m.y-h/2.0),new GisPoint(m.x+w/2.0,m.y+h/2.0));
	this.trafo=new Transformation(screenBox,mapBox,Transformation._UseGK);
	updateImage();
    }
    
    public void zoomTo(GisBox gisBox)
    {
	this.mapBox    = gisBox;
	this.trafo=new Transformation(screenBox,mapBox,Transformation._UseGK);
	updateImage();
    }
	
    public void zoomToFullExtent()
    {
	// todo: solve this problem with a flag
	boolean minXset=false;
	boolean minYset=false;
	boolean maxXset=false;
	boolean maxYset=false;
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
			try
			    {
				Object oId=idList.elementAt(n);
				GisPoint gp=gisObjectClass.getBasePoint(oId);
				double cx=gp.getX();
				double cy=gp.getY();
				if(cx > maxX || !maxXset)
				    {
					maxX = cx;
					maxXset=true;
				    }
				if(cx < minX || !minXset)
				    {
					minX = cx;
					minXset=true;
				    }
				if(cy > maxY || !maxYset)
				    {
					maxY = cy;
					maxYset=true;
				    }
				if(cy < minY || !minYset)
				    {
					minY = cy;
					minYset=true;
				    }
			    }
			catch(ObjectNotFoundException e)
			    {
				//
			    }
		    }
	    }
	GisPoint gp1;
	GisPoint gp2;
	if(!maxXset) // nothing is set ~ no objetcts
	    {
		gp1 = new GisPoint(1000,500);
		gp2 = new GisPoint(0,0);
	    }
	else
	    {
		if(minX==maxX)
		    maxX=minX+1000;
		if(minY==maxY)
		    maxY=minY+500;
		gp1 = new GisPoint(maxX,maxY);
		gp2 = new GisPoint(minX,minY);
	    }

	//	System.out.println("MaxX: "+maxX+"MaxY: "+maxY+"MinX: "+minX+"MinY: "+minY);
	ScreenPoint sp1 = this.trafo.convert(gp1);
	ScreenPoint sp2 = this.trafo.convert(gp2);
	//	System.out.println("MaxXsp1: "+sp1.getX()+"MaxYsp1: "+sp1.getY()+"MinXsp2: "+sp2.getX()+"MinYsp2: "+sp2.getY());
	// making a frame around...
	double extX=Math.abs(sp1.getX()-sp2.getX())*0.1;
	double extY=Math.abs(sp1.getY()-sp2.getY())*0.1;
	double maxX1 = sp1.getX()+extX;
	double maxY1 = sp1.getY()+extY;
	double minX1 = sp2.getX()-extX;
	double minY1 = sp2.getY()-extY;
	ScreenPoint sp1neu = new ScreenPoint(maxX1,maxY1);
	ScreenPoint sp2neu = new ScreenPoint(minX1,minY1);
	GisPoint gp1neu = this.trafo.convert(sp1neu);
	GisPoint gp2neu = this.trafo.convert(sp2neu);
	//	System.out.println("MaxX1: "+gp1neu.getX()+"MaxY1: "+gp1neu.getY()+"MinX1: "+gp2neu.getX()+"MinY1: "+gp2neu.getY());
	this.mapBox = new GisBox(gp2neu,gp1neu);
	this.trafo=new Transformation(screenBox,mapBox,Transformation._UseGK);
	updateImage();
    }

    /*
    public void zoomToFullExtent()
    {
	// todo: solve this problem with a flag
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
			try
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
			catch(ObjectNotFoundException e)
			    {
				//
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
	updateImage();
    }
    */    
    public void paint(Graphics g)
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
    
    public void updateImage()
    {
	this.mapBuffer=netModel.getBufferedMap((Component)this,trafo,scale);
	repaint();
    }

    public void setScreenBox(ScreenBox screenBox)
    {
	this.screenBox=screenBox;
	trafo=new Transformation(screenBox,mapBox,Transformation._UseGK);
	updateImage();
    }

    public GisBox getZoomGisBox(GisPoint gp1,GisPoint gp2)
    {
	// test minimum size
	double minXDif=30;
	double minYDif=15;
	ScreenPoint sp1=trafo.convert(gp1);
	ScreenPoint sp2=trafo.convert(gp2);
	if( Math.abs(sp2.x-sp1.x) < minXDif &&
	    Math.abs(sp2.y-sp1.y) < minYDif)
	    {
		sp2.x=sp1.x+minXDif;
		sp2.y=sp1.y+minYDif;
		gp2=trafo.convert(sp2);
	    }	

	// test x/y scaling
	double width=Math.abs(gp2.x-gp1.x);
	double height=Math.abs(gp2.y-gp1.y);

	if(width<ratioWtoH*height)
	    width=ratioWtoH*height;
	else
	    height=width/ratioWtoH;
	/*
	  double minX=Math.min(gp1.x-width,gp2.x-width);
	  double maxX=Math.min(gp1.x+width,gp2.x+width);
	  double minY=Math.min(gp1.y-height,gp2.y-height);
	  double maxY=Math.min(gp1.y+height,gp2.y+height);
	*/
	return new GisBox(new GisPoint(gp1.x-width,gp1.y-height),
			  new GisPoint(gp1.x+width,gp1.y+height));
    }
}
