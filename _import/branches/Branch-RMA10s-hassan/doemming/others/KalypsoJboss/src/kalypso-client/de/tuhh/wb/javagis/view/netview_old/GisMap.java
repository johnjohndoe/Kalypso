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
import java.awt.MediaTracker;

import de.tuhh.wb.javagis.data.*;


public class GisMap extends JPanel
{
    private Image mapBuffer;
    private Image nullStrand;
    private Image damStrand;
    private Image reservoirStrand;
    private Image channelStrand;
    private Image riverBasin;
    private Image node;
    public Transformation trafo;
    private GisBox mapBox; // Sichtfenster in GK
    private ScreenBox screenBox; // Sichtfenster in Pixel
    public static final double ratioWtoH=2;
    private GisNetModel netModel;
    private GisNetView netView;
    private Font font;
    
    public GisMap(GisNetModel netModel, GisNetView netView)
    {
	super();
	this.netModel=netModel;
	this.netView=netView;
	this.mapBuffer=null;
	this.screenBox = new ScreenBox(0,0,300,150);
	setSize(300,150);
	setVisible(true);
	nullStrand = getToolkit().createImage("symbols/NullStrand.gif");
	damStrand = getToolkit().getImage("symbols/DamStrand.gif");
	reservoirStrand = getToolkit().getImage("symbols/ReservoirStrand.gif");
	channelStrand = getToolkit().getImage("symbols/ChannelStrand.gif");
	riverBasin = getToolkit().getImage("symbols/RiverBasin.gif");
	node = getToolkit().getImage("symbols/Node.gif");
	MediaTracker mt = new MediaTracker(this);
	mt.addImage(nullStrand,0);
	mt.addImage(damStrand,1);
	mt.addImage(reservoirStrand,2);
	mt.addImage(channelStrand,3);
	mt.addImage(riverBasin,4);
	mt.addImage(node,5);
	try{
	    mt.waitForAll();
	    System.out.println("RiverBasin: "+riverBasin.getHeight(this));
	}
	catch(InterruptedException e)
	    {
		System.out.println("\nMediaTracker: "+e.getMessage());
	    }
	
	//noZoom();
    }
    
    
    public void noZoom()
    {
	this.mapBox = new GisBox(1,1,ratioWtoH*200,200);
	this.trafo=new Transformation(screenBox,mapBox,Transformation._UseGK);
	//this.mapBuffer=netModel.getBufferedMap((Component)this,trafo);
	//	refresh();
	//	updateImage(1.4d);
	repaint();
    }
    
    public void zoomOut()
    {
	double f=2; // factor;
	double h=mapBox.getHeight();
	double w=ratioWtoH*h;
	this.mapBox    = new GisBox(new GisPoint(mapBox.p1.x-(f*w-w)/2.0, mapBox.p1.y-(f*h-h)/2.0),
				    new GisPoint(mapBox.p2.x+(f*w-w)/2.0, mapBox.p2.y+(f*h-h)/2.0));
	this.trafo=new Transformation(screenBox,mapBox,Transformation._UseGK);
	updateImage(1.4d);
	repaint();
    }
    
    public void panTo(GisPoint m)
    {
	double h=mapBox.getHeight();
	double w=ratioWtoH*h;
	
	this.mapBox    = new GisBox(new GisPoint(m.x-w/2.0,m.y-h/2.0),new GisPoint(m.x+w/2.0,m.y+h/2.0));
	this.trafo=new Transformation(screenBox,mapBox,Transformation._UseGK);
	updateImage(1.4);
	repaint();
    }
    
    public void zoomTo(GisBox gisBox)
    {
	this.mapBox    = gisBox;
	this.trafo=new Transformation(screenBox,mapBox,Transformation._UseGK);
	updateImage(1.4);
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
	this.mapBuffer=netModel.getBufferedMap((Component)this,trafo,(int)scale*10);
	//refresh();
	repaint();
    }

    public void setScreenBox(ScreenBox screenBox)
    {
	this.screenBox=screenBox;
	trafo=new Transformation(screenBox,mapBox,Transformation._UseGK);
	updateImage(1.4);
    }

}
