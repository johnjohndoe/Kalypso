package de.tuhh.wb.javagis.view.netview;



import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Image;
import java.util.Vector;

import javax.ejb.ObjectNotFoundException;
import javax.swing.JOptionPane;
import javax.swing.JPanel;

import de.tuhh.wb.javagis.data.GisObjectClass;



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

		jop.showMessageDialog(this,(Object)"Keine Verkleinerung moeglich+++!","Warnung",JOptionPane.WARNING_MESSAGE);

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
	
	public void panTo2(GisPoint m1,GisPoint m2) //double scale)

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

	updateImage();//(scale);

	repaint();

	}

    

    public void zoomTo(GisBox gisBox)

    {

	this.mapBox    = gisBox;

	this.trafo=new Transformation(screenBox,mapBox,Transformation._UseGK);

	updateImage();

    }

	

    public void zoomToFullExtent()

    {

	boolean minXset=false;

	boolean minYset=false;

	boolean maxXset=false;

	boolean maxYset=false;



	double minX = 0;

	double minY = 0;

	double maxX = 0;

	double maxY = 0;

	

	for(int i=0;i<gisObjectClasses.size();i++)

	    {

		GisObjectClass gisObjectClass=(GisObjectClass)gisObjectClasses.elementAt(i);

		if(!netModel.getHiddenElements().contains(gisObjectClass.getKey()))

		    {

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

	    }

	minY=minY-(maxY-minY)*0.1;



	double gk_centerX;

	double gk_centerY;

	double gk_width;

	double gk_height;



	if(!maxXset) // nothing is set ~ no objetcts

	    {

		gk_centerX=1000;

		gk_centerY=500;

		

		gk_height=200;

		gk_width=ratioWtoH*gk_height;

	    }

	else

	    {

		gk_centerX=(minX+maxX)/2d;

		gk_centerY=(minY+maxY)/2d;

		gk_width=maxX-minX;

		gk_height=maxY-minY;

	    }





	gk_width=gk_width*1.1;

	gk_height=gk_height*1.1;

	

	if(ratioWtoH*gk_height<gk_width) 	//hoehe aus breite berechnen

	    gk_height=gk_width/ratioWtoH;

	else // breite aus hoehe berechnen

	    gk_width=gk_height*ratioWtoH;



	





	GisPoint minGP=new GisPoint(gk_centerX-gk_width/2d,gk_centerY-gk_height/2d);

	GisPoint maxGP=new GisPoint(gk_centerX+gk_width/2d,gk_centerY+gk_height/2d);



	zoomTo(new GisBox(minGP,maxGP));

	//	this.trafo=new Transformation(screenBox,mapBox,Transformation._UseGK);

	//	updateImage();

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
    System.out.println("Update Image!!");

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

