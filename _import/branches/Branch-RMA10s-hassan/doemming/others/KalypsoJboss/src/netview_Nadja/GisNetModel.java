package de.tuhh.wb.javagis.view.netview;

import javax.swing.table.AbstractTableModel;

import de.tuhh.wb.javagis.model.ElementSession;
import java.util.Hashtable;
import java.util.Vector;
import java.util.Enumeration;

import java.awt.geom.Point2D;
import java.awt.Image;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.geom.AffineTransform;
import java.awt.BasicStroke;
import java.awt.Font;
import de.tuhh.wb.javagis.data.*;
import de.tuhh.wb.javagis.view.netview.GisNetView;

public class GisNetModel
{
    private final static int MAX_CACHE_ROWS=2000;
    private final static int CACHE_PAGE_SIZE=1000;

    public Vector myGisObjectClasses;
    public Vector myObjectIdListVector;

    private Vector myGisRelationClasses;
    private Vector myRelationIdListVector;
    
    // all GisObjectClasses should have symbol-mode
    public GisNetModel(Vector gisObjectClasses, Vector gisRelationClasses)
    {
	this.myGisObjectClasses=gisObjectClasses;
	this.myObjectIdListVector=new Vector();
	for(int i=0;i<myGisObjectClasses.size();i++)
	    {
		GisObjectClass gisObjectClass=(GisObjectClass)myGisObjectClasses.elementAt(i);
		System.out.println("NetView: show object: "+gisObjectClass.getName());
		myObjectIdListVector.add(gisObjectClass.getAllPrimaryKeys()); // ToDo: select from ViewBox
		System.out.println("NetView: got PrimaryKeys for Objects");
	    }
	this.myGisRelationClasses=gisRelationClasses;
	this.myRelationIdListVector=new Vector();
	for(int i=0;i<myGisRelationClasses.size();i++)
	    {
		GisRelationClass gisRelationClass=(GisRelationClass)myGisRelationClasses.elementAt(i);
		System.out.println("NetView: show relation: "+gisRelationClass.getName());
		myRelationIdListVector.add(gisRelationClass.getAllPrimaryKeys()); // ToDo: select from ViewBox
		System.out.println("NetView: got PrimaryKeys for Relations");
	    }
    }

    public GisObject snap(GisPoint snapPoint)
    {
	GisObject result=null;
	double distance=1000000d; // radius
	double testDistance;
	for(int i=0;i<myGisObjectClasses.size();i++)
	    {
		GisObjectClass gisObjectClass=(GisObjectClass)myGisObjectClasses.elementAt(i);
		Vector idList=(Vector)myObjectIdListVector.elementAt(i);
		for(int n=0;n<idList.size();n++)
		    {
			Object oId=idList.elementAt(n);
			testDistance=gisObjectClass.getBasePoint(oId).distanceSq(snapPoint);
			if(testDistance<distance)
			    {
				distance=testDistance;
				result=gisObjectClass.getGisObject(oId);
			    }
		    }
	    }
	return result;
    }
    
    public Image getBufferedMap(java.awt.Component component, Transformation trafo,double scale)
    {
	Image mapBuffer=component.createImage(component.getSize().width,component.getSize().height);
	Graphics g=mapBuffer.getGraphics();
	double size = scale*10;
	Font font = new Font("SansSerif",Font.PLAIN,(int)size);
	g.setFont(font);
	g.setColor(Color.black);

	// Relations:
	for(int i=0;i<myGisRelationClasses.size();i++)
	    {
		GisRelationClass gisRelationClass=(GisRelationClass)myGisRelationClasses.elementAt(i);
		Image symbol=gisRelationClass.getSymbol();
		int symbolWidth=symbol.getWidth(null);
		int symbolHeight=symbol.getHeight(null);
		int xOffset=0;
		int yOffset=0;
		if(symbolHeight>0 && symbolWidth>0)
		    {
			xOffset=symbolWidth/2;
			yOffset=symbolHeight/2;
		    }
		Graphics2D g2=(Graphics2D)g;
		

		AffineTransform trans_org =  g2.getTransform();

		Vector idList=(Vector)myRelationIdListVector.elementAt(i);
		for(int n=0;n<idList.size();n++)
		    {
			Object rId=idList.elementAt(n);
			GisPoint gpSrc=gisRelationClass.getBasePointSource(rId);
			ScreenPoint spSrc=trafo.convert(gpSrc);
			GisPoint gpDest=gisRelationClass.getBasePointDestination(rId);
			ScreenPoint spDest=trafo.convert(gpDest);
			double cx=(spSrc.getX()+spDest.getX())/2.0d;
			double cy=(spSrc.getY()+spDest.getY())/2.0d;
			g2.setColor(Color.blue);
			BasicStroke stroke = new BasicStroke((float)scale);
			g2.setStroke(stroke);
			g2.drawLine((int)spSrc.getX(),(int)spSrc.getY(),(int)spDest.getX(),(int)spDest.getY());
			AffineTransform trans = new AffineTransform();
			trans.translate(cx,cy);
			trans.rotate(getAngle(spSrc,spDest));
			trans.scale(scale,scale);
			g2.transform(trans);
			g2.drawImage(symbol,(int)-xOffset,(int)-yOffset,null);
			g2.setTransform(trans_org);
			g.setColor(Color.magenta);
			g.drawString("#"+rId.toString(),(int)cx,(int)cy-yOffset);
			g.setColor(Color.black);
		    }
	    }

	// Objects:
	for(int i=0;i<myGisObjectClasses.size();i++)
	    {
		GisObjectClass gisObjectClass=(GisObjectClass)myGisObjectClasses.elementAt(i);
		Image symbol=gisObjectClass.getSymbol();
		int symbolWidth=symbol.getWidth(null);
		int symbolHeight=symbol.getHeight(null);
		int xOffset=0;
		int yOffset=0;
		if(symbolHeight>0 && symbolWidth>0)
		    {
			xOffset=symbolWidth/2;
			yOffset=symbolHeight/2;
		    }
			
		Graphics2D g2=(Graphics2D)g;
			
		AffineTransform trans_org =  g2.getTransform();
			
		Vector idList=(Vector)myObjectIdListVector.elementAt(i);
		for(int n=0;n<idList.size();n++)
		    {
			Object oId=idList.elementAt(n);
			GisPoint gp=gisObjectClass.getBasePoint(oId);
			ScreenPoint sp=trafo.convert(gp);
			double cx=sp.getX();
			double cy=sp.getY();
			AffineTransform trans = new AffineTransform();
			trans.translate(cx,cy);
			trans.scale(scale,scale);
			g2.transform(trans);
			g2.drawImage(symbol,(int)-xOffset,(int)-yOffset,null);
			g2.setTransform(trans_org);
			g.setColor(Color.blue);
			g.drawString("#"+oId.toString(),(int)sp.getX(),(int)sp.getY()-yOffset);
			g.setColor(Color.black);
		    }
	    }
	return mapBuffer;
    }
    
    public boolean isAllowedRelation(GisObject srcGO,GisObject destGO)
    {
	for(int i=0;i<myGisRelationClasses.size();i++)
	    {
		GisRelationClass gisRelationClass=(GisRelationClass)myGisRelationClasses.elementAt(i);
		if(gisRelationClass.isAllowedRelation(srcGO.getKey(),destGO.getKey()))
		    return true;
	    }
	return false;
    }

    public void createRelation(GisObject srcGO,GisObject destGO)
    {
	// test number of possible relations
	Vector possibleRelations=new Vector();
	for(int i=0;i<myGisRelationClasses.size();i++)
	    {
		GisRelationClass gisRelationClass=(GisRelationClass)myGisRelationClasses.elementAt(i);
		if(gisRelationClass.isAllowedRelation(srcGO.getKey(),destGO.getKey()))
		    {
			Vector pair=new Vector();
			pair.add(gisRelationClass);
			pair.add(new Integer(i));
			possibleRelations.add(pair);
		    }
	    }
	if(possibleRelations.size()<1)
	    {
		System.out.println("no possible Relation found :-(");
		return;
	    }
	if(possibleRelations.size()>1)
	    System.out.println("many possible Relations found :-)"); //ToDo: ask for wanted Relation
	
	Vector pair=(Vector)possibleRelations.elementAt(0);
	GisRelationClass gisRelationClass=(GisRelationClass)pair.elementAt(0);
	Integer gisRelationClassNumber=(Integer)pair.elementAt(1);
	Object rId=gisRelationClass.createRelation(srcGO,destGO);
	Vector idList=(Vector)myRelationIdListVector.elementAt(gisRelationClassNumber.intValue());
	idList.add(rId);
    }

    private double getAngle(ScreenPoint sp1,ScreenPoint sp2)
    {
	double dx=sp2.x-sp1.x;
	double dy=sp2.y-sp1.y;
 	double angle;

	if(dx == 0)
	    angle = dy > 0 ? Math.PI/2 : -Math.PI/2; // straight up or straight down
	else if(dx > 0 && dy >= 0) // => 1st quadrant
	    angle = Math.atan(dy/dx);
	else if(dx < 0 && dy >= 0) // => 2nd quadrant
	    angle = Math.PI - Math.atan(-1*dy/dx);
	else if(dx < 0 && dy <= 0) // => 3rd quadrant
	    angle = Math.PI + Math.atan(dy/dx);
	else // => 4th quadrant
	    angle =- Math.atan(-1*dy/dx);
	return angle;
    }
}
