package de.tuhh.wb.javagis.view.netview;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.geom.AffineTransform;
import java.util.HashSet;
import java.util.Vector;

import javax.ejb.ObjectNotFoundException;
import javax.swing.JCheckBoxMenuItem;

import de.tuhh.wb.javagis.data.GisElement;
import de.tuhh.wb.javagis.data.GisElementClass;
import de.tuhh.wb.javagis.data.GisObject;
import de.tuhh.wb.javagis.data.GisObjectClass;
import de.tuhh.wb.javagis.data.GisRelation;
import de.tuhh.wb.javagis.data.GisRelationClass;
import de.tuhh.wb.javagis.data.event.ElementClassListener;
import de.tuhh.wb.javagis.view.singleview.GisSingleObjectView;

public class GisNetModel implements ActionListener,ElementClassListener
{
    private final static int MAX_CACHE_ROWS=2000;
    private final static int CACHE_PAGE_SIZE=1000;
    private boolean showVerbose=false;

    public Vector myGisObjectClasses;
    public Vector myObjectIdListVector;

    public Vector myGisRelationClasses;
    private Vector myRelationIdListVector;
    
    private HashSet hiddenElements=new HashSet();
    private HashSet showProperty=new HashSet();

    public HashSet getHiddenElements()
    {
	return hiddenElements;
    }
    // all GisObjectClasses should have symbol-mode
    private GisMap myGisMap=null;

    public void setGisMap(GisMap gisMap)
    {
	this.myGisMap=gisMap;
    }

    public boolean beVerbose()
    {
	return showVerbose;
    }

    //String action="show_"+key+"_"+propKey;
    public boolean isVisibleProp(String action)
    {
	if(showProperty.contains(action))
	    return true;
	else
	    return false;
    }
    
    public GisNetModel(Vector gisObjectClasses, Vector gisRelationClasses)
    {
	//	this.hiddenElements.add("wc2objects");
	//	this.hiddenElements.add("wc2nodes");

	this.myGisObjectClasses=gisObjectClasses;
	this.myObjectIdListVector=new Vector();
	for(int i=0;i<myGisObjectClasses.size();i++)
	    {
		GisObjectClass gisObjectClass=(GisObjectClass)myGisObjectClasses.elementAt(i);
		System.out.println("NetView: show object: "+gisObjectClass.getName());
		Vector oIds;
		try
		    {
			oIds=gisObjectClass.getAllPrimaryKeys(); // ToDo: select from ViewBox
		    }
		catch(ObjectNotFoundException e)
		    {
			oIds=new Vector();
		    }
		myObjectIdListVector.add(oIds);
		gisObjectClass.loadBasePoints(oIds);
		System.out.println("NetView: got PrimaryKeys for Objects");
		gisObjectClass.addElementClassListener(this);
	    }
	this.myGisRelationClasses=gisRelationClasses;
	this.myRelationIdListVector=new Vector();
	for(int i=0;i<myGisRelationClasses.size();i++)
	    {		
		GisRelationClass gisRelationClass=(GisRelationClass)myGisRelationClasses.elementAt(i);	       
		//		System.out.println("NetView: show relation: "+gisRelationClass.getName());
		Vector rIds;
		try
		    {
			rIds=gisRelationClass.getAllPrimaryKeys(); // ToDo: select from ViewBox
		    }
		catch(ObjectNotFoundException e)
		    {
			rIds=new Vector();
		    }
		myRelationIdListVector.add(rIds);
		gisRelationClass.addElementClassListener(this);
		//		System.out.println("NetView: got PrimaryKeys for Relations");
	    }
    }

    public void close()
    {
	for(int i=0;i<myGisObjectClasses.size();i++)
	    {
		GisObjectClass gisObjectClass=(GisObjectClass)myGisObjectClasses.elementAt(i);
		gisObjectClass.removeElementClassListener(this);
	    }
	for(int i=0;i<myGisRelationClasses.size();i++)
	    {		
		GisRelationClass gisRelationClass=(GisRelationClass)myGisRelationClasses.elementAt(i);	       
		gisRelationClass.removeElementClassListener(this);
	    }
    }
    
    public GisObject snap(GisPoint snapPoint)
    {
	GisObject result=null;
	double distance=0; // radius
	double testDistance;
	for(int i=0;i<myGisObjectClasses.size();i++)
	    {
		GisObjectClass gisObjectClass=(GisObjectClass)myGisObjectClasses.elementAt(i);
		if(!hiddenElements.contains(gisObjectClass.getKey()))
		    {
			Vector idList=(Vector)myObjectIdListVector.elementAt(i);
			for(int n=0;n<idList.size();n++)
			    {
				Object oId=idList.elementAt(n);
				try
				    {
					testDistance=gisObjectClass.getBasePoint(oId).distanceSq(snapPoint);
					if(testDistance<distance || result==null)
					    {
						distance=testDistance;
						result=gisObjectClass.getGisObject(oId);
					    }
				    }
				catch(ObjectNotFoundException e)
				    {
					//
				    }
			    }
		    }
	    }
	return result;
    }



    public GisRelation snapRelation(GisPoint snapPoint)
    {
	GisRelation result=null;
	double distance=0; // radius
	double testDistance;
	Transformation trafo=myGisMap.trafo;
	for(int i=0;i<myGisRelationClasses.size();i++)
	    {
		GisRelationClass gisRelationClass=(GisRelationClass)myGisRelationClasses.elementAt(i);
		if(!hiddenElements.contains(gisRelationClass.getKey()))
		    {
			Vector idList=(Vector)myRelationIdListVector.elementAt(i);
			for(int n=0;n<idList.size();n++)
			    {
				Object rId=idList.elementAt(n);
				try
				    {
					
					GisPoint gpSrc=gisRelationClass.getBasePointSource(rId);
					GisPoint gpDest=gisRelationClass.getBasePointDestination(rId);
					double cx=(gpSrc.getX()+gpDest.getX())/2.0d;
					double cy=(gpSrc.getY()+gpDest.getY())/2.0d;				
					testDistance=(new GisPoint(cx,cy)).distanceSq(snapPoint);
					if(testDistance<distance || result==null)
					    {
						distance=testDistance;
						result=gisRelationClass.getGisRelation(rId);
					    }
				    }
				catch(ObjectNotFoundException e)
				    {
					//
				    }
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

		String key=gisRelationClass.getKey();
		Vector propIdsToShow=new Vector();
		if(showProperty.contains("show_"+key+"_ID"))
		    propIdsToShow.add(new Integer(-1));
		for(int sp=0;sp<gisRelationClass.getSimplePropertySize();sp++)
		    {
			String propKey=gisRelationClass.getSimplePropertyKey(sp);
			if(showProperty.contains("show_"+key+"_"+propKey))
			    propIdsToShow.add(new Integer(sp));
		    }
		if(!hiddenElements.contains(gisRelationClass.getKey()))
		    {
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
				try
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



					
					for(int spr=0;spr<propIdsToShow.size();spr++)
					    {
						int propPos=((Integer)propIdsToShow.elementAt(spr)).intValue();
						String text;
						String propName;
						if(propPos==-1)//ID
						    {
							propName="ID:";
							text="#"+rId.toString();
						    }
						else
						    {
							propName=gisRelationClass.getSimplePropertyName(propPos)+":";
							Object value=gisRelationClass.getSimplePropertyValue(rId,propPos);
							if(value!=null)
							    text=value.toString();
							else
							    text="";
						    }
						int yPos=(int)cy-yOffset-(propIdsToShow.size()-1-spr)*g.getFont().getSize();
						if(showVerbose)
						    g.drawString(propName+text,(int)cx,yPos);
						else
						    g.drawString(text,(int)cx,yPos);
					    }					
					//					g.drawString("#"+rId.toString(),(int)cx,(int)cy-yOffset);
					g.setColor(Color.black);
				    }
				catch(ObjectNotFoundException e)
				    {
					//
				    }
			    }
		    }
	    }
	// Objects:

	//String action="show_"+key+"_"+propKey;

	for(int i=0;i<myGisObjectClasses.size();i++)
	    {
		GisObjectClass gisObjectClass=(GisObjectClass)myGisObjectClasses.elementAt(i);
		String key=gisObjectClass.getKey();
		Vector propIdsToShow=new Vector();
		if(showProperty.contains("show_"+key+"_ID"))
		    propIdsToShow.add(new Integer(-1));
		for(int sp=0;sp<gisObjectClass.getSimplePropertySize();sp++)
		    {
			String propKey=gisObjectClass.getSimplePropertyKey(sp);
			if(showProperty.contains("show_"+key+"_"+propKey))
			    propIdsToShow.add(new Integer(sp));
		    }
		if(!hiddenElements.contains(gisObjectClass.getKey()))
		    {
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
				try
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

					for(int spr=0;spr<propIdsToShow.size();spr++)
					    {
						int propPos=((Integer)propIdsToShow.elementAt(spr)).intValue();
						String text;
						String propName;
						if(propPos==-1)//ID
						    {
							propName="ID:";
							text="#"+oId.toString();
						    }
						else
						    {
							propName=gisObjectClass.getSimplePropertyName(propPos)+"=";
							Object value=gisObjectClass.getSimplePropertyValue(oId,propPos);
							if(value!=null)
							    text=value.toString();
							else
							    text="";
						    }
						int yPos=(int)sp.getY()-yOffset-(propIdsToShow.size()-1-spr)*g.getFont().getSize();
						if(showVerbose)
						    g.drawString(propName+text,(int)sp.getX(),yPos);
						else
						    g.drawString(text,(int)sp.getX(),yPos);
					    }					
					g.setColor(Color.black);
				    }
				catch(ObjectNotFoundException e)
				    {
					//
				    }
			    }
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
	gisRelationClass.createRelation(srcGO,destGO);
	Vector idList=(Vector)myRelationIdListVector.elementAt(gisRelationClassNumber.intValue());
	//	idList.add(rId);
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

    public void actionPerformed(ActionEvent e)
    {
	String action = e.getActionCommand();
	System.out.println("action: "+action);
	Object source=e.getSource();
	if(source instanceof JCheckBoxMenuItem)
	    {
		JCheckBoxMenuItem cb=(JCheckBoxMenuItem)source;
		if(action.startsWith("show_"))
		    {
			if(cb.getState())//true
			    {
				System.out.println("on: "+action);
				showProperty.add(action);
			    }	
			else
			    {
				System.out.println("off: "+action);
				showProperty.remove(action);
			    }	
		    }
		else if("verboseView".equals(action))
		    {
			this.showVerbose=cb.getState();
		    }
		else
		    {
			if(cb.getState()) //true : show
			    hiddenElements.remove(action);
			else
			    hiddenElements.add(action);
		    }
	    }
	if(action.startsWith("openDetailedView"))
	    {                      
		int elementTable=Integer.parseInt(action.replaceAll(".+,","").replaceAll("#.+",""));
		Object id=new Integer(action.replaceAll(".+#",""));
		System.out.println("table,id"+elementTable+","+id.toString());
		GisElement gisElement=null;
		for(int i=0;i<myGisObjectClasses.size();i++)
		    {
			GisObjectClass gisObjectClass=(GisObjectClass)myGisObjectClasses.elementAt(i);
			if(gisObjectClass.getElementTable()==elementTable)
				gisElement=gisObjectClass.getGisElement(id);
		    }
		for(int i=0;i<myGisRelationClasses.size();i++)
		    {		
			GisRelationClass gisRelationClass=(GisRelationClass)myGisRelationClasses.elementAt(i);	       
			if(gisRelationClass.getElementTable()==elementTable)
			    gisElement=gisRelationClass.getGisElement(id);
			}
		if(gisElement!=null)
		    {
			GisSingleObjectView.load("detailedView",gisElement);
			//			System.out.println("jaha");
		    }
	    }
	
	//String action="show_"+key+"_"+propKey;

	if(action.startsWith("createObject_"))
	    {
		GisPoint gp=myGisMap.getLastClick();
		if(gp!=null)
		    {
			for(int i=0;i<myGisObjectClasses.size();i++)
			    {
				
				GisObjectClass gisObjectClass = (GisObjectClass)myGisObjectClasses.elementAt(i);
				if(("createObject_"+gisObjectClass.getKey()).equals(action))
				    {
					gisObjectClass.createObject(gp);
				    }	
			    }
		    }	
	    }
	myGisMap.updateImage();
    }
    
    
    private Vector getIds(int elementTable)
    {
	for(int i=0;i<myGisObjectClasses.size();i++)
	    {
		GisElementClass gisElementClass=(GisElementClass)myGisObjectClasses.elementAt(i);
		if(elementTable==gisElementClass.getElementTable())
		    {
			Vector oIds=(Vector)myObjectIdListVector.elementAt(i);
			return oIds;
		    }
	    }
	
	for(int i=0;i<myGisRelationClasses.size();i++)
	    {
		GisElementClass gisElementClass=(GisElementClass)myGisRelationClasses.elementAt(i);
		if(elementTable==gisElementClass.getElementTable())
		    {
			Vector oIds=(Vector)myRelationIdListVector.elementAt(i);
			return oIds;
		    }

	    }
	return new Vector();
    }
    
    public void onTableElementCreate(int elementTable,Object eId)
    {
	Vector oIds= getIds(elementTable);
	if(!oIds.contains(eId))
	    {
		oIds.add(eId);
		if(myGisMap!=null)
		    myGisMap.updateImage();
	    }
    }

    public void onTableElementRemove(int elementTable,Object eId)
    {
	Vector oIds= getIds(elementTable);
	if(oIds.remove(eId))
	    if(myGisMap!=null)
		myGisMap.updateImage();
    }


    public void onSimplePropertyChanged(int elementTable,Object eId)
    {
	if(myGisMap!=null)
	    myGisMap.updateImage();
    }
}
