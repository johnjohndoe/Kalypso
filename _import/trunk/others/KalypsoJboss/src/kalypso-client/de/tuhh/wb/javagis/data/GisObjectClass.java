package de.tuhh.wb.javagis.data;

//import javax.swing.table.AbstractTableModel;

//import de.tuhh.wb.javagis.model.ElementSession;
//import de.tuhh.wb.javagis.model.ObjectSession;
import java.util.Hashtable;
import java.util.Vector;
import java.util.Enumeration;

import java.awt.geom.Point2D;
import de.tuhh.wb.javagis.model.BasePointTransfer;         
import de.tuhh.wb.javagis.view.netview.GisPoint;
import javax.ejb.ObjectNotFoundException;

public class GisObjectClass extends GisElementClass
{
    private int mot;//myObjectTable;

    private Hashtable basePoints;

    private Hashtable forwardRelations;
    private Hashtable backwardRelations;
    
    //public GisElementClass(Version version,Modeldata model,int elementTable)
    public GisObjectClass(Version version,Modeldata model,int elementTable,int objectTable)
    {
	super(version,model,elementTable);
	this.mot=objectTable;
	this.basePoints=new Hashtable();
	this.forwardRelations=new Hashtable();
	this.backwardRelations=new Hashtable();
    }
    
    private void loadForwardRelations(Object oId)
    {
	try
	    {
		System.out.println("load ForwardRelation : ID "+oId.toString());
		
		Vector allRelations=myVersion.returnForwardRelations(mot,oId);
		Vector newRelations=new Vector();
		for(int i=0;i<allRelations.size();i++)
		    {
			Vector relations=(Vector)allRelations.elementAt(i);
			String relationClassKey=(String)relations.elementAt(0);
			System.out.println("relation To \""+relationClassKey+"\"");
			GisRelationClass gisRelationClass=myVersion.getGisRelationClass(relationClassKey);
			Vector idList=(Vector)relations.elementAt(1);
			System.out.println("numer of Relations :"+idList.size());
			for(int r=0;r<idList.size();r++)
			    {
				Vector newRelation=new Vector();
				newRelation.add(gisRelationClass); // 0
				newRelation.add(idList.elementAt(r)); // 1
				newRelations.add(newRelation);
			    }				
		    }
		forwardRelations.put(oId,newRelations);
	    }
	catch(ObjectNotFoundException e)
	    {
		//
	    }
    }
    
    private void loadBackwardRelations(Object oId)
    {
	try
	    {
		System.out.println("load BeckwardRelation : ID "+oId.toString());
		
		Vector allRelations=myVersion.returnBackwardRelations(mot,oId);
		
		Vector newRelations=new Vector();
		for(int i=0;i<allRelations.size();i++)
		    {
			Vector relations=(Vector)allRelations.elementAt(i);
			String relationClassKey=(String)relations.elementAt(0);
			System.out.println("relation from \""+relationClassKey+"\"");				
			GisRelationClass gisRelationClass=myVersion.getGisRelationClass(relationClassKey);
			Vector idList=(Vector)relations.elementAt(1);
			System.out.println("numer of Relations :"+idList.size());
			for(int r=0;r<idList.size();r++)
			    {
				Vector newRelation=new Vector();
				newRelation.add(gisRelationClass); // 0
				newRelation.add(idList.elementAt(r)); // 1
				newRelations.add(newRelation);
			    }
		    }
		backwardRelations.put(oId,newRelations);
	    }
	catch(ObjectNotFoundException e)
	    {
		//
	    }
    }

    public GisRelation getForwardRelation(Object oId,int pos)
    {
	if(!forwardRelations.containsKey(oId))
	    loadForwardRelations(oId);
	    
	Vector relations =(Vector)forwardRelations.get(oId);
	if(relations.isEmpty())
	    return null;
	else
	    {
		Vector relation=(Vector)relations.elementAt(pos);
		GisRelationClass gisRelationClass=(GisRelationClass)relation.elementAt(0);
		Object relId=relation.elementAt(1);
		return gisRelationClass.getGisRelation(relId);
	    }
    }
    
    public GisRelation getBackwardRelation(Object oId,int pos)
    {
	if(!backwardRelations.containsKey(oId))
	    loadBackwardRelations(oId);
	    
	Vector relations =(Vector)backwardRelations.get(oId);
	if(relations.isEmpty())
	    return null;
	else
	    {
		Vector relation=(Vector)relations.elementAt(pos);
		GisRelationClass gisRelationClass=(GisRelationClass)relation.elementAt(0);
		Object relId=relation.elementAt(1);
		return gisRelationClass.getGisRelation(relId);
	    }
    }

    public int getForwardRelationSize(Object oId)
    {
	if(!forwardRelations.containsKey(oId))
	    loadForwardRelations(oId);
	Object relations=forwardRelations.get(oId);
	return ((Vector)relations).size();
    }

    public int getBackwardRelationSize(Object oId)
    {
	if(!backwardRelations.containsKey(oId))
	    loadBackwardRelations(oId);
	Object relations=backwardRelations.get(oId);
	return ((Vector)relations).size();
    }
    
    // BasePoints
    public void loadBasePoints(Vector oIds)
    {
	try
	    {
		Vector toLoad=new Vector();
		for(int i=0;i<oIds.size();i++)
		    if(!basePoints.containsKey(oIds.elementAt(i)))
			{
			    toLoad.add(oIds.elementAt(i));
			}
		if(!toLoad.isEmpty())
		    {
			Hashtable bps=myVersion.getBasePoints(met,toLoad);
			for (Enumeration e = bps.keys() ; e.hasMoreElements();)
			    {
				Object oId=e.nextElement();
				basePoints.put(oId,new GisPoint((BasePointTransfer)bps.get(oId)));
			    }
		    }
	    }
	catch(ObjectNotFoundException e)
	    {
		//
	    }
    }
    
    public GisPoint getBasePoint(Object oId) throws ObjectNotFoundException
    {
	if(!basePoints.containsKey(oId))
	    {
		basePoints.put(oId,myVersion.getBasePoint(met,oId));
		//java.util.Random r= new java.util.Random();
		//				basePoints.put(oId,new GisPoint(10d+r.nextDouble()*100d,
		//10d+r.nextDouble()*100d));
	    }
	return (GisPoint)basePoints.get(oId);
    }
    
    public void setBasePoint(Object oId,GisPoint basePoint)
    {
	myVersion.setBasePoint(met,oId,basePoint);
	basePoints.remove(oId);
    }

    public GisObject getGisObject(Object oId)
    {
	return new GisObject(oId,this);
    }

    // create
    public void createObject()
    {
	myVersion.createObject(mot);
    }
   
    // remove
    public void remove(Object oId)
    {
	super.remove(oId);
	forwardRelations.remove(oId);
	backwardRelations.remove(oId);
	basePoints.remove(oId);
    }

    public void onTableElementCreate(int elementTable,Object eId)
    {
	super.onTableElementCreate(elementTable,eId);
    }

    public void onTableElementRemove(int elementTable,Object oId)
    {
	forwardRelations.remove(oId);
	backwardRelations.remove(oId);
	basePoints.remove(oId);
	super.onTableElementRemove(elementTable,oId);
    }
    public void onSimplePropertyChanged(int elementTable,Object eId)
    {
	super.onSimplePropertyChanged(elementTable,eId);
    }
}
