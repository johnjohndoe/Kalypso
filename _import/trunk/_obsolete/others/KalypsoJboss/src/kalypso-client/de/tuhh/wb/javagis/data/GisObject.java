package de.tuhh.wb.javagis.data;

import javax.ejb.ObjectNotFoundException;

import de.tuhh.wb.javagis.view.netview.GisPoint;
public class GisObject extends GisElement
{
    public GisObject(Object id,GisObjectClass gisObjectClass)
    {
	super(id,(GisElementClass)gisObjectClass);
    }
    
    public boolean isRelation()
    {
	return false;
    }

    public int getForwardRelationSize()
    {
	return ((GisObjectClass)myGisElementClass).getForwardRelationSize(myId);
    }
    public int getBackwardRelationSize()
    {
	return ((GisObjectClass)myGisElementClass).getBackwardRelationSize(myId);
    }

    public String getForwardRelationLabel(int pos)
    {
	return "("+getForwardRelation(pos).getForwardLabel()+") "+
	    getForwardRelatedGisObject(pos).getLabel()+"->";
    }
    public String getBackwardRelationLabel(int pos)
    {
	return "<--"+getBackwardRelatedGisObject(pos).getLabel()+
	    " ("+getBackwardRelation(pos).getBackwardLabel()+")";
    }

    public GisRelation getForwardRelation(int pos)
    {
	return ((GisObjectClass)myGisElementClass).getForwardRelation(myId,pos);
    }

    public GisRelation getBackwardRelation(int pos)
    {
	return ((GisObjectClass)myGisElementClass).getBackwardRelation(myId,pos);
    }

    public GisObject getForwardRelatedGisObject(int pos)
    {
	return getForwardRelation(pos).getDestGisObject();
    }
    public GisObject getBackwardRelatedGisObject(int pos)
    {
	return getBackwardRelation(pos).getSrcGisObject();
    }

    public void setBasePoint(GisPoint gp)
    {
	((GisObjectClass)myGisElementClass).setBasePoint(myId,gp);    
    }

    public GisPoint getBasePoint() throws ObjectNotFoundException
    {
	return ((GisObjectClass)myGisElementClass).getBasePoint(myId);    
    }

    //
    public void remove()
    {
	//	mySession.remove(myId);
	//	mySession.remove(this);
    }
    

}
