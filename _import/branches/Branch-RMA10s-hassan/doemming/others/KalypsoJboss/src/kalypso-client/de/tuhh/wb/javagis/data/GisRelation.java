package de.tuhh.wb.javagis.data;

import javax.naming.InitialContext;
import javax.rmi.PortableRemoteObject;
import java.util.Vector;
import java.util.Hashtable;
import java.util.Enumeration;

//import de.tuhh.wb.javagis.model.ElementSession;
import de.tuhh.wb.javagis.view.netview.GisPoint;
public class GisRelation extends GisElement
{
    public GisRelation(Object id,GisRelationClass gisRelationClass)
    {
	super(id,(GisElementClass)gisRelationClass);
    }
    
    public boolean isRelation()
    {
	return true;
    }
    
    public String getForwardLabel()
    {
	return ((GisRelationClass)myGisElementClass).getForwardLabel();
    }
    public String getBackwardLabel()
    {
	return ((GisRelationClass)myGisElementClass).getBackwardLabel();
    }

    public GisObject getSrcGisObject()
    {
	return ((GisRelationClass)myGisElementClass).getSrcGisObject(myId);
    }
    public GisObject getDestGisObject()
    {
	return ((GisRelationClass)myGisElementClass).getDestGisObject(myId);
    }

    //
    public void remove()
    {
	//	mySession.remove(myId);
	//	mySession.remove(this);
    }
    

}
