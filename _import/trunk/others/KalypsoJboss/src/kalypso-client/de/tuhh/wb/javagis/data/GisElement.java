package de.tuhh.wb.javagis.data;

import javax.naming.InitialContext;
import javax.rmi.PortableRemoteObject;
import java.util.Vector;
import java.util.Hashtable;
import java.util.Enumeration;

//import de.tuhh.wb.javagis.model.ElementSession;
import de.tuhh.wb.javagis.view.netview.GisPoint;
public class GisElement
{
    public Object myId;
    public GisElementClass myGisElementClass;

    public GisElement(Object id,GisElementClass gisElementClass)
    {
	this.myId=id;
	this.myGisElementClass=gisElementClass;
    }
    
    public GisElementClass getGisElementClass()
    {
	return myGisElementClass;
    }

    public String getLabel()
    {
	return myGisElementClass.getLabel()+"#"+myId.toString();
    }
    public String getName()
    {
	return myGisElementClass.getName();
    }
    public String getKey()
    {
	return myGisElementClass.getKey();
    }

    public String getDescription()
    {
	return myGisElementClass.getDescription();
    }

    public Object getId()
    {
	return myId;
    }

    public int getSimplePropertySize()
    {
	return myGisElementClass.getSimplePropertySize();
    }
    public String getSimplePropertyName(int position)
    {
	return myGisElementClass.getSimplePropertyName(position);
    }
    public String getSimplePropertyDescription(int position)
    {
	return myGisElementClass.getSimplePropertyDescription(position);
    }
    public Class getSimplePropertyClass(int position)
    {
	return myGisElementClass.getSimplePropertyClass(position);
    }
    public Object getSimplePropertyValue(int position)
    {
	return myGisElementClass.getSimplePropertyValue(myId,position);
    }
    public void setSimplePropertyValue(int position,Object value)
    {
	myGisElementClass.setSimplePropertyValue(myId, position, value);
    }

    public void remove()
    {
	//	mySession.remove(myId);
	//	mySession.remove(this);
    }
}
