package de.tuhh.wb.javagis.data;

import javax.swing.table.AbstractTableModel;
import javax.swing.*;
/*
import de.tuhh.wb.javagis.model.ElementSession;
import de.tuhh.wb.javagis.model.ObjectSession;
import de.tuhh.wb.javagis.model.RelationSession;
*/
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Hashtable;
import java.util.Vector;
import java.util.Enumeration;
import java.util.List;

import java.awt.geom.Point2D;
import java.awt.Toolkit;
import java.awt.Image;
import java.awt.Component;
import java.awt.MediaTracker;
import java.beans.XMLEncoder;
import java.io.BufferedOutputStream;
import java.io.FileOutputStream;
import javax.swing.JButton;

import de.tuhh.wb.javagis.model.GisInterfaceTableModel;
import de.tuhh.wb.javagis.data.event.TableListener;
import de.tuhh.wb.javagis.data.event.ElementClassListener;
import de.tuhh.wb.javagis.data.event.KalypsoEventManager;
import de.tuhh.wb.javagis.tools.I18n;
import javax.ejb.ObjectNotFoundException;

public abstract class GisElementClass implements TableListener
{
    private List elementClassListeners;
    Version myVersion;
    int met; // myElementTable
    Modeldata mm; // myModels
    
    //    final static int MAX_CACHE_ROWS=2000;
    //    final static int CACHE_PAGE_SIZE=1000;
    private static final Component dummyComponent=new Component(){};

    private Hashtable simpleProperties;
    
    // general...
    private Image symbol;
        
    public GisElementClass(Version version,Modeldata model,int elementTable)
    {
	this.elementClassListeners=new ArrayList();
	this.myVersion=version;
	this.mm=model;
	this.met=elementTable;
	this.simpleProperties=new Hashtable();
	this.symbol=null;
	KalypsoEventManager.getInstance().addTableListener(myVersion.getThemeKey(),getVersionId(),elementTable,this);
    }

    public synchronized void addElementClassListener(ElementClassListener listener)
    {
	if(!elementClassListeners.contains(listener))
	    elementClassListeners.add(listener);
    }

    public synchronized void removeElementClassListener(ElementClassListener listener)
    {
	elementClassListeners.remove(listener);
    }

    public void close()
    {
	KalypsoEventManager.getInstance().removeTableListener(myVersion.getThemeKey(),getVersionId(),getElementTable(),this);
    }

    public int getElementTable()
    {
	return met;
    }
    public Object getVersionId()
    {
	return myVersion.getVersionId();
    }
    public String getLabel()
    {
	return myVersion.getLabel()+"/"+getName();
    }

    public ImageIcon getIcon()
    {
	return new ImageIcon(getSymbol());
    }
    
    public Image getSymbol()
    {
	if(symbol==null)
	    {
		if(mm.isRelation[met])
		    {
			String symbolPath="symbols/rel_"+getKey()+".gif";
			System.out.println("Relation: "+symbolPath);
			symbol = Toolkit.getDefaultToolkit().getImage(symbolPath);
		    }
		else
		    symbol = Toolkit.getDefaultToolkit().getImage("symbols/"+mm.elementSymbolNames[met]+".gif");
		MediaTracker mt = new MediaTracker(dummyComponent);
		mt.addImage(symbol,0);
		try
		    {
			mt.waitForAll();
		    }
		catch (InterruptedException e)
		    {
			//nothing
		    }
	    }
	return symbol;
    }
    
    public Vector getAllPrimaryKeys() throws ObjectNotFoundException
    {
	return myVersion.getAllPrimaryKeys(met);
    }
    
    public String getName()
    {
	String language = I18n.getLanguage();
		if(language.equals("eng"))
		   return mm.elementNames[met];
		else
			return I18n.get("GECName_"+mm.elementNames[met]);
    }
    public String getKey()
    {
	return mm.elementKeys[met];
    }
    public String getDescription()
    {
	String language = I18n.getLanguage();
	if(language.equals("eng"))
		   return mm.elementDescriptions[met];
		else
			return I18n.get("GECDescription_"+mm.elementDescriptions[met]);
    }
    public boolean isRelation()
    {
	return mm.isRelation[met];
    }
    public boolean hasSymbol()
    {
	return !mm.elementSymbolNames[met].equals("");
    }
    public int getSimplePropertySize()
    {
	return mm.simplePropertyKeys[met].length;
    }
    public Class getSimplePropertyClass(int n)
    {
	return mm.simplePropertyClasses[met][n];
    }
    public String getSimplePropertyName(int n)
    {
	String language = I18n.getLanguage();
	if(language.equals("eng"))
		   return mm.simplePropertyNames[met][n];
		else
			return I18n.get("GECSimplePropName_"+mm.simplePropertyNames[met][n]);
    }

    public String getSimplePropertyKey(int n)
    {
	return mm.simplePropertyKeys[met][n];
    }

    public String getSimplePropertyFormat(int n)
    {
	return mm.simplePropertyFormats[met][n];
    }

    public String getSimplePropertyDescription(int n)
    {
	String language = I18n.getLanguage();
		if(language.equals("eng"))
		   return mm.simplePropertyDescriptions[met][n];
		else
			return I18n.get("GECDescription_"+mm.simplePropertyDescriptions[met][n]);
    }

    public String getSimplePropertyUnit(int n)
    {
	return mm.simplePropertyUnits[met][n];
    }

    public int getVectorSetSize()
    {
	return mm.vectorSetKeys[met].length;
    }
    public String getVectorSetName(int n)
    {
		/**String language = I18n.getLanguage();
		if(language.equals("eng"))
		  	return mm.vectorSetNames[met][n];
		else
		 return I18n.get("GECVectorSetName_"+mm.vectorSetNames[met][n]);*/
	return mm.vectorSetNames[met][n];
    }
    
    public String getVectorSetDescription(int n)
    {
	String language = I18n.getLanguage();
		if(language.equals("eng"))
		  	return mm.vectorSetDescriptions[met][n];
		else
		 return I18n.get("GECVectorSetDescription_"+mm.vectorSetDescriptions[met][n]);
	//return mm.vectorSetDescriptions[met][n];
    }

    public Vector getVectorSets(Object oId) throws ObjectNotFoundException
    {
	return myVersion.getVectorSets(met,oId);
    }

    public void setVectorSet(Object oId,int pos,Object vectorSet)
    {
	myVersion.setVectorSet(met,oId,pos,vectorSet);
    }

    public void setVectorSets(Object oId,Vector vectorSets)
    {
	myVersion.setVectorSets(met,oId,vectorSets);
    }

    public Object getSimplePropertyValue(Object oId,int n) throws ObjectNotFoundException 
    {
	if(!simpleProperties.containsKey(oId))
	    {
		Vector  result=myVersion.getSimplePropertyRow(met,oId);
		simpleProperties.put(oId,result);
	    }
	return ((Vector)simpleProperties.get(oId)).elementAt(n);
    }

    public void setSimplePropertyValue(Object oId,int n, Object value)
    {
	myVersion.setSimplePropertyValue(met,oId, n, value);
	simpleProperties.remove(oId);
    }

    public void preLoadSimplePropertyValues(Vector eIds)
    {
	try
	    {
		Vector uncachedIds=new Vector();
		for (Enumeration e = eIds.elements() ; e.hasMoreElements() ;)
		    {
			Object test=e.nextElement();
			if(!simpleProperties.containsKey(test))
			    uncachedIds.add(test);
		    }
		Hashtable resultRows=myVersion.getSimplePropertyRows(met,uncachedIds);
		for (Enumeration e = resultRows.keys() ; e.hasMoreElements() ;)
		    {
			Object primKey=e.nextElement();
			simpleProperties.put(primKey,resultRows.get(primKey));
			System.out.println("Cache: add "+primKey);
		    }
	    }
	catch(ObjectNotFoundException e)
	    {
		// nothing
	    }
    }

    public GisElement getGisElement(Object eId)
    {
	if(this instanceof GisRelationClass)
	    return (GisElement) new GisRelation(eId,(GisRelationClass)this);
	else if(this instanceof GisObjectClass)
	    return (GisElement) new GisObject(eId,(GisObjectClass)this);
	return null;
    }

    public void remove(Object eId)
    {
	myVersion.remove(met,eId);
    }
    
    public void onTableElementCreate(int elementTable,Object eId)
    {
	ElementClassListener listener=null;
	for(int i=0;i<elementClassListeners.size();i++)
	    {
		listener=(ElementClassListener)elementClassListeners.get(i);
		listener.onTableElementCreate(elementTable,eId);
	    }
    }
    public void onTableElementRemove(int elementTable,Object eId)
    {
	simpleProperties.remove(eId);

	ElementClassListener listener=null;
	for(int i=0;i<elementClassListeners.size();i++)
	    {
		listener=(ElementClassListener)elementClassListeners.get(i);
		listener.onTableElementRemove(elementTable,eId);
	    }
    }
    public void onSimplePropertyChanged(int elementTable,Object eId)
    {
	simpleProperties.remove(eId);

	ElementClassListener listener=null;
	for(int i=0;i<elementClassListeners.size();i++)
	    {
		listener=(ElementClassListener)elementClassListeners.get(i);
		listener.onSimplePropertyChanged(elementTable,eId);
	    }
    }
}
