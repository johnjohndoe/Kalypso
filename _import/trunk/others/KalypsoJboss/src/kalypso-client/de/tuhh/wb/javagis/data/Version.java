package de.tuhh.wb.javagis.data;

import javax.naming.InitialContext;
import javax.rmi.PortableRemoteObject;
import java.util.Vector;
import java.util.Hashtable;
import java.util.Enumeration;

//import de.tuhh.wb.javagis.model.na.VersionSessionHome;
//import de.tuhh.wb.javagis.model.na.VersionSession;
//import de.tuhh.wb.javagis.data.VersionSessionHome;// reflectionApi ..
//import de.tuhh.wb.javagis.model.VersionSession;

import java.io.File;
import de.tuhh.wb.javagis.Main;
import de.tuhh.wb.javagis.view.netview.GisPoint;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.*;
import javax.swing.*;
import java.lang.reflect.*;

import  de.tuhh.wb.javagis.view.netview.GisNetModel;
import  de.tuhh.wb.javagis.view.tableview.GisTableModel;
import  de.tuhh.wb.javagis.view.JobRequest;

public class Version // implements ActionListener
{
    /*
      Version is unique with (themeKey,vId)
     */
    
    private static DoubleKeyHash knownVersions = new DoubleKeyHash();

    private String myThemeKey;
    private Object myVersionId;

    private Hashtable myGisObjectClasses;
    private Hashtable myGisRelationClasses;

    private Version(String themeKey,Object vId)
    {		
	this.myThemeKey=themeKey;
	this.myVersionId=vId;
     	this.myGisObjectClasses=new Hashtable();
	this.myGisRelationClasses=new Hashtable();
	
	Modeldata model=getModel();
	// objectClasses:
	for(int i=0;i<model.objectSize;i++)
	    {
		int elementTable=model.getElementTable(model.objectKeys[i]);
		GisObjectClass gisObjectClass=new GisObjectClass(this,model,elementTable,i);
		myGisObjectClasses.put(gisObjectClass.getKey(),gisObjectClass);	
	    }

	// relationClasses:
	for(int i=0;i<model.relationSize;i++)
	    {
		int elementTable=model.getElementTable(model.relationKeys[i]);
		GisRelationClass gisRelationClass=new GisRelationClass(this,model,elementTable,i);
		myGisRelationClasses.put(gisRelationClass.getKey(),gisRelationClass);	
	    }
    }

    // constructor of singelton-pattern
    public static Version getVersion(String themeKey,Object vId)
    {
	if(!knownVersions.containsKey(themeKey,vId))
	    knownVersions.put(themeKey,vId,new Version(themeKey,vId));
	if(knownVersions.containsKey(themeKey,vId))
	    return (Version)knownVersions.get(themeKey,vId);
	else
	    return null;
    }
    
    public String getThemeKey()
    {
	return myThemeKey;
    }
    public Object getVersionId()
    {
	return myVersionId;
    }
    public Vector getGisObjectClasses()
    {
	return new Vector(myGisObjectClasses.values());
    }

    public Vector getGisRelationClasses()
    {
	return new Vector(myGisRelationClasses.values());
    }
    public GisRelationClass getGisRelationClass(String key)
    {
	if(myGisRelationClasses.containsKey(key))
	    return (GisRelationClass)myGisRelationClasses.get(key);
	else
	    return null;
    }
    public GisObjectClass getGisObjectClass(String key)
    {
	if(myGisObjectClasses.containsKey(key))
	    return (GisObjectClass)myGisObjectClasses.get(key);
	else
	    return null;
    }

    /*
    public Vector getGisTableModels()
    {
    }
    */
    public Vector getGisObjectTableModels()
    {
	Vector gisObjectClasses=getGisObjectClasses();
	Vector gisTableModels=new Vector();
	for(int i=0;i<gisObjectClasses.size();i++)
	    {
		GisObjectClass gisObjectClass=(GisObjectClass)gisObjectClasses.elementAt(i);
		GisTableModel gisTableModel=new GisTableModel(gisObjectClass);
		gisTableModels.add(gisTableModel);
	    }
	return gisTableModels;
    }

    public Vector getGisRelationTableModels()
    {
	Vector gisTableModels=new Vector();
	Vector gisRelationClasses=getGisRelationClasses();
	for(int i=0;i<gisRelationClasses.size();i++)
	    {
		GisRelationClass gisRelationClass=(GisRelationClass)gisRelationClasses.elementAt(i);
		GisTableModel gisTableModel=new GisTableModel(gisRelationClass);
		gisTableModels.add(gisTableModel);		
	    }	
	return gisTableModels;
    }
    
    public GisNetModel getGisNetModel()
    {
	Vector netViewObjectClasses=new Vector();
	
	Vector gisObjectClasses=getGisObjectClasses();
	for(int i=0;i<gisObjectClasses.size();i++)
	    {
		if(((GisObjectClass)gisObjectClasses.elementAt(i)).hasSymbol())
		    netViewObjectClasses.add(gisObjectClasses.elementAt(i));
	    }
	
	Vector netViewRelationClasses=getGisRelationClasses();
	return new GisNetModel(netViewObjectClasses,netViewRelationClasses);
    }






    //  VersionProperties: getter and setter 
    public String getLabel()
    {
	return getVersionProject()+"/"+getVersionName();	
    }

    public String getVersionProject()
    {
	return Main.versionClass.getVersionProject(myThemeKey,myVersionId);
    }

    public String getVersionName()
    {
	return Main.versionClass.getVersionName(myThemeKey,myVersionId);
    }
    
    public String getVersionState()
    {
	return Main.versionClass.getVersionState(myThemeKey,myVersionId);
    }

    public String getVersionDescription()
    {
	return Main.versionClass.getVersionDescription(myThemeKey,myVersionId);
    }

    public String getVersionHistory()
    {
	return Main.versionClass.getVersionHistory(myThemeKey,myVersionId);
    }

    public void setVersionProject(String project)
    {
	Main.versionClass.setVersionProject(myThemeKey,myVersionId,project);
    }

    public void setVersionName(String name)
    {
	Main.versionClass.setVersionName(myThemeKey,myVersionId,name);
    }
    
    public void setVersionState(String state)
    {
	Main.versionClass.setVersionState(myThemeKey,myVersionId,state);
    }

    public void setVersionDescription(String description)
    {
	Main.versionClass.setVersionDescription(myThemeKey,myVersionId,description);
    }

    public void setVersionHistory(String history)
    {
	Main.versionClass.setVersionHistory(myThemeKey,myVersionId,history);
    }


    // import/export
    public void importFromXML(File importFile)
    {
	Main.versionClass.importFromXml(myThemeKey,myVersionId,importFile);	
    }
    
    public void exportToXML(File exportFile)
    {
	Main.versionClass.exportToXml(myThemeKey,myVersionId,exportFile);
    }
    
    public Modeldata getModel()
	{
	    return Main.versionClass.getModel(myThemeKey);
	}

    // emulated ElementSession:
    public void remove(int et,Object eId)
    {
	Main.versionClass.remove(myThemeKey,et,eId);
    }


    public Vector getAllPrimaryKeys(int et)
    {
	return Main.versionClass.getPrimaryKeyList(myThemeKey,myVersionId,et);
    }

    public Vector getVectorSets(int et,Object eId)
    {
	return Main.versionClass.getVectorSets(myThemeKey,myVersionId,et,eId);
    }
    
    public void setVectorSet(int et,Object eId,int pos,Object vectorSet)
    {
	Main.versionClass.setVectorSet(myThemeKey,et,eId,pos,vectorSet);
    }
    public void setVectorSets(int et,Object eId,Vector vectorSets)
    {
	Main.versionClass.setVectorSets(myThemeKey,et,eId,vectorSets);
    }

    public Vector getSimplePropertyRow(int et,Object eId)
    {
	return Main.versionClass.getSimplePropertyRow(myThemeKey,et,eId);
    }

    public Hashtable getSimplePropertyRows(int et,Vector eIds)
    {
	return Main.versionClass.getSimplePropertyRows(myThemeKey,et,eIds);
    }

    public void setSimplePropertyValue(int et,Object eId, int pos,Object value)
    {
	Main.versionClass.setSimplePropertyValue(myThemeKey,et,eId,pos,value);
    }

    public Hashtable getBasePoints(int et,Vector eIds)
    {
	return Main.versionClass.getBasePoint(myThemeKey,et,eIds);
    }

    public GisPoint getBasePoint(int et,Object eId)
    {
	return Main.versionClass.getBasePoint(myThemeKey,et,eId);
    }

    public void setBasePoint(int et,Object eId,GisPoint basepoint)
    {
	Main.versionClass.setBasePoint(myThemeKey,et,eId,basepoint);
    }

    //emulated RelationSession:
    public void createRelation(int rt,String srcKey,Object srcId,String destKey,Object destId)
    {
	Main.versionClass.createRelation(myThemeKey,myVersionId,rt,srcKey,srcId,destKey,destId);
    }

    public Vector getRelationVector(int rt,Object rId)
    {
	return Main.versionClass.getRelationVector(myThemeKey,rt,rId);
    }


    //emulated ObjectSession
    public Vector returnForwardRelations(int ot,Object oId)
    {
	return Main.versionClass.returnForwardRelations(myThemeKey,ot,oId);
    }
    public Vector returnBackwardRelations(int ot,Object oId)
    {
	return Main.versionClass.returnBackwardRelations(myThemeKey,ot,oId);
    }
    public void createObject(int ot)
    {
	Main.versionClass.objectCreate(myThemeKey,myVersionId,ot);
    }    
}
