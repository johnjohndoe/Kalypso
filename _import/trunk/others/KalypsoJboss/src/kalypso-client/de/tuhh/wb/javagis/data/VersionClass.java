package de.tuhh.wb.javagis.data;
import java.io.File;
import java.io.PrintWriter;
import java.io.FileWriter;
import java.io.IOException;
import javax.naming.InitialContext;
import javax.rmi.PortableRemoteObject;
import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Vector;
import java.util.Collection;
import java.util.Hashtable;
import java.util.Enumeration;
import javax.ejb.SessionBean;
import javax.ejb.EJBObject;

import de.tuhh.wb.javagis.Main;
import de.tuhh.wb.javagis.FileSystemUtils;
import de.tuhh.wb.javagis.data.event.KalypsoEventManager;

import de.tuhh.wb.javagis.view.netview.GisPoint;

import de.tuhh.wb.javagis.model.BasePointTransfer;         
import de.tuhh.wb.javagis.model.VersionSession;

//import de.tuhh.wb.javagis.model.ElementSession;
//import de.tuhh.wb.javagis.model.ObjectSession;
//import de.tuhh.wb.javagis.model.RelationSession;

import java.lang.reflect.*;

import  de.tuhh.wb.javagis.view.netview.GisNetModel;
import  de.tuhh.wb.javagis.view.tableview.GisTableModel;
import  de.tuhh.wb.javagis.view.JobRequest;

import javax.ejb.ObjectNotFoundException;

import ejb.event.EJBEvent;
import ejb.event.EJBEventListener;
import ejb.event.EJBEventManager;

public class VersionClass implements EJBEventListener
{
    private boolean lostVersionSession;
    private Hashtable versions;
    private Hashtable models;
    private EJBEventManager eventManager;

    private Hashtable env;
    private static VersionClass instance=null;
    private VersionClass(String host,String port,String user,String pass)
    {
	this.versions=new Hashtable();
	this.models=new Hashtable();
	this.lostVersionSession=false;
	this.eventManager=EJBEventManager.getInstance();
	
	this.env=new Hashtable();
	System.out.println("JBOSS_HOST: "+host);
	env.put("java.naming.factory.initial","org.jnp.interfaces.NamingContextFactory");
	env.put("java.naming.factory.url.pkgs","org.jboss.naming:org.jnp.interfaces");
	env.put("java.naming.provider.url",host);
	Main.versionClass=this;
	eventManager.addEJBEventListener(this);
    }

    public static VersionClass getInstance(String host,String port,String user,String pass)
    {
	if(VersionClass.instance==null)
	    VersionClass.instance=new VersionClass(host,port,user,pass);
	return VersionClass.instance;
    }

    public Vector getThemeKeys()
    {
	Vector result=new Vector();
	try
	    {			
		String allKeys=Main.props.getProperty("theme_keys");
		String keys[] = allKeys.split("\\:");
		for(int i=0;i<keys.length;i++)
		    result.add(keys[i]);

		/*
		result.add("Modell");		
		result.add("Control");		
		result.add("SimulationCase");
		result.add("HWS_System");
		result.add("HWS_Szenario");
		result.add("HWS_Logistics");
		result.add("HWS_Combination");
		*/		
		//		result.add("Test2");
	    }
	catch(Exception e)
	    {
		System.out.println("could not get themeKeys");
		log(e);
		System.out.println("getThemeKeys Exception:\n"+e.getMessage());
	    }
	return result;
    }

    public static synchronized void freeResources()
    {
	if(instance!=null)
	    {
		for(Enumeration e=instance.versions.elements();e.hasMoreElements();)
		    {
			try
			    {
				System.out.println("freeing resource");
				((EJBObject)e.nextElement()).remove();
			    }
			catch(Exception err)
			    {
				err.printStackTrace();
				System.out.println("remove session-error: due to timeout session was lost anyway, no problem");
			    }
		    }
		instance.eventManager.removeEJBEventListener(instance);
		instance=null;
	    }
    }
    
    public synchronized VersionSession getVersionSession(String themeKey)
	throws javax.naming.NamingException,
	javax.ejb.FinderException,
	java.rmi.RemoteException,
	java.lang.NoSuchMethodException,
	java.lang.IllegalAccessException,
	java.lang.reflect.InvocationTargetException
    {
	if(!versions.containsKey(themeKey))
	    {
		if(lostVersionSession)
		    {
			System.out.println("timeout ? - versionSession is lost...");
			System.out.println(" try to reload versionSession...");			
		    }
		InitialContext ctx=new InitialContext(env);
		System.out.println("getVersionSession: got InitialContext");
		
		String jndiName=themeKey+".VersionSession";
		System.out.println("JNDI-Name: "+jndiName);
		
		Object objectHome = ctx.lookup(jndiName);
		System.out.println("did lookup");
		
		Class vshc=objectHome.getClass();
		VersionSession versionSession=(VersionSession)(vshc.getMethod("create",null)).invoke(objectHome,null);
		//				VersionSession versionSession=(VersionSession)home.create();
		System.out.println("got VersionSession");
		System.out.println("number of elements: "+versionSession.getSize());
		versions.put(themeKey,versionSession);
	    }
	return (VersionSession)versions.get(themeKey);
    }
    
    public synchronized Vector getPrimaryKeyList(String themeKey)
    {
	Vector result;
	try
	    {
		VersionSession versionSession=getVersionSession(themeKey);
		result=versionSession.getPrimaryKeyList();
		lostVersionSession=false;
	    }
	catch(Exception e)
	    {
		if(!lostVersionSession)
		    {
			lostVersionSession=true;
			versions.remove(themeKey);		
			result=getPrimaryKeyList(themeKey);
		    }
		else
		    {
			lostVersionSession=false;
			log(e);
			System.out.println("getPrimaryKeyList Exception:\n"+e.getMessage());
			result=new Vector();
		    }
	    }
	return result;
    }
    

    public synchronized Version createVersion(String themeKey,String project,String state,String versionName, String description)
    {
	Version version;
	try
	    {
		System.out.println("VersionClass.java: "+themeKey+" ...");
		VersionSession versionSession=getVersionSession(themeKey);
		Object versionId=versionSession.createVersion(project,versionName,state,description,"created today");
		version=Version.getVersion(themeKey,versionId);
		lostVersionSession=false;
		System.out.println("created Version:"+themeKey);
	    }
	catch(Exception e)
	    {
		if(!lostVersionSession)
		    {
			lostVersionSession=true;
			versions.remove(themeKey);		
			version=createVersion(themeKey,project,state,versionName,description);
		    }
		else
		    {
			lostVersionSession=false;
			log(e);
			System.out.println(e.getMessage());
			version=null;
		    }
	    }
	return version;
    }

    public synchronized void renameVersion(String themeKey,Object vId,String project,String state,String name,String description)
    {
	try
	    {
		VersionSession versionSession=getVersionSession(themeKey);
		versionSession.renameVersion(vId,project,name,state,description,"renamed");
		lostVersionSession=false;
		System.out.println("renamed Version:"+themeKey);
	    }
	catch(Exception e)
	    {
		if(!lostVersionSession)
		    {
			lostVersionSession=true;
			versions.remove(themeKey);		
			renameVersion(themeKey,vId,project,state,name,description);
		    }
		else
		    {
			lostVersionSession=false;
			log(e);
			System.out.println(e.getMessage());
		    }
	    }
    }

    public synchronized void copyVersion(String themeKey,Object vId,String project,String state,String name,String description)
    {
	try
	    {
		VersionSession versionSession=getVersionSession(themeKey);
		versionSession.copyVersion(vId,project,name,state,description,"a copy");
		lostVersionSession=false;
		System.out.println("copied Version:"+themeKey);
	    }
	catch(Exception e)
	    {
		if(!lostVersionSession)
		    {
			lostVersionSession=true;
			versions.remove(themeKey);		
			copyVersion(themeKey,vId,project,state,name,description);
		    }
		else
		    {
			lostVersionSession=false;
			log(e);
			System.out.println(e.getMessage());
		    }
	    }
    }

    public synchronized Version getVersion(String themeKey,Object vId)
    {
	return Version.getVersion(themeKey,vId);
    }

    public synchronized void removeVersion(String themeKey,Object vId)
    {
	try
	    {
		VersionSession versionSession=getVersionSession(themeKey);
		versionSession.removeVersion(vId);
		lostVersionSession=false;
	    }
	catch(Exception e)
	    {
		if(!lostVersionSession)
		    {
			lostVersionSession=true;
			versions.remove(themeKey);		
			removeVersion(themeKey,vId);
		    }
		else
		    {
			lostVersionSession=false;
			log(e);
			System.out.println(e.getMessage());
		    }
	    }
    }



    // Version-Properties
    public synchronized String getVersionProject(String themeKey,Object vId)
    {
	String result=null;
	try
	    {
		VersionSession versionSession=getVersionSession(themeKey);
		//		System.out.println(vId);
		result=versionSession.getVersionProject(vId);
		lostVersionSession=false;
	    }
	catch(Exception e)
	    {
		if(!lostVersionSession)
		    {
			lostVersionSession=true;
			versions.remove(themeKey);		
			result=getVersionProject(themeKey,vId);
		    }
		else
		    {
			lostVersionSession=false;
			log(e);
			System.out.println(e.getMessage());
		    }

	    }
	return result;
    }

    public synchronized String getVersionName(String themeKey,Object vId)
    {
	String result=null;
	try
	    {
		VersionSession versionSession=getVersionSession(themeKey);
		result=versionSession.getVersionName(vId);
		lostVersionSession=false;

	    }
	catch(Exception e)
	    {
		if(!lostVersionSession)
		    {
			lostVersionSession=true;
			versions.remove(themeKey);		
			result=getVersionName(themeKey,vId);
		    }
		else
		    {
			lostVersionSession=false;
			log(e);
			System.out.println(e.getMessage());
		    }
	    }
	return result;
    }
    
    public synchronized String getVersionState(String themeKey,Object vId)
    {
	String result=null;
	try
	    {	
		VersionSession versionSession=getVersionSession(themeKey);
		result=versionSession.getVersionState(vId);
		lostVersionSession=false;
	    }
	catch(Exception e)
	    {
		if(!lostVersionSession)
		    {
			lostVersionSession=true;
			versions.remove(themeKey);		
			result=getVersionState(themeKey,vId);
		    }
		else
		    {
 			lostVersionSession=false;
			log(e);
			System.out.println(e.getMessage());
		    }
	    }
	return result;
    }

    public synchronized String getVersionDescription(String themeKey,Object vId)
    {
	String result=null;
	try
	    {
		VersionSession versionSession=getVersionSession(themeKey);
		result=versionSession.getVersionDescription(vId);
		lostVersionSession=false;
	    }
	catch(Exception e)
	    {
		if(!lostVersionSession)
		    {
			lostVersionSession=true;
			versions.remove(themeKey);		
			result=getVersionDescription(themeKey,vId);
		    }
		else
		    {
			lostVersionSession=false;
			log(e);
			System.out.println(e.getMessage());
		    }
	    }
	return result;
    }

    public synchronized String getVersionHistory(String themeKey,Object vId)
    {
	String result=null;
	try
	    {
		VersionSession versionSession=getVersionSession(themeKey);
		result=versionSession.getVersionHistory(vId);
		lostVersionSession=false;
	    }
	catch(Exception e)
	    {
		if(!lostVersionSession)
		    {
			lostVersionSession=true;
			versions.remove(themeKey);		
			result=getVersionHistory(themeKey,vId);
		    }
		else
		    {
			lostVersionSession=false;
			log(e);
			System.out.println(e.getMessage());
		    }
	    }
	return result;
    }

    public synchronized void setVersionProject(String themeKey,Object vId,String project)
    {
	try
	    {	
		VersionSession versionSession=getVersionSession(themeKey);
		versionSession.setVersionProject(vId,project);
		lostVersionSession=false;
	    }
	catch(Exception e)
	    {
		if(!lostVersionSession)
		    {
			lostVersionSession=true;
			versions.remove(themeKey);		
			setVersionProject(themeKey,vId,project);
		    }
		else
		    {
			lostVersionSession=false;
			log(e);
			System.out.println(e.getMessage());
		    }
	    }
    }

    public synchronized void setVersionName(String themeKey,Object vId,String name)
    {
	try
	    {
		VersionSession versionSession=getVersionSession(themeKey);
		versionSession.setVersionName(vId,name);
		lostVersionSession=false;
	    }
	catch(Exception e)
	    {
		if(!lostVersionSession)
		    {
			lostVersionSession=true;
			versions.remove(themeKey);		
			setVersionName(themeKey,vId,name);
		    }
		else
		    {
			lostVersionSession=false;
			log(e);
			System.out.println(e.getMessage());
		    }
	    }
    }
    
    public synchronized void setVersionState(String themeKey,Object vId,String state)
    {
	try
	    {
		VersionSession versionSession=getVersionSession(themeKey);
		versionSession.setVersionState(vId,state);
		lostVersionSession=false;
	    }
	catch(Exception e)
	    {
		if(!lostVersionSession)
		    {
			lostVersionSession=true;
			versions.remove(themeKey);		
			setVersionState(themeKey,vId,state);
		    }
		else
		    {
			lostVersionSession=false;
			log(e);
			System.out.println(e.getMessage());
		    }
	    }
    }

    public synchronized void setVersionDescription(String themeKey,Object vId,String description)
    {
	try
	    {
		VersionSession versionSession=getVersionSession(themeKey);
		versionSession.setVersionDescription(vId,description);
		lostVersionSession=false;
	    }
	catch(Exception e)
	    {
		if(!lostVersionSession)
		    {
			lostVersionSession=true;
			versions.remove(themeKey);		
			setVersionDescription(themeKey,vId,description);
		    }
		else
		    {
			lostVersionSession=false;
			log(e);
			System.out.println(e.getMessage());
		    }
	    }
    }

    public synchronized void setVersionHistory(String themeKey,Object vId,String history)
    {
	try
	    {
		VersionSession versionSession=getVersionSession(themeKey);
		versionSession.setVersionHistory(vId,history);
		lostVersionSession=false;
	    }
	catch(Exception e)
	    {
		if(!lostVersionSession)
		    {
			lostVersionSession=true;
			versions.remove(themeKey);		
			setVersionHistory(themeKey,vId,history);
		    }
		else
		    {
			lostVersionSession=false;
			log(e);
			System.out.println(e.getMessage());
		    }
	    }
    }

    public String getLabel(String themeKey,Object vId)
    {
	/*
	System.out.println("ThemeKey: "+themeKey+" ID "+vId);
	try
	    {
		VersionSession versionSession=getVersionSession(themeKey);
		return versionSession.getVersionProject(vId)+"/"+versionSession.getVersionName(vId);
		lostVersionSession=false;

	    }
	catch(Exception e)
	    {
		System.out.println("getLabel Exception:\n"+e.getMessage());
		return null;
	    }
	*/
	return "label";
    }







    // Import and Export to/from Version/Xml-file
    public synchronized void importFromXml(String themeKey,Object vId,File file)
    {
	try
	    {
		VersionSession versionSession=getVersionSession(themeKey);
		//		versionSession.importFromXml(vId,file);
		String xmlString=FileSystemUtils.file2String(file);
		if(!"".equals(xmlString))
		    versionSession.importFromXml(vId,xmlString);
		lostVersionSession=false;
	    }
	catch(Exception e)
	    {
		if(!lostVersionSession)
		    {
			lostVersionSession=true;
			versions.remove(themeKey);		
			importFromXml(themeKey,vId,file);
		    }
		else
		    {
			lostVersionSession=false;
			log(e);
			System.out.println(e.getMessage());
		    }
	    }
    }

    public synchronized void exportToXml(String themeKey,Object vId,File file)
    {
 	try
	    {
		VersionSession versionSession=getVersionSession(themeKey);
		//		String content=versionSession.exportToXml(vId,file);
		String content=versionSession.exportToXml(vId);
		lostVersionSession=false;
		PrintWriter out=null;
		try
		    {
			out=new PrintWriter(new FileWriter(file));
			out.print(content);
			out.close();
		    }
		catch(IOException e)
		    {
			e.printStackTrace();
			if(out==null)
			    out.close();
		    }

	    }
	catch(Exception e)
	    {
		if(!lostVersionSession)
		    {
			lostVersionSession=true;
			versions.remove(themeKey);		
			exportToXml(themeKey,vId,file);
		    }
		else
		    {
			lostVersionSession=false;
			log(e);
			System.out.println(e.getMessage());
		    }
	    }
    }


      public synchronized void simulate(String themeKey,Object vId)
      {
	  /*
	    try
      {
      VersionSession versionSession=getVersionSession(themeKey);
      if("SimulationCase".equals(themeKey))
      {
      //		    versionSession.run("/tmp/run.sh");
      //		    versionSession.run("echo \"test:"+vId.toString()+"\"");		   
      versionSession.run("C:\\kalypso\\src\\kalypso-run\\run.bat "+vId.toString());		   
      //		    versionSession.run("/opt/jboss/xdoclet/src/kalypso-run/start "+vId.toString());		   
      
      } 
      lostVersionSession=false;
      }
      catch(Exception e)
      {
      if(!lostVersionSession)
      {
      lostVersionSession=true;
      versions.remove(themeKey);		
      simulate(themeKey,vId);
      }
      else
      {
      lostVersionSession=false;
      log(e);
      System.out.println(e.getMessage());
      }
      }
    */
      }
    //emulated ElementSession:





    












    






    public void log(Exception e)
    {
	System.out.println("LOG: "+e.getMessage());
	//	e.printStackTrace();
    }


    public synchronized Modeldata getModel(String themeKey)
    {       
	Modeldata result=null;
	if(!models.containsKey(themeKey))
	    {
		try
		    {	
			VersionSession versionSession=getVersionSession(themeKey);
			models.put(themeKey,new Modeldata(versionSession));
			lostVersionSession=false;
		    }
		catch(Exception e)
		    {
			if(!lostVersionSession)
 			    {
				lostVersionSession=true;
				versions.remove(themeKey);		
				result=getModel(themeKey);
			    }
			else
			    {
				lostVersionSession=false;
				log(e);
				System.out.println(e.getMessage());
			    }
		    }
	    }
	return (Modeldata)models.get(themeKey);
    }	

    // emulated ElementSession:

    public synchronized void remove(String themeKey,int et,Object eId)
    {
	try
	    {
		VersionSession vs=getVersionSession(themeKey);		
		vs.removeElement(et,eId);
		lostVersionSession=false;
	    }
	catch(Exception e)
	    {
		if(!lostVersionSession)
		    {
			lostVersionSession=true;
			versions.remove(themeKey);		
			remove(themeKey,et,eId);
		    }
		else
		    {
			lostVersionSession=false;
			log(e);
		    }
	    }
    }

    public synchronized Hashtable getBasePoint(String themeKey,int et,Vector eIds)
	throws ObjectNotFoundException
    {
	Hashtable result=null;
	try
	    {
		VersionSession vs=getVersionSession(themeKey);		
		result=vs.elementGetBasePoints(et,eIds);
		lostVersionSession=false;
	    }
	catch(Exception e)
	    {
		if(!lostVersionSession)
		    {
			lostVersionSession=true;
			versions.remove(themeKey);		
			result=getBasePoint(themeKey,et,eIds);
		    }
		else
		    {
			lostVersionSession=false;
			log(e);
			if(e instanceof ObjectNotFoundException)
			    throw (ObjectNotFoundException)e;
		    }
	    }
	return result;
    }

    public synchronized GisPoint getBasePoint(String themeKey,int et,Object eId)
	throws ObjectNotFoundException
    {
	GisPoint result=null;
	try
	    {
		VersionSession vs=getVersionSession(themeKey);		
		result=new GisPoint(vs.elementGetBasePoint(et,eId));
		lostVersionSession=false;
	    }
	catch(Exception e)
	    {
		if(!lostVersionSession)
		    {
			lostVersionSession=true;
			versions.remove(themeKey);		
			result=getBasePoint(themeKey,et,eId);
		    }
		else
		    {
			lostVersionSession=false;
			log(e);
			if(e instanceof ObjectNotFoundException)
			    throw (ObjectNotFoundException)e;
		    }
	    }
	return result;
    }

    public synchronized void setBasePoint(String themeKey,int et,Object eId,GisPoint basePoint)
    {
	try
	    {
		VersionSession vs=getVersionSession(themeKey);		
		vs.elementSetBasePoint(et,eId,basePoint.toBasePointTransfer());
		lostVersionSession=false;
	    }
	catch(Exception e)
	    {
		if(!lostVersionSession)
		    {
			lostVersionSession=true;
			versions.remove(themeKey);		
			setBasePoint(themeKey,et,eId,basePoint);
		    }
		else
		    {
			lostVersionSession=false;
			log(e);
		    }
	    }
    }
    
    public synchronized Vector getPrimaryKeyList(String themeKey,Object vId,int et)
	throws ObjectNotFoundException
    {
	Vector result=null;
	try
	    {
		VersionSession vs=getVersionSession(themeKey);		
		result=vs.getPrimaryKeyList(et,vId);
		lostVersionSession=false;
	    }
	catch(Exception e)
	    {
		if(!lostVersionSession)
		    {
			lostVersionSession=true;
			versions.remove(themeKey);		
			result=getPrimaryKeyList(themeKey,vId,et);
		    }
		else
		    {
			lostVersionSession=false;
			log(e);
			if(e instanceof ObjectNotFoundException)
			    throw (ObjectNotFoundException)e;

		    }
	    }
	return result;
    }

    public synchronized Vector getSimplePropertyRow(String themeKey,int et,Object eId)
	throws ObjectNotFoundException
    {
	Vector result=null;
	try
	    {
		VersionSession vs=getVersionSession(themeKey);		
		result=vs.elementGetSimplePropertyRow(et,eId);
		lostVersionSession=false;
	    }
	catch(Exception e)
	    {
		if(!lostVersionSession)
		    {
			lostVersionSession=true;
			versions.remove(themeKey);		
			result=getSimplePropertyRow(themeKey,et,eId);
		    }
		else
		    {
			lostVersionSession=false;
			log(e);
			if(e instanceof ObjectNotFoundException)
			    throw (ObjectNotFoundException)e;
		    }
	    }
	return result;
    }

    public synchronized Hashtable getSimplePropertyRows(String themeKey,int et,Vector eIds)
	throws ObjectNotFoundException
    {
	Hashtable result=null;
	try
	    {
		VersionSession vs=getVersionSession(themeKey);		
		result=vs.elementGetSimplePropertyRows(et,eIds);
		lostVersionSession=false;
	    }
	catch(Exception e)
	    {
		if(!lostVersionSession)
		    {
			lostVersionSession=true;
			versions.remove(themeKey);		
			result=getSimplePropertyRows(themeKey,et,eIds);
		    }
		else
		    {
			lostVersionSession=false;
			log(e);
			if(e instanceof ObjectNotFoundException)
			    throw (ObjectNotFoundException)e;
		    }
	    }
	return result;
    }

    public synchronized void setSimplePropertyValue(String themeKey,int et,Object eId,int pos,Object value)
    {
	try
	    {
		VersionSession vs=getVersionSession(themeKey);		
		vs.elementSetSimpleProperty(et,eId,pos,value);
		lostVersionSession=false;
	    }
	catch(Exception e)
	    {
		if(!lostVersionSession)
		    {
			lostVersionSession=true;
			versions.remove(themeKey);		
			setSimplePropertyValue(themeKey,et,eId,pos,value);
		    }
		else
		    {
			lostVersionSession=false;
			log(e);
		    }
	    }
    }

    public synchronized Vector getVectorSets(String themeKey,Object vId,int et,Object eId)
	throws ObjectNotFoundException
    {
	Vector result=null;
	try
	    {
		VersionSession vs=getVersionSession(themeKey);		
		result=vs.elementGetVectorSets(et,eId);
		lostVersionSession=false;
	    }
	catch(Exception e)
	    {
		if(!lostVersionSession)
		    {
			lostVersionSession=true;
			versions.remove(themeKey);		
			result=getVectorSets(themeKey,vId,et,eId);
		    }
		else
		    {
			lostVersionSession=false;
			log(e);
			if(e instanceof ObjectNotFoundException)
			    throw (ObjectNotFoundException)e;
		    }
	    }
	return result;
    }

    public synchronized void setVectorSet(String themeKey,int et,Object eId,int pos,Object vectorSet)
    {
	try
	    {
		VersionSession vs=getVersionSession(themeKey);		
		vs.elementSetVectorSet(et,eId,pos,vectorSet);
		lostVersionSession=false;
	    }
	catch(Exception e)
	    {
		if(!lostVersionSession)
		    {
			lostVersionSession=true;
			versions.remove(themeKey);		
			setVectorSet(themeKey,et,eId,pos,vectorSet);
		    }
		else
		    {
			lostVersionSession=false;
			log(e);
		    }
	    }
    }

    public synchronized void setVectorSets(String themeKey,int et,Object eId,Vector vectorSets)
    {
	try
	    {
		VersionSession vs=getVersionSession(themeKey);		
		vs.elementSetVectorSets(et,eId,vectorSets);
		lostVersionSession=false;
	    }
	catch(Exception e)
	    {
		if(!lostVersionSession)
		    {
			lostVersionSession=true;
			versions.remove(themeKey);		
			setVectorSets(themeKey,et,eId,vectorSets);
		    }
		else
		    {
			lostVersionSession=false;
			log(e);
		    }
	    }
    }


    //emulated RelationSession
    public synchronized void createRelation(String themeKey,Object vId,int rt,String srcKey,Object srcId,String destKey,Object destId)
    {
	try
	    {
		VersionSession vs=getVersionSession(themeKey);		
		vs.createRelation(rt,vId,srcKey,srcId,destKey,destId);
		lostVersionSession=false;
	    }
	catch(Exception e)
	    {
		if(!lostVersionSession)
		    {
			lostVersionSession=true;
			versions.remove(themeKey);		
			createRelation(themeKey,vId,rt,srcKey,srcId,destKey,destId);
		    }
		else
		    {
			lostVersionSession=false;
			log(e);
		    }
	    }
    }


    public synchronized Vector getRelationVector(String themeKey,int rt,Object rId)
	throws ObjectNotFoundException
    {
	Vector result=null;
	try
	    {
		VersionSession vs=getVersionSession(themeKey);		
		result=vs.getRelationVector(rt,rId);
		lostVersionSession=false;
	    }
	catch(Exception e)
	    {
		if(!lostVersionSession)
		    {
			lostVersionSession=true;
			versions.remove(themeKey);		
			result=getRelationVector(themeKey,rt,rId);
		    }
		else
		    {
			lostVersionSession=false;
			log(e);
			if(e instanceof ObjectNotFoundException)
			    throw (ObjectNotFoundException)e;
		    }
	    }
	return result;
    }

    
    //emulated ObjectSession
    public synchronized void objectCreate(String themeKey,Object vId,int ot)
    {
	try
	    {
		VersionSession vs=getVersionSession(themeKey);		
		vs.objectCreate(ot,vId);
		lostVersionSession=false;
	    }
	catch(Exception e)
	    {
		if(!lostVersionSession)
		    {
			lostVersionSession=true;
			versions.remove(themeKey);		
			objectCreate(themeKey,vId,ot);
		    }
		else
		    {
			lostVersionSession=false;
			log(e);
			System.out.println(e.getMessage());
		    }
	    }
    }

    //emulated ObjectSession
    public synchronized void objectCreate(String themeKey,Object vId,int ot,GisPoint basePoint)
    {
	try
	    {
		VersionSession vs=getVersionSession(themeKey);		
		vs.objectCreate(ot,vId,basePoint.toBasePointTransfer());
		lostVersionSession=false;
	    }
	catch(Exception e)
	    {
		if(!lostVersionSession)
		    {
			lostVersionSession=true;
			versions.remove(themeKey);		
			objectCreate(themeKey,vId,ot,basePoint);
		    }
		else
		    {
			lostVersionSession=false;
			log(e);
			System.out.println(e.getMessage());
		    }
	    }
    }

    public synchronized Vector returnForwardRelations(String themeKey,int ot,Object oId)
	throws ObjectNotFoundException
    {
	Vector result=null;
	try
	    {
		VersionSession vs=getVersionSession(themeKey);		
		result=vs.objectGetForwardRelations(ot,oId);
		lostVersionSession=false;
	    }
	catch(Exception e)
	    {
		if(!lostVersionSession)
		    {
			lostVersionSession=true;
			versions.remove(themeKey);		
			result=returnForwardRelations(themeKey,ot,oId);
		    }
		else
		    {
			lostVersionSession=false;
			log(e);
			if(e instanceof ObjectNotFoundException)
			    throw (ObjectNotFoundException)e;
		    }
	    }
	return result;
    }

    public synchronized Vector returnBackwardRelations(String themeKey,int ot,Object oId)
	throws ObjectNotFoundException
    {
	Vector result=null;
	try
	    {
		VersionSession vs=getVersionSession(themeKey);		
		result=vs.objectGetBackwardRelations(ot,oId);
		lostVersionSession=false;
	    }
	catch(Exception e)
	    {
		if(!lostVersionSession)
		    {
			lostVersionSession=true;
			versions.remove(themeKey);		
			result=returnBackwardRelations(themeKey,ot,oId);
		    }
		else
		    {
			lostVersionSession=false;
			log(e);
			if(e instanceof ObjectNotFoundException)
			    throw (ObjectNotFoundException)e;
		    }
	    }
	return result;
    }

    // EJBEventListener:
    public synchronized void notify(EJBEvent event)
    {
	KalypsoEventManager.getInstance().notify(event);
    }
}
