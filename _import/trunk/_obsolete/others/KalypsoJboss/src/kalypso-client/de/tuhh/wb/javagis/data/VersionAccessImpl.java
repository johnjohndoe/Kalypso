package de.tuhh.wb.javagis.data;

import java.io.File;
import java.util.Vector;

import de.tuhh.wb.javagis.Main;

public class VersionAccessImpl implements VersionAccess
{
    private VersionClass myVersionClass;
    private KeyObjectVector myIdList;
    private Vector myThemeKeys;
    //    private final static String themeKey="Kalypso";

    public VersionAccessImpl(String host,String port,String user,String pass)
    {
	this.myVersionClass=VersionClass.getInstance(host,port,user,pass);
	this.myIdList=new KeyObjectVector();
 	this.myThemeKeys=new Vector();
	updateIndex();
	info();
    }
    
    public Vector getThemeKeys()
    {
	return myThemeKeys;
    }
    
    public void info()
    {
	/*
	  for(int i=0;i<getSize();i++)
	  {
	  System.out.println(" Project: "+getProjectName(i));
	  System.out.println("   Thema: "+getThemeName(i));
	  System.out.println("   State: "+getState(i));
	  System.out.println("    Name: "+getName(i));
	  System.out.println("    Description: "+getDescription(i));
	  System.out.println("    history: "+getHistory(i)+"\n");
	  }
	*/
    }
    
    public int getSize()
    {
	return myIdList.size();
    }

    public String getProjectName(int pos)
    {
	Object vId=myIdList.getIdAt(pos);
	String themeKey=myIdList.getKeyAt(pos);
	return getProjectName(themeKey,vId);
    }
    public String getProjectName(String themeKey,Object vId)
    {
	return myVersionClass.getVersionProject(themeKey,vId);
    }

    public void   setProjectName(int pos,String project)
    {
	Object vId=myIdList.getIdAt(pos);
	String themeKey=myIdList.getKeyAt(pos);
	myVersionClass.setVersionProject(themeKey,vId,project);
    }

    public String getThemeKey(int pos)
    {
	return myIdList.getKeyAt(pos);
    }
    public String getThemeName(int pos)
    {
	return myIdList.getKeyAt(pos);
	//	Object vId=myIdList.elementAt(pos);
	//	return myVersionClass.getThemeName(themeKey);
	// ToDo
    }


    // no setter

    public String getState(int pos)
    {
	Object vId=myIdList.getIdAt(pos);
	String themeKey=myIdList.getKeyAt(pos);
	return getState(themeKey,vId);
    }
    public String getState(String themeKey,Object vId)
    {
	return myVersionClass.getVersionState(themeKey,vId);
    }

    public void   setState(int pos,String state)
    {
	Object vId=myIdList.getIdAt(pos);
	String themeKey=myIdList.getKeyAt(pos);
	myVersionClass.setVersionState(themeKey,vId,state);
    }

    public String getName(int pos)
    {
	Object vId=myIdList.getIdAt(pos);
	String themeKey=myIdList.getKeyAt(pos);
	return myVersionClass.getVersionName(themeKey,vId)+" (#"+vId.toString()+")";
    }
    public String getName(String themeKey,Object vId)
    {
	return myVersionClass.getVersionName(themeKey,vId);
    }
    public String getFullName(String themeKey,Object vId)
    {
	return 
	    getProjectName(themeKey,vId)+"-"+
	    themeKey+"-"+
	    getState(themeKey,vId)+"-"+
	    getName(themeKey,vId);
    }

    public void   setName(int pos,String name)
    {
	Object vId=myIdList.getIdAt(pos);
	String themeKey=myIdList.getKeyAt(pos);
	myVersionClass.setVersionName(themeKey,vId,name);
    }

    public String getDescription(int pos)
    {
	Object vId=myIdList.getIdAt(pos);
	String themeKey=myIdList.getKeyAt(pos);
	return getDescription(themeKey,vId);
    }
    public String getDescription(String themeKey,Object vId)
    {
	return myVersionClass.getVersionDescription(themeKey,vId);
    }


    public void   setDescription(int pos,String description)
    {
	Object vId=myIdList.getIdAt(pos);
	String themeKey=myIdList.getKeyAt(pos);
	myVersionClass.setVersionDescription(themeKey,vId,description);
    }

    public String getHistory(int pos)
    {
	Object vId=myIdList.getIdAt(pos);
	String themeKey=myIdList.getKeyAt(pos);
	return myVersionClass.getVersionHistory(themeKey,vId);
    }

    // no set

    public void updateIndex()
    {
	this.myIdList.clear();
	this.myThemeKeys=myVersionClass.getThemeKeys();

	for(int i=0;i<myThemeKeys.size();i++)
	    {
		String themeKey=(String)myThemeKeys.elementAt(i);
		Vector idList=myVersionClass.getPrimaryKeyList(themeKey);
		this.myIdList.addAll(themeKey,idList);
	    }
	info();
    }
    

    public void createVersion(String project,String themeKey,String state,String name, String description)
    {
	myVersionClass.createVersion(themeKey, project, state, name, description);
    }

    public void createExtendedVersion(int pos,String project,String themeKey,String state,String name, String description)
    {
	// ToDo extendVersion...
	if(myThemeKeys.contains(themeKey))
	    myVersionClass.createVersion(themeKey, project, state, name, description);
	else
	    System.out.println("sorry, theme \""+themeKey+"\" is not available");
    }

    public void renameVersion(String themeKey,Object vId,String project,String state,String name,String description)
    {
	myVersionClass.renameVersion(themeKey,vId,project, state, name, description);
    }

    public void copyVersion(String themeKey,Object vId,String project,String state,String name,String description)
    {
	myVersionClass.copyVersion(themeKey,vId,project, state, name, description);
    }

    public void removeVersion(int pos)
    {
	Object vId=myIdList.getIdAt(pos);
	String themeKey=myIdList.getKeyAt(pos);
	myVersionClass.removeVersion(themeKey,vId);
    }

    /*
      public void openVersion(int pos)
    {
	Object vId=myIdList.getIdAt(pos);
	String themeKey=myIdList.getKeyAt(pos);
	Version version=myVersionClass.getVersion(themeKey,vId);

	//	Main.viewManager.showTableView(version);
	//	Main.viewManager.showNetView(version);
    }
    */
    public void openObjectTableView(int pos)
    {
	Object vId=myIdList.getIdAt(pos);
	String themeKey=myIdList.getKeyAt(pos);
	Version version=myVersionClass.getVersion(themeKey,vId);
	//	System.out.println("Version:"+version.toString());
	//	System.out.println("Viemanager:"+Main.viewManager);
	Main.viewManager.showObjectTableView(version);
    }
    
    public void openRelationTableView(int pos)
    {
	Object vId=myIdList.getIdAt(pos);
	String themeKey=myIdList.getKeyAt(pos);
	Version version=myVersionClass.getVersion(themeKey,vId);
	//	System.out.println("Version:"+version.toString());
	//	System.out.println("Viemanager:"+Main.viewManager);
	
	Main.viewManager.showRelationTableView(version);
    }

    public Version getVersion(int pos)
    {
	Object vId=myIdList.getIdAt(pos);
	String themeKey=myIdList.getKeyAt(pos);
	return myVersionClass.getVersion(themeKey,vId);
    }

    public Object getVersionId(int pos)
    {
	return myIdList.getIdAt(pos);
    }

    public void openNetView(int pos)
    {
	Object vId=myIdList.getIdAt(pos);
	String themeKey=myIdList.getKeyAt(pos);

	Version version=myVersionClass.getVersion(themeKey,vId);
	Main.viewManager.showNetView(version);
    }
    
    public void xmlImport(String themeKey,Object vId,File importFile)
    {
	myVersionClass.importFromXml(themeKey,vId,importFile);
    }

    public void xmlExport(String themeKey,Object vId,File exportFile)
    {
	myVersionClass.exportToXml(themeKey,vId,exportFile);	
    }

    public void simulate(int pos)
    {
	Object vId=myIdList.getIdAt(pos);
	String themeKey=myIdList.getKeyAt(pos);
	myVersionClass.simulate(themeKey,vId);
    }

}
