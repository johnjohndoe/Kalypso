package de.tuhh.wb.javagis.view.tableview;

import javax.swing.table.AbstractTableModel;

//import de.tuhh.wb.javagis.model.ElementSession;
import java.util.Hashtable;
import java.util.Vector;
import java.util.Enumeration;
import de.tuhh.wb.javagis.view.GisView;
import de.tuhh.wb.javagis.tools.I18n;

import de.tuhh.wb.javagis.data.*;
import de.tuhh.wb.javagis.data.event.ElementClassListener;
import javax.swing.JButton;
import de.tuhh.wb.javagis.model.GisInterfaceTableModel;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.text.DateFormat;
import java.util.Date;
import java.util.Comparator;

import javax.ejb.ObjectNotFoundException;
import javax.swing.*;

public class GisTableModel extends AbstractTableModel implements GisInterfaceTableModel, ElementClassListener
{
    private Vector myIdList;
    private GisElementClass myGisElementClass;
    private GisView myGisView;
    
    public Icon getIcon()
    {
	return myGisElementClass.getIcon();
    }

    public GisTableModel(GisElementClass gisElementClass)
    {
	super();
	this.myGisElementClass=gisElementClass;
	try
	    {
		this.myIdList=gisElementClass.getAllPrimaryKeys();
		/*
		  System.out.println("-- preloadingSimplePropertyValues for ..."+getName());
		  gisElementClass.preLoadSimplePropertyValues(myIdList);
		  System.out.println("   ... finished");
		*/
	    }
	catch(ObjectNotFoundException e)
	    {
		this.myIdList=new Vector();
	    }
	this.myGisView=null;
	myGisElementClass.addElementClassListener(this);
    }

    public void close()
    {
	myGisElementClass.removeElementClassListener(this);
    }
    
    public void setGisView(GisView gisView)
    {
	this.myGisView=gisView;
    }

    public void showAllElements()
    {
	try
	    {
		this.myIdList=myGisElementClass.getAllPrimaryKeys();
	    }
	catch(ObjectNotFoundException e)
	    {
		this.myIdList=new Vector();
	    }
	if(myGisView!=null)
	    myGisView.refreshView();
    }
    
    public void setAllSimplePropertiesTo(int col,Object value)
    {
	for(int row=0;row<getRowCount();row++)
	    {
		setValueAt(value,row,col);
	    }
    }
    
    public void filter(int col,Object value,boolean selectFromAll,int filterType)
    {
	try
	    {
		Vector idList=null;
		Vector filteredList=new Vector();
		if(selectFromAll)
		    idList=new Vector(myGisElementClass.getAllPrimaryKeys());
		else
		    idList=new Vector(myIdList);
		ByPropertyComparator comparator=new ByPropertyComparator(myGisElementClass,col,value,filterType);
		Object id=null;
		
		for(int i=0;i<idList.size();i++)
		    {
			id=idList.elementAt(i);
			if(comparator.filter(id))
			    {
				System.out.println("true");
				filteredList.add(id);
		    }
			else
			    System.out.println("false");
		    }
		this.myIdList=filteredList;
		if(myGisView!=null)
		    myGisView.refreshView();
	    }
	catch(ObjectNotFoundException e)
	    {
		//
	    }
    }
    
    public void orderColumnBy(int col,boolean reverse)
    {
	System.out.println("order by ID");
	{
	    try
		{
		    Comparator comparator=new ByPropertyComparator(myGisElementClass,col);
		    java.util.Collections.sort(myIdList,comparator);
		    if(reverse)
			java.util.Collections.reverse(myIdList);
		    if(myGisView!=null)
			myGisView.refreshView();
		}
	    catch(Exception e)
		{
		    System.out.println(e.getMessage());
		}
	}
    }
    
    public void onTableElementCreate(int elementTable,Object eId)
    {
	if(!myIdList.contains(eId))
	    {
		myIdList.add(0,eId);
		System.out.println("need update for view...");
		if(myGisView!=null)
		    myGisView.refreshView();
	    }
    }

    public void onTableElementRemove(int elementTable,Object eId)
    {
	if(myIdList.remove(eId))
	    {
		System.out.println("need update for view...");
		if(myGisView!=null)
		    myGisView.refreshView();
	    }
    }
    public void onSimplePropertyChanged(int elementTable,Object eId)
    {
	if(myGisView!=null)
	    myGisView.refreshView();
    }

    public int getColumnCount()
    {
	 return myGisElementClass.getSimplePropertySize()+1;
    }

    public int getRowCount()
    {
	return myIdList.size();
    }

    public String getColKey(int col)
    {
	if(col==0)
	    return "ID";
	return myGisElementClass.getSimplePropertyKey(col-1);
    }


    public String getColumnName(int col)
    {
	if(col==0)
	    return "<html>#ID<br></html>";
	String unit=myGisElementClass.getSimplePropertyUnit(col-1);
	if(unit!=null && !"".equals(unit))
	    return new String("<html>"+myGisElementClass.getSimplePropertyName(col-1)+
			      " ["+unit+"]<br></html>");
	else
	    return new String("<html>"+myGisElementClass.getSimplePropertyName(col-1)+
			      "<br></html>");
    }

    public String getColumnNameNoHtml(int col)
    {
	if(col==0)
	    return "#ID";
	return myGisElementClass.getSimplePropertyName(col-1);
    }

    public String getDescription(int col)
    {
	if(col==0)
	    return I18n.get("GTM_primaryKey");
	return new String(myGisElementClass.getSimplePropertyDescription(col-1));

    }

    public Class getColumnClass(int col)
    {
	if(col==0)
	    return Integer.class;
	Class propClass=myGisElementClass.getSimplePropertyClass(col-1);
		/*if(propClass==java.util.Date.class)
		 return String.class;*/
	/*
	  else if("bce_db".equals(myGisElementClass.getSimplePropertyFormat(col-1)))
	  return javax.swing.JButton.class;
	*/
		//else
	    return propClass;
    }

    public boolean isBCEButton(int col)
    {
	if("bce_db".equals(myGisElementClass.getSimplePropertyFormat(col-1)))
	    return true;
	return false;
    }
	
	public boolean isDateButton(int col)
	{
		Class propClass=myGisElementClass.getSimplePropertyClass(col-1);
		if(propClass==java.util.Date.class)
			return true;
		return false;
	}

    public boolean isCellEditable(int row,int col)
    {
	if(col==0)
	    return false;
	else
	    return true;
    }

    public Object getId(int row)
    {
	return myIdList.elementAt(row);
    }

    public Object getValueAt(int row,int col)
    {
	Object id=myIdList.elementAt(row);
	if(col==0)
	    return id;
	else
	    {
		try
		    {
			return myGisElementClass.getSimplePropertyValue(id,col-1);
		    }
		catch(ObjectNotFoundException e)
		    {
			return null;
		    }
		/*if(value instanceof java.util.Date)
		  {
		  DateFormat dateFormat=new SimpleDateFormat(myGisElementClass.getSimplePropertyFormat(col-1));
		  return dateFormat.format((Date)value);
		  }*/
		
		/*
		  else if("bce_db".equals(myGisElementClass.getSimplePropertyFormat(col-1)))
		  {
		  if(value==null)
		  {
		  return new JButton("select sequence");
		  }
		  else
		  {
		*/
		/*
		  int trim=text.indexOf(",");
		  String label=;
		  if(trim>=0)
		  label=text.substring(0,trim);
		*/
		/*
		  return new JButton((String)value);
		  }
		  }
		*/

	    }
    }

    public void setValueAt(Object value,int row,int col)
    {
	Object id=myIdList.elementAt(row);
	if(col==0)
	    return;
	else
	    {
			/*if(myGisElementClass.getSimplePropertyClass(col-1)==Date.class && value instanceof String)
		{
		    try
			{
			    DateFormat dateFormat=new SimpleDateFormat(myGisElementClass.getSimplePropertyFormat(col-1));
			    Date date=dateFormat.parse((String)value);
			    myGisElementClass.setSimplePropertyValue(id,col-1,date);
			}
		    catch(ParseException e)
			{
			    System.out.println("wrong DateFormat, couldn't parse");
			}
			 }*/
		/*
		  else if("bce_db".equals(myGisElementClass.getSimplePropertyFormat(col-1)))
		  {
		  System.out.println("bce_db_setValueAt....?");
		  }
		*/
		//else

		    myGisElementClass.setSimplePropertyValue(id,col-1,value);
	    }
    }
    
    public String getDescription()
    {
	return myGisElementClass.getDescription();
    }

    public String getName()
    {
	return myGisElementClass.getName();
    }
    public String getKey()
    {
	return myGisElementClass.getKey();
    }

    // am Anfang der Liste neues Object erzeugen
    public void createNewObject()
    {
	if(myGisElementClass instanceof GisObjectClass)
	    {
		GisObjectClass myGisObjectClass=(GisObjectClass)myGisElementClass;
		myGisObjectClass.createObject();
	    }
	else
	    System.out.println("create Relations not in TableView, create it in NetView please");
    }
    
    // am Ende der Liste neues Object erzeugen
    public void appendNewObject()
    {
	if(myGisElementClass instanceof GisObjectClass)
	    {
		GisObjectClass myGisObjectClass=(GisObjectClass)myGisElementClass;
 		myGisObjectClass.createObject();
	    }
	else
	    System.out.println("create Relations not in TableView, create it in NetView please");
    }

    public void insertNewObjectAt(int position)
    {
	if(myGisElementClass instanceof GisObjectClass)
	    {
		GisObjectClass myGisObjectClass=(GisObjectClass)myGisElementClass;
		myGisObjectClass.createObject();
	    }
	else
	    System.out.println("create Relations not in TableView, create it in NetView please");
    }


    public void removeObject(int row)
    {
	if(myGisElementClass instanceof GisObjectClass)
	    ((GisObjectClass)myGisElementClass).remove(myIdList.elementAt(row));
    }

    // von "startIndex" an soundsoviele Objecte loeschen
    public void removeObjects(int startIndex, int number)
    {
	//number:0 -> nix passiert
	//number:1 -> ein Element wird gelöscht
	//number:n -> n Elemente werden gelöscht
	/*
	  Vector toDelete=new Vector();
	  for(int i=startIndex;i<startIndex+number;i++)
	  toDelete.add(primaryKeyList.elementAt(i));
	  for(int i=0;i<toDelete.size();i++)
	  primaryKeyList.removeElement(toDelete.elementAt(i));
	*/
    }

    public boolean hasVectorSets()
    {
	return (myGisElementClass.getVectorSetSize()>0);
    }

    public void setPropertyInVectorSet(int selectedTab,int selectedRow,int selectedCol,Object selectedValue)
    {
	Vector models=null;
	GisInterfaceTableModel vtable=null;
	for(int row=0;row<getRowCount();row++)
	    {
		models=getVectorSetTableModels(row);
		vtable=(GisInterfaceTableModel)models.elementAt(selectedTab);
		vtable.setValueAt(selectedValue,selectedRow,selectedCol);
		myGisElementClass.setVectorSets(myIdList.elementAt(row),models);
	    }
    }

    public Vector getVectorSetTableModels(int row)
    {
	try
	    {
		Object id=myIdList.elementAt(row);
		return myGisElementClass.getVectorSets(id);
	    }
	catch(ObjectNotFoundException e)
	    {
		return new Vector();
	    }
    }

    public Object getElementId(int row)
    {
	return myIdList.elementAt(row);
    }

    public GisElement getGisElement(int row)
    {
	Object eId= myIdList.elementAt(row);
	return myGisElementClass.getGisElement(eId);
    }

    public GisElementClass getGisElementClass()
    {
	return myGisElementClass;
    }

    /*
      public void storeVectorSets(Object oId,Vector vectorSets)
      {
      System.out.println("GisTableModel.storeVectorSets ..."+getName());
      myGisElementClass.setVectorSets(oId,vectorSets);
      }
    */
}
