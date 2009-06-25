package de.tuhh.wb.javagis.data;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.Hashtable;
import java.util.Vector;
import java.util.Enumeration;
import java.util.List;
import java.io.Serializable;
import de.tuhh.wb.javagis.model.VersionSession;

public class Modeldata implements Serializable
{    
    public String[] elementKeys;
    public String[] objectKeys;
    public String[] relationKeys;
    public int elementSize;
    public int objectSize;
    public int relationSize;

    public String[] elementNames;
    public String[] elementSymbolNames;
    public String[] elementDescriptions;


    public String[][] simplePropertyKeys;
    public String[][] simplePropertyNames;
    public String[][] simplePropertyDescriptions;
    public String[][] simplePropertyUnits;
    public Class[][] simplePropertyClasses;
    public String[][] simplePropertyFormats;

    public String[][] vectorSetKeys;
    public String[][] vectorSetNames;
    public String[][] vectorSetDescriptions;

    //relations:
    public boolean[] isRelation;
    public String[] relationForwardLabels;
    public String[] relationBackwardLabels;

    public String[][] relationAllowedSrcKeys;
    public String[][] relationAllowedDestKeys;
    //relations from object-view:
    public String[][] objectAllowedForwardKeys;
    public String[][] objectAllowedBackwardKeys;

    public Modeldata(VersionSession vs)
    {
	try
	    {
		this.elementKeys  = vs.getElementKeys();
		this.objectKeys   = vs.getObjectKeys();
		this.relationKeys = vs.getRelationKeys();
		
		this.elementSize  =  elementKeys.length;
		this.objectSize   =   objectKeys.length;
		this.relationSize = relationKeys.length;
		
		this.elementNames=vs.getElementNames();
		this.elementSymbolNames=vs.getElementSymbolNames();
		this.elementDescriptions=vs.getElementDescriptions();
		
		this.simplePropertyKeys=new String[elementSize][];
		this.simplePropertyNames=new String[elementSize][];
		this.simplePropertyDescriptions=new String[elementSize][];
		this.simplePropertyUnits=new String[elementSize][];
		this.simplePropertyClasses=new Class[elementSize][];
		this.simplePropertyFormats=new String[elementSize][];

		this.vectorSetKeys=new String[elementSize][];
		this.vectorSetNames=new String[elementSize][];
		this.vectorSetDescriptions=new String[elementSize][];
		
		for(int table=0;table<elementSize;table++)
		    {
			this.simplePropertyKeys[table]=vs.elementGetSimplePropertyKeys(table);
			this.simplePropertyNames[table]=vs.elementGetSimplePropertyNames(table);
			this.simplePropertyDescriptions[table]=vs.elementGetSimplePropertyDescriptions(table);
			this.simplePropertyUnits[table]=vs.elementGetSimplePropertyUnits(table);
			this.simplePropertyClasses[table]=vs.elementGetSimplePropertyClasses(table);
			this.simplePropertyFormats[table]=vs.elementGetSimplePropertyFormats(table);
			
			this.vectorSetKeys[table]=vs.elementGetVectorSetKeys(table);
			this.vectorSetNames[table]=vs.elementGetVectorSetNames(table);
			this.vectorSetDescriptions[table]=vs.elementGetVectorSetDescriptions(table);
		    }
		
		//Relations
		this.isRelation=vs.elementIsRelation();
		this.relationForwardLabels=vs.relationGetForwardLabels();
		this.relationBackwardLabels=vs.relationGetBackwardLabels();
		
		this.relationAllowedSrcKeys=new String[relationSize][];
		this.relationAllowedDestKeys=new String[relationSize][];
		
		for(int table=0;table<relationSize;table++)
		    {
			this.relationAllowedSrcKeys[table]=vs.relationGetSourceKeys(table);
			this.relationAllowedDestKeys[table]=vs.relationGetDestinationKeys(table);
		    }
		
		// relation from object-view:
		this.objectAllowedForwardKeys=new String[objectSize][];
		this.objectAllowedBackwardKeys=new String[objectSize][];
		for(int table=0;table<objectSize;table++)
		    {
			this.objectAllowedForwardKeys[table]=vs.objectForwardRelations(table);
			this.objectAllowedForwardKeys[table]=vs.objectBackwardRelations(table);
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    public int getElementTable(String elementKey)
    {
	for(int i=0;i<elementSize;i++)
	    if(elementKey.equals(elementKeys[i]))
		return i;
	System.out.println("ERROR: Key \""+elementKey+"\" is not an elementKey :-(");
	System.exit(0);
	return 0;
    }

    public int getObjectTable(String objectKey)
    {
	for(int i=0;i<objectSize;i++)
	    if(objectKey.equals(objectKeys[i]))
		return i;
	System.out.println("ERROR: Key \""+objectKey+"\" is not an oibjectKey :-(");
	System.exit(0);
	return 0;
    }

    public int getRelationTable(String relationKey)
    {
	for(int i=0;i<relationSize;i++)
	    if(relationKey.equals(relationKeys[i]))
		return i;
	System.out.println("ERROR: Key \""+relationKey+"\" is not a relationKey :-(");
	System.exit(0);
	return 0;
    }

    public void info()
    {
	for(int i=0;i<elementSize;i++)
	    printElement(i);

	System.out.println("ModelInfo:");
	print("ElementKeys:",elementKeys);
	
	System.out.println("");
    }


    public void printElement(int pos)
    {
	System.out.println("\n################################");
	System.out.println("# "+elementNames[pos]+" ("+elementKeys[pos]+")");
	System.out.println("SymbolName :"+elementSymbolNames[pos]);
	System.out.println("Description :"+elementDescriptions[pos]);
	printProperties(pos);
    }
    public void printProperties(int pos)
    {
	System.out.println("------------------SimpleProperties:");
	int simplePropSize=simplePropertyKeys[pos].length;
	for(int i=0;i<simplePropSize;i++)
	    {
		System.out.println("\n"+simplePropertyNames[pos][i]+" ("+simplePropertyKeys[pos][i]+")");
		System.out.println("Class: "+simplePropertyClasses[pos][i]);
		System.out.println("Format: "+simplePropertyFormats[pos][i]);
		System.out.println("Description: "+simplePropertyDescriptions[pos][i]);
	    }
	System.out.println("-------------------VectorSets:");
	int vectorSetSize=vectorSetKeys[pos].length;
	for(int i=0;i<vectorSetSize;i++)
	    {
		System.out.println("\n"+vectorSetNames[pos][i]+" ("+vectorSetKeys[pos][i]+")");
		System.out.println("Description: "+vectorSetDescriptions[pos][i]);
	    }
    }


    public void print(String text,Object[][] values)
    {
	System.out.println(text);
	for(int i=0;i<values.length;i++)
	    print( String.valueOf(i),values[i]);
    }

    public void print(String text,Object[] values)
    {
	System.out.print(text);
	for(int i=0;i<values.length;i++)
	    System.out.print(" "+values[i].toString());
	System.out.println();
    }
}
