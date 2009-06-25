package de.tuhh.wb.javagis.model;

import org.xml.sax.Attributes;
import org.xml.sax.helpers.AttributesImpl;
import java.util.Hashtable;

public class GisTransferObject
{
    private static final String uri="";
    private AttributesImpl simpleProperties;
    private Hashtable vectorSets;
    private Hashtable chartProperties;
    private String identifier;  
    private String tableName;
    private boolean isIdSynchron; // use the same IDs in DataBase and XML-File ? (yes=true)

    public GisTransferObject(String tableName,String identifier, boolean isIdSynchron)
    {
	this.tableName=tableName;
	this.identifier=identifier;
	this.isIdSynchron=isIdSynchron;
	this.chartProperties=new Hashtable();
	this.vectorSets=new Hashtable();
    }

    public void setSimpleProperties(Attributes simpleProperties)
    {
	this.simpleProperties=new AttributesImpl(simpleProperties);
    }
    
    public void addSimpleProperty(String propKey,String propValue)
    {
	String localName=propKey;
	String qName="";
	String type="xsi:string";
	String value=propValue;
	simpleProperties.addAttribute(uri,localName,qName,type,value);
    }

    public String getSimpleProperty(String simplePropKey)
    {
	String localName=simplePropKey;
	return simpleProperties.getValue(uri,localName);
    }

    public String getTableName()
    {
	return tableName;
    }

    public void info()
    {

	System.out.println("GisTransferObject:");
	System.out.print("table: "+tableName);
	if(isIdSynchron)
	    System.out.println(" ID:  #"+identifier);
	else
	    System.out.println(" NO:  #"+identifier);
	System.out.println("SimpleProperties:");
	for(int i=0;i<simpleProperties.getLength();i++)
	    {
		System.out.print("  "+simpleProperties.getLocalName(i)+" = ");
		System.out.println("\""+simpleProperties.getValue(i)+"\"");
	    }
    }
}
