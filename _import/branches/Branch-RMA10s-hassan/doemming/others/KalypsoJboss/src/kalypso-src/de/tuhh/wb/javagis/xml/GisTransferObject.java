package de.tuhh.wb.javagis.xml;

import de.tuhh.wb.javagis.model.Tools;
import org.xml.sax.Attributes;
import org.xml.sax.helpers.AttributesImpl;
import java.util.Hashtable;
import java.util.Vector;
import java.util.Date;
import java.util.Enumeration;
import java.util.GregorianCalendar;
import java.text.DateFormat;
import java.text.SimpleDateFormat;

public class GisTransferObject
{
    public static final String uri="";
    private AttributesImpl simpleProperties;
    private AttributesImpl basePointProperties;
    private Hashtable vectorSets;
    private String identifier;  
    private String tableName;
    private boolean isIdSynchron; // use the same IDs in DataBase and XML-File ? (yes=true)
    private boolean isRelation;
    private String srcTable;
    private String destTable;
    private String srcIdentifier;
    private String destIdentifier;

    public GisTransferObject(String tableName,String identifier, boolean isIdSynchron)
    {
	this.isRelation=false;
	this.srcTable=null;
	this.destTable=null;
	this.srcIdentifier=null;
	this.destIdentifier=null;
	this.simpleProperties=new AttributesImpl();
	this.basePointProperties=new AttributesImpl();
	this.tableName=tableName;
	this.identifier=identifier;
	this.isIdSynchron=isIdSynchron;
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
    public void addSimpleProperty(String propKey,boolean propValue)
    {
	addSimpleProperty(propKey,Boolean.toString(propValue));
    }
    public void setBasePoint(Attributes basePointProperties)
    {
	this.basePointProperties=new AttributesImpl(basePointProperties);
    }

    public void addBasePoint(String propKey,String propValue)
    {
	String localName=propKey;
	String qName="";
	String type="xsi:string";
	String value=propValue;
	basePointProperties.addAttribute(uri,localName,qName,type,value);
    }

    public void setRelation(Attributes relationProperties)
    {
	this.isRelation=true;
	this.srcTable=relationProperties.getValue(uri,"srcKey");
	this.destTable=relationProperties.getValue(uri,"destKey");
	this.srcIdentifier=relationProperties.getValue(uri,"srcID");
	this.destIdentifier=relationProperties.getValue(uri,"destID");
    }
    public void setRelationSrcTable(String value)
    {
	this.srcTable=value;
	this.isRelation=true;
    }
    public void setRelationDestTable(String value)
    {
	this.destTable=value;
	this.isRelation=true;
    }
    public void setRelationSrcIdentifier(String value)
    {
	this.srcIdentifier=value;
	this.isRelation=true;
    }
    public void setRelationDestIdentifier(String value)
    {
	this.destIdentifier=value;
	this.isRelation=true;
    }

    public String getTableName()
    {
	return tableName;
    }

    public String getIdentifier()
    {
	return identifier;
    }

    public String getSimpleProperty(String simplePropKey)
    {
	if(simpleProperties==null)
	    return null;
 	else
	    {
		String localName=simplePropKey;
		return simpleProperties.getValue(uri,localName);
	    }
    }

    public String getBasePointProperty(String propKey)
    {
	if(basePointProperties==null)
	    return null;
	else
	    {
		String localName=propKey;
		return basePointProperties.getValue(uri,localName);
	    }
    }

    
    public Date getSimplePropertyAsDate(String simplePropKey)
    {
	try
	    {
		GregorianCalendar calendar=new java.util.GregorianCalendar();	
		calendar.setTimeInMillis(java.lang.Long.parseLong(getSimpleProperty(simplePropKey)));
		return calendar.getTime();
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
		return null;
	    }
    }
    
    public boolean getSimplePropertyAsBoolean(String simplePropKey)
    {
	String value=getSimpleProperty(simplePropKey);
	if(value!=null)
	    return (new Boolean(value)).booleanValue();
	else
	    return false;
    }

    public String getSimplePropertyFormatedDate(String simplePropKey,String datePattern)
    {
	DateFormat dateFormat=new SimpleDateFormat(datePattern);
	return dateFormat.format(getSimplePropertyAsDate(simplePropKey));	
    }

    public int getSimplePropertySize()
    {
	if(simpleProperties==null)
	    return 0;
	else
	    return simpleProperties.getLength();
    }

    public Vector getSimplePropertyKeys()
    {
	Vector result=new Vector();
	for(int i=0;i<getSimplePropertySize();i++)
	    {
		result.add(simpleProperties.getLocalName(i));
	    }
	return result;
    }
    
    public boolean isRelation()
    {
	return this.isRelation;
    }

    public String getRelationSrcTable()
    {
	return this.srcTable;
    }
    public String getRelationDestTable()
    {
	return this.destTable;
    }
    public String getRelationSrcIdentifier()
    {
	return this.srcIdentifier;
    }
    public String getRelationDestIdentifier()
    {
	return this.destIdentifier;
    }

    public void addVectorSet(VectorSet vectorSet)
    {
	//	System.out.println("parsing: gisTransferObject.addVectorSet");
	String vectorSetKey=vectorSet.getKey();
	vectorSets.put(vectorSetKey,vectorSet);
    }

    public Vector getVectorSetKeys()
    {
	Vector result=new Vector();
	for(Enumeration e = vectorSets.keys() ; e.hasMoreElements() ;)
	    {
		result.add(e.nextElement());
	    }
	return result;
    }
    
    public VectorSet getVectorSet(String key)
    {
	return (VectorSet)vectorSets.get(key);
    }

    public String toXmlString()
    {
	StringBuffer xml=new StringBuffer();
	xml.append("<table key=\""+tableName+"\">");
	xml.append("<o ID=\""+identifier+"\">");

	xml.append(Tools.genXmlTag("bp",basePointProperties));
	xml.append(Tools.genXmlTag("sp",simpleProperties));

	//VectorSets:
	for (Enumeration e = vectorSets.elements() ; e.hasMoreElements() ;)
	    {
		VectorSet v=(VectorSet)e.nextElement();
		xml.append(v.toXmlString());
	    }
	if(isRelation)
	    {
		xml.append("<rel ");
		xml.append("srcKey=\""+getRelationSrcTable()+"\" ");
		xml.append("srcID=\""+getRelationSrcIdentifier()+"\" ");
		xml.append("destKey=\""+getRelationDestTable()+"\" ");
		xml.append("destID=\""+getRelationDestIdentifier()+"\"");
		xml.append("/>");
	    }
	xml.append("</o>");
	xml.append("</table>");
	return xml.toString();
    }
}
