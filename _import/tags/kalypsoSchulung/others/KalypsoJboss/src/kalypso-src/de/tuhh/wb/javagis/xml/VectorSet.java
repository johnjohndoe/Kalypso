package de.tuhh.wb.javagis.xml;

import org.xml.sax.Attributes;
import org.xml.sax.helpers.AttributesImpl;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Vector;
import de.tuhh.wb.javagis.model.Tools;

public class VectorSet
{
    public static final String uri="";
    String myKey;
    private Vector rows;
    private Hashtable agregatedVectorSets;
    public VectorSet(Attributes atts)
    {
	this.myKey=atts.getValue(GisTransferObject.uri,"key");
	this.rows=new Vector();
	this.agregatedVectorSets=new Hashtable();
    }
    
    public VectorSet(String key)
    {
	this.myKey=key;
	this.rows=new Vector();
	this.agregatedVectorSets=new Hashtable();
    }

    public String getKey()
    {
	return myKey;
    }

    public void addRow(Attributes atts)
    {
	//	System.out.println("parsing: VectorSet\""+myKey+"\".addRow()  size:"+atts.getLength());
	rows.add(new AttributesImpl(atts));
    }

    public void addRow(String[] attributeName,Object[] attribute)
    {
	AttributesImpl atts=new AttributesImpl();
	String qName="";
	String type="xsi:string";

	for(int i=0;i<attributeName.length;i++)
	    if(attribute[i]!=null)
		{
		    String localName=attributeName[i];
		    String value=attribute[i].toString();
		    atts.addAttribute(uri,localName,qName,type,value);
		}
	addRow(atts);
    }
    
    public Vector getSimplePropertyKeys(int row)
    {
	Vector result=new Vector();
	Attributes simpleProps=(Attributes)rows.elementAt(row);
	for(int i=0;i<simpleProps.getLength();i++)
	    {
		String key=simpleProps.getLocalName(i);
		result.add(key);
	    }
	return result;
    }

    public String getSimpleProperty(String simplePropKey,int row)
    {
	Attributes simpleProps=(Attributes)rows.elementAt(row);
	String localName=simplePropKey;
	return simpleProps.getValue(GisTransferObject.uri,localName);
    }

    public boolean getSimplePropertyAsBoolean(String simplePropKey,int row)
    {
	String value=getSimpleProperty(simplePropKey,row);
	if(value!=null)
	    return (new Boolean(value)).booleanValue();
	else
	    return true;
    }

    public AttributesImpl getSimpleProperties(int row)
    {
	return new AttributesImpl((Attributes)rows.elementAt(row));
    }

    public void addVectorSet(VectorSet agregatedVectorSet)
    {
	String agregKey=agregatedVectorSet.getKey();
	if(!agregatedVectorSets.containsKey(agregKey))
	    agregatedVectorSets.put(agregKey,new Vector());
	Vector agregVector=(Vector)agregatedVectorSets.get(agregKey);
  	agregVector.add(agregatedVectorSet);
    }

    public Vector getVectorSetKeys()
    {
	Vector result=new Vector();
	for(Enumeration e = agregatedVectorSets.keys() ; e.hasMoreElements() ;)
	    {
		result.add(e.nextElement());
	    }
	return result;
    }

    public VectorSet getVectorSet(String key,int row)
    {
	Vector vectorSets=(Vector)agregatedVectorSets.get(key);
	return (VectorSet)vectorSets.elementAt(row);
    }

    public boolean hasVectorSets()
    {
	return !agregatedVectorSets.isEmpty();
    }
    public int size()
    {
	return rows.size();
    }
    
    public void info()
    {
	for(int row=0;row<rows.size();row++)
	    {
		System.out.println("Row #"+row);
		Vector keys=getSimplePropertyKeys(row);
		for(int i=0;i<keys.size();i++)
		    {
			String key=(String)keys.elementAt(i);
			System.out.println("SimpleProperty: "+key+"="+getSimpleProperty(key,row));
		    }
	    }
    }

    public String toXmlString()
    {
	boolean hasVectorSets=hasVectorSets();
	StringBuffer xml=new StringBuffer();
	xml.append("<v key=\""+myKey+"\">");
	for(int row=0;row<rows.size();row++)
	    {
		if(hasVectorSets)
		    {
			xml.append(Tools.genXmlStartTag("<v_row",getSimpleProperties(row)));
			Vector keys=getVectorSetKeys();
			for(int k=0;k<keys.size();k++)
			    {
				VectorSet v=getVectorSet((String)keys.elementAt(k),row);
				xml.append(v.toXmlString());
			    }
			xml.append("</v_row>");
		    }
		else
		    xml.append(Tools.genXmlTag("v_row",getSimpleProperties(row)));
		
	    }	
	xml.append("</v>");
	return xml.toString();
    }
}
