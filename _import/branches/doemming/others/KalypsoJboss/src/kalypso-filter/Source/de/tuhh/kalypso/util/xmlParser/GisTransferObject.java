//package de.tuhh.wb.javagis.xml;
package de.tuhh.kalypso.util.xmlParser;

import org.xml.sax.Attributes;
import org.xml.sax.helpers.AttributesImpl;
import java.util.Hashtable;
import java.util.Vector;
import java.util.Enumeration;

public class GisTransferObject
{
    public static final String uri="";
    private AttributesImpl simpleProperties;
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
		this.simpleProperties=null;
		
		this.tableName=tableName;
		this.identifier=identifier;
		this.isIdSynchron=isIdSynchron;
		this.vectorSets=new Hashtable();
    }
	
    public void setSimpleProperties(Attributes simpleProperties)
    {
		this.simpleProperties=new AttributesImpl(simpleProperties);
    }
    
    public void setRelation(Attributes relationProperties)
    {
		this.isRelation=true;
		this.srcTable=relationProperties.getValue(uri,"srcKey");
		this.destTable=relationProperties.getValue(uri,"destKey");
		this.srcIdentifier=relationProperties.getValue(uri,"srcID");
		this.destIdentifier=relationProperties.getValue(uri,"destID");
    }
	
    public void addSimpleProperty(String propKey,String propValue)
    {
		String localName=propKey;
		String qName="";
		String type="xsi:string";
		String value=propValue;
		simpleProperties.addAttribute(uri,localName,qName,type,value);
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
	public void info()
	{
		System.out.println("-------------------------------------------------");
		System.out.println( "o_name=" + getTableName() + " o_ID=" + getIdentifier() );
		if( vectorSets != null || vectorSets.size() != 0 )
			System.out.println( "object has " + vectorSets.size() + " vector sets" );
		if( getSimplePropertyKeys() != null )
		{
			for( int i = 0; i < getSimplePropertyKeys().size(); i++ )
			{
				System.out.println( "key=" + getSimplePropertyKeys().elementAt( i )
									   + " value=" + getSimpleProperty( (String) getSimplePropertyKeys().elementAt( i ) ));
			}
		}
		if( isRelation() == true )
		{
			System.out.println( "srcKey=" + getRelationSrcTable() + " srcID=" + getRelationSrcIdentifier() +
								   "\ndestKey=" + getRelationDestTable() + " destID=" + getRelationDestIdentifier() );
		}
		System.out.println("-------------------------------------------------");
		System.out.println();
	}
	
}

