import org.xml.sax.Attributes;
import org.xml.sax.helpers.AttributesImpl;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Vector;

public class VectorSet
{
    String myKey;
    private Vector rows;
    private Hashtable agregatedVectorSets;
    public VectorSet(Attributes atts)
    {
	this.myKey=atts.getValue(GisTransferObject.uri,"key");
	//	System.out.println("parsing VectorSet...\""+myKey+"\"");
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
}
