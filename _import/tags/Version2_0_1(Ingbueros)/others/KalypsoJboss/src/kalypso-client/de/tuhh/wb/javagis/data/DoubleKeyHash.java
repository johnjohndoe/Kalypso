package de.tuhh.wb.javagis.data;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Vector;
public class DoubleKeyHash
{
    private Hashtable myHash;
    private final static String separator=",";
    public DoubleKeyHash()
    {
	this.myHash=new Hashtable();	
    }
    
    public Enumeration getFirstKeys()
    {
	return myHash.keys();
    }

    public Enumeration getSubKeys(Object key1)
    {
	if(myHash.containsKey(key1))
	    return ((Hashtable)myHash.get(key1)).keys();
	return null;
    }

    public Object get(Object key1,Object key2)
    {
	if(myHash.containsKey(key1))
	    return ((Hashtable)myHash.get(key1)).get(key2);
	else
	    return null;
    }

    public boolean containsKey(Object key1,Object key2)
    {
	if(!myHash.containsKey(key1))
	    return false;
	return ((Hashtable)myHash.get(key1)).containsKey(key2);
    }

    public void put(Object key1,Object key2,Object value)
    {
	if(!myHash.containsKey(key1))
	    myHash.put(key1,new Hashtable());
	((Hashtable)myHash.get(key1)).put(key2,value);
    }
    
    public Vector toStrings()
    {
	Vector result=new Vector();
	for(Enumeration e1=myHash.keys();e1.hasMoreElements();)
	    {
		Object key1=e1.nextElement();
		Hashtable innerHash=(Hashtable)myHash.get(key1);
		
		for(Enumeration e2=innerHash.keys();e2.hasMoreElements();)
		    {
			Object key2=e2.nextElement();
			String line=key1.toString()+separator
			    +key2.toString()+separator
			    +get(key1,key2).toString();
			result.add(line);			    
		    }		
	    }
	return result;
    }
}
