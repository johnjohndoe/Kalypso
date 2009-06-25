package de.tuhh.wb.javagis.data;
import java.util.Hashtable;
import java.util.Vector;
import java.util.Enumeration;

public class TrippelKeyHash
{
    private final static String separator=",";
    private Hashtable myHash;
    
    public TrippelKeyHash()
    {
	this.myHash=new Hashtable();	
    }
    
    public Enumeration keys()
    {
	return myHash.keys();
    }
    
    public Object get(Object key1,Object key2,Object key3)
    {
	if(myHash.containsKey(key1))
	    return ((DoubleKeyHash)myHash.get(key1)).get(key2,key3);
	else
	    return null;
    }

    public void remove(Object key1)
    {
	if(key1!=null && myHash.containsKey(key1))
	    myHash.remove(key1);
    }
    
    public boolean containsKey(Object key1,Object key2,Object key3)
    {
	if(!myHash.containsKey(key1))
	    return false;
	return ((DoubleKeyHash)myHash.get(key1)).containsKey(key2,key3);
    }

    public void put(Object key1,Object key2,Object key3,Object value)
    {
	if(!myHash.containsKey(key1))
	    myHash.put(key1,new DoubleKeyHash());
	((DoubleKeyHash)myHash.get(key1)).put(key2,key3,value);
    }

    public Vector toStrings()
    {
	Vector result=new Vector();
	for(Enumeration e1=myHash.keys();e1.hasMoreElements();)
	    {
		Object key1=e1.nextElement();	       
		DoubleKeyHash innerHash=(DoubleKeyHash)myHash.get(key1);
		
		Vector innerStrings=innerHash.toStrings();
		
		for(int i=0;i<innerStrings.size();i++)
		    {
			result.add(key1.toString()+separator+innerStrings.elementAt(i).toString());
		    }

		/*
		  for(Enumeration e2=innerHash.keys();e2.hasMoreElements();)
		  {
		  Object key2=e2.nextElement();
		  String line=key1.toString()+separator
		  +key2.toString()+separator
		  +get(key1,key2).toString();
		  result.add(line);			    
		  }
		*/
	    }
	return result;
    }
}
