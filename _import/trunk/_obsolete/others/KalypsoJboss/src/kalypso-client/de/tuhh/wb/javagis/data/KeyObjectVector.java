package de.tuhh.wb.javagis.data;

import java.util.Vector;

public class KeyObjectVector
{
    private Vector keys;
    private Vector objects;

    public KeyObjectVector()
    {
	this.keys=new Vector();
	this.objects=new Vector();
    }
    
    public void add(String key,Object object)
    {
	if(key!= null && object!=null)
	    {
		System.out.println("add "+key+" "+object);
		keys.add(key);
		objects.add(object);
	    }
    }
    
    public int size()
    {
	return keys.size();
    }

    public void addAll(String key,Vector objectList)
    {
	for(int i=0;i<objectList.size();i++)
	    add(key,objectList.elementAt(i));
    }

    public String getKeyAt(int pos)
    {
	return (String)keys.elementAt(pos);
    }
    
    public Object getIdAt(int pos)
    {
	return getObjectAt(pos);
    }

    public Object getObjectAt(int pos)
    {
	return objects.elementAt(pos);
    }
    public void clear()
    {
	keys.clear();
	objects.clear();
    }
}
