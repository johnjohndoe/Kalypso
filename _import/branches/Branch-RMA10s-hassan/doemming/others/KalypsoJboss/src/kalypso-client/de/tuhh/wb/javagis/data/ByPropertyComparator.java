package de.tuhh.wb.javagis.data;
import java.util.Comparator;

public class ByPropertyComparator implements Comparator
{
    public static final int SMALLER=0;
    public static final int SMALLER_EQUAL=1;
    public static final int EQUAL=2;
    public static final int GREATER_EQUAL=3;
    public static final int GREATER=4;

    private int myPropCol;
    private GisElementClass myGisElementClass;
    private int myType;
    private Object myCompareValue;

    public ByPropertyComparator(GisElementClass gisElementClass,int propertyColumn)
    {	
	this.myGisElementClass=gisElementClass;
	this.myPropCol=propertyColumn;
    }
    
    public ByPropertyComparator(GisElementClass gisElementClass,int propertyColumn,Object compareValue,int equationType)
    {	
	this.myGisElementClass=gisElementClass;
	this.myPropCol=propertyColumn;
	this.myType=equationType;
	this.myCompareValue=compareValue;
    }

    public int compare(Object id1, Object id2) 
    {
	Object p1,p2;
	if(myPropCol==0)
	    {
		p1=id1;
		p2=id2;
	    }
	else
	    {
		p1=myGisElementClass.getSimplePropertyValue(id1,myPropCol-1);
		p2=myGisElementClass.getSimplePropertyValue(id2,myPropCol-1);
	    }
	if(p1==null && p2==null)
	    return 0;
	if(p1==null && p2 !=null)
	    return 1;
	if(p2==null && p1 !=null)
	    return -1;
	return ((Comparable)p1).compareTo(p2);
    }
    
    public int compareToValue(Object id,Object value)
    {
	Object p;
	if(myPropCol==0)
	    p=id;
	else
	    p=myGisElementClass.getSimplePropertyValue(id,myPropCol-1);
	if(p==null && value==null)
	    return 0;
	if(p==null && value !=null)
	    return 1;
	if(value==null && p !=null)
	    return -1;
	return ((Comparable)p).compareTo(value);
    }
    
    public boolean filter(Object id)
    {
	Object p;
	int result=0;
	if(myPropCol==0)
	    p=id;
	else
	    p=myGisElementClass.getSimplePropertyValue(id,myPropCol-1);

	if(p==null && myCompareValue==null)
	    result= 0;
	if(p==null && myCompareValue !=null)
	    result= 1;
	if(myCompareValue==null && p !=null)
	    result= -1;
	if(myCompareValue!=null && p!=null)
	    result= ((Comparable)p).compareTo(myCompareValue);
	switch(myType)
	    {
	    case SMALLER:
		return(result<0);
	    case SMALLER_EQUAL:
		return(result<=0);
	    case EQUAL:
		return(result==0);
	    case GREATER_EQUAL:
		return(result>=0);
	    case GREATER:
		return(result>0);
	    default:
		break;
	    }
	return true;
    }

    public boolean equals(Object obj)
    {
	return false;
    }
    
}
