
package de.tuhh.wb.javagis.data;

public class TSLink
{
	private String myString;
	
	public TSLink (String value)
	{
		myString = value;
	}
	
	public String getString()
	{
		return myString;
	}
	public String toString()
	{
		return getText();
		/*
		if(myString==null)
			return "null";
		else
			return myString.toString();
		 */
		 }
	
	public String getCode()
	{
		if(myString==null)
			return null;
		else
			return myString.replaceAll(".+,","");
	}
	public String getText()
	{
		if(myString==null)
			return null;
		else
			return myString.replaceAll(",.+","");
	}
}

