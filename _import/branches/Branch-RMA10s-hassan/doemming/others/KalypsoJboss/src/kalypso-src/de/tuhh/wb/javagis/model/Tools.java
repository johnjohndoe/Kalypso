package de.tuhh.wb.javagis.model;

import java.io.PrintWriter; 
import org.xml.sax.helpers.AttributesImpl;

public abstract class Tools
{
    public static void genXml(PrintWriter out, String name,String attributeName,String attribute, String content)
    {
	out.print("<"+name+" "+attributeName+"=\""+attribute+"\">");
	out.print(content);
	out.print("</"+name+">");
    }
 
    /*    public static void genXml(PrintWriter out, String name,String[] attributeName,String[] attribute, String content)
    {
	out.print("<"+name);
	for(int i=0;i<attributeName.length;i++)
	 out.print(" "+attributeName[i]+"=\""+attribute[i]+"\"");

	if("".equals(content))
	    out.print("/>");
	else
	    {
		out.print(">");
		out.print(content);
		out.print("</"+name+">");
	    }
    }
    */
    public static void genXml(PrintWriter out, String name,String[] attributeName,Object[] attribute, String content)
    {
	out.print("<"+name);
	for(int i=0;i<attributeName.length;i++)
	    if(attribute[i]!=null)
		out.print(" "+attributeName[i]+"=\""+attribute[i].toString()+"\"");

	if("".equals(content))
	    out.print("/>");
	else
	    {
		out.print(">");
		out.print(content);
		out.print("</"+name+">");
	    }
    }

    public static void genXmlOpenTag(PrintWriter out, String name,String[] attributeName,Object[] attribute)
    {
	out.print("<"+name);
	for(int i=0;i<attributeName.length;i++)
	    if(attribute[i]!=null)
		out.print(" "+attributeName[i]+"=\""+attribute[i].toString()+"\"");
	out.print(">");    
    }

    public static void genXml(PrintWriter out, String name, String content)
    {
	out.print("<"+name+">");
	out.print(content);
	out.print("</"+name+">");
    }
    public static void genXmlTag(PrintWriter out, String name)
    {
	out.print("<"+name+">");
    }
    public static void genXmlTag(PrintWriter out, String name,String attributeName,String attribute)
    {
	out.print("<"+name+" "+attributeName+"=\""+attribute+"\">");
    }
    
    //    public static void xslt(String xmlSource,String destination,String xslFile)
    //    {}

    public static String genXmlTag(String tagName,AttributesImpl atts)
    {

	StringBuffer xml=new StringBuffer();
	if(atts!=null)
	    {
		int n=atts.getLength();
		if(atts.getLength()>0)
		    {
			xml.append("<"+tagName+" ");
			for(int i=0;i<n;i++)
			    {
				xml.append(" "+atts.getLocalName(i));
				xml.append("=\"");
				xml.append(atts.getValue(i));
				xml.append("\"");      
			    }
			xml.append("/>");
		    }
	    }
	return xml.toString();
    }

    public static String genXmlStartTag(String tagName,AttributesImpl atts)
    {
	StringBuffer xml=new StringBuffer();
	xml.append("<tagName");
	for(int i=0;i<atts.getLength();i++)
	    {
		xml.append(" "+atts.getLocalName(i));
		xml.append("=\"");
		xml.append(atts.getValue(i));
		xml.append("\"");      
	    }
	return xml.toString();
    }
}    
