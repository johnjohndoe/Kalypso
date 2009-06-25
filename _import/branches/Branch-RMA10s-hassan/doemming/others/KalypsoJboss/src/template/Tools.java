package de.tuhh.wb.javagis.model;

import java.io.PrintWriter; 

public abstract class Tools
{
    public static void genXml(PrintWriter out, String name,String attributeName,String attribute, String content)
    {
	out.print("<"+name+" "+attributeName+"=\""+attribute+"\">");
	out.print(content);
	out.println("</"+name+">");
    }

    public static void genXml(PrintWriter out, String name,String[] attributeName,String[] attribute, String content)
    {
	out.print("<"+name);
	for(int i=0;i<attributeName.length;i++)
	 out.print(" "+attributeName[i]+"=\""+attribute[i]+"\"");

	if("".equals(content))
	    out.println("/>");
	else
	    {
		out.println(">");
		out.print(content);
		out.println("</"+name+">");
	    }
    }

    public static void genXml(PrintWriter out, String name,String[] attributeName,Object[] attribute, String content)
    {
	out.print("<"+name);
	for(int i=0;i<attributeName.length;i++)
	    if(attribute[i]!=null)
		out.print(" "+attributeName[i]+"=\""+attribute[i].toString()+"\"");

	if("".equals(content))
	    out.println("/>");
	else
	    {
		out.println(">");
		out.print(content);
		out.println("</"+name+">");
	    }
    }

    public static void genXml(PrintWriter out, String name, String content)
    {
	out.print("<"+name+">");
	out.print(content);
	out.println("</"+name+">");
    }
    public static void genXmlTag(PrintWriter out, String name)
    {
	out.print("<"+name+">");
    }
    public static void genXmlTag(PrintWriter out, String name,String attributeName,String attribute)
    {
	out.print("<"+name+" "+attributeName+"=\""+attribute+"\">");
    }
}    
