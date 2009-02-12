//package de.tuhh.wb.javagis.importexport;

import java.io.IOException;

import org.xml.sax.ContentHandler;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;
import org.apache.xerces.parsers.SAXParser;

public class XmlImport 
{
    private String fileName;
    private KalypsoXmlImportListener importListener;
    public XmlImport(String fileName,KalypsoXmlImportListener importListener)
    {
	this.fileName=fileName;
	this.importListener=importListener;
    }

    public void start() throws IOException,SAXException
    {
	XMLReader reader=new SAXParser();
	reader.setContentHandler(new KalypsoXmlContentHandler(importListener));
	/*
	//		try 
	{
	// aktivating validation
	//			reader.setFeature("http://xml.org/sax/features/validation",true);
	// aktivating schema-validation
	//			reader.setFeature("http://apache.org/xml/features/validation/schema",true);
	}
	//		catch (SAXException e)
	{
	//System.out.println("Cannot activate validation.");
		    }
	*/
	//	 	reader.parse("xml_files/xml.out");
	//	 	reader.parse("vieleDaten.xml");
	reader.parse(fileName);
    }    
}
