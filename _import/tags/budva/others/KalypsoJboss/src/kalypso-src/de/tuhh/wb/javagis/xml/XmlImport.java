package de.tuhh.wb.javagis.xml;

import java.io.IOException;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import org.xml.sax.InputSource;

import org.xml.sax.ContentHandler;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;
import org.apache.xerces.parsers.SAXParser;
import java.io.StringReader;


public class XmlImport 
{
    private InputSource inputSource;
    private KalypsoXmlImportListener importListener;
    private KalypsoXmlVectorSetListener vectorSetListener;

    public XmlImport(InputSource inputSource,KalypsoXmlImportListener importListener)
    {
	this.importListener=null;
	this.vectorSetListener=null;
	this.importListener=importListener;
	this.inputSource=inputSource;
    }

    public XmlImport(String fileName,KalypsoXmlImportListener importListener) throws FileNotFoundException
    {
	this.importListener=null;
	this.vectorSetListener=null;
	this.importListener=importListener;
	File file=new File(fileName);

	FileInputStream inputStream=new FileInputStream(file);
	this.inputSource=new InputSource(inputStream);
    }

    public XmlImport(File file,KalypsoXmlImportListener importListener) throws FileNotFoundException
    {
	this.importListener=null;
	this.vectorSetListener=null;
	this.importListener=importListener;

	FileInputStream inputStream=new FileInputStream(file);
	this.inputSource=new InputSource(inputStream);
    }
    
    public void start() throws IOException,SAXException
    {
	XMLReader reader=new SAXParser();
	if(importListener!=null)
	    reader.setContentHandler(new KalypsoXmlContentHandler(importListener));
	if(vectorSetListener!=null)
	    reader.setContentHandler(new KalypsoXmlContentHandler(vectorSetListener));
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
	
	/*		FileInputStream inputStream=new FileInputStream(file);
			InputSource inputSource=new InputSource(inputStream);
			reader.parse(inputSource);
			inputStream.close();
			file=null;
	*/
	reader.parse(inputSource);
	
    }
    
    public void importVectorSet(VectorSet vsto)
    {
	//	System.out.println("next");
	String xml=vsto.toXmlString();
	System.out.println(xml);
    }
}
