package de.tuhh.kalypso.util.xmlParser;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;

import org.apache.xerces.parsers.SAXParser;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;

public class XmlImport 
{
    private File file;
    private KalypsoXmlImportListener importListener;
    //    private KalypsoXmlVectorSetListener vectorSetListener;

    public XmlImport(String fileName,KalypsoXmlImportListener importListener)
    {
	this.importListener=null;
	//	this.vectorSetListener=null;
	this.file=new File(fileName);
	this.importListener=importListener;
    }

    public XmlImport(File file,KalypsoXmlImportListener importListener)
    {
	this.importListener=null;
	//	this.vectorSetListener=null;
	this.file=file;
	this.importListener=importListener;
    }

    public void start() throws IOException,SAXException
    {
	XMLReader reader=new SAXParser();
	if(importListener!=null)
	    reader.setContentHandler(new KalypsoXmlContentHandler(importListener));
	//	if(vectorSetListener!=null)
	//	    reader.setContentHandler(new KalypsoXmlContentHandler(vectorSetListener));
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
	FileInputStream inputStream=new FileInputStream(file);
	InputSource     inputSource=new InputSource(inputStream);
	reader.parse(inputSource);
    }    

    /*    public void importVectorSet(VectorSet vsto)
    {
	//	System.out.println("next");
	String xml=vsto.toXmlString();
	System.out.println(xml);
    }
    */
}
