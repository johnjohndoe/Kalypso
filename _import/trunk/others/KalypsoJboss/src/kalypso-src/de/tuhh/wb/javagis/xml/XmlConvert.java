package de.tuhh.wb.javagis.xml;

import java.io.IOException;
import java.io.StringReader;

import org.apache.xerces.parsers.SAXParser;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;

public class XmlConvert implements KalypsoXmlVectorSetListener
{
    private VectorSet vectorSet;
    public XmlConvert()
    {
	reset();
    }

    private void reset()
    {
	this.vectorSet=null;
    }
    public void parse(String xmlString) throws IOException,SAXException
    {
	reset();
	String xml=
	    "<?xml version=\"1.0\" encoding=\"utf-8\"?><table key=\"parse\"><o ID=\"0\">"
	    +xmlString
	    +"</o></table>";
	XMLReader reader=new SAXParser();
	reader.setContentHandler(new KalypsoXmlContentHandler((KalypsoXmlVectorSetListener)this));
	InputSource inputSource=new InputSource(new StringReader(xml));
	reader.parse(inputSource);
    }
    
    public void importVectorSet(VectorSet vsto)
    {
	this.vectorSet=vsto;
    }
    
    public VectorSet toVectorSet()
    {
	return vectorSet;
    }
}
