package de.tuhh.wb.javagis.tools.xml;

import java.io.*;
import java.net.*;
import org.w3c.dom.*;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.DocumentBuilder;

import javax.xml.transform.*;
import javax.xml.transform.stream.*;
import javax.xml.transform.dom.*;

import org.apache.xpath.XPath;
import org.apache.xpath.XPathContext;


public class ServiceTools
{
    public ServiceTools()
    {}
    private static String getCapabilitiesURL="http://destructivus.wb.tu-harburg.de:8080/geoserver/GetCapabilities";
    private static String describeFeatureTypeURL="http://destructivus.wb.tu-harburg.de:8080/geoserver/DescribeFeatureType";
    private static String getFeatureURL="http://destructivus.wb.tu-harburg.de:8080/geoserver/GetFeature";
    private static String transactionURL="http://destructivus.wb.tu-harburg.de:8080/geoserver/Transaction";
    
    private static String getCapabilitiesString="<GetCapabilities service=\"WFS\""
 	+" xmlns:wfs=\"http://www.opengis.net/wfs\""
	+"/>";
    //    private static String strGetCapabilities="<GetCapabilities version=\"0.0.14\"/>";
    
    private static String describeFeatureTypeString
        ="<DescribeFeatureType outputFormat=\"XMLSCHEMA\">"
	//	+"<TypeName>roads</TypeName>"
        +"</DescribeFeatureType>";
    
    private static String getFeatureString=
	"<GetFeature outputFormat=\"GML2\""
	+" xmlns=\"http://www.opengis.net/wfs\""
	+" xmlns:gml=\"http://www.opengis.net/gml\""
	+" xmlns:ogc=\"http://www.opengis.net/ogc\""
	+" xmlns:myns=\"http://destructivus.wb.tu-harburg.de:8080/geoserver/myns\""
        +" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\""
	+" xsi:schemaLocation=\""
   	+"http://www.opengis.net/wfs ../wfs/1.0.0/WFS-basic.xsd\""

	+">"
	+"<Query typeName=\"myns:rivers\">"
	/*
	  +"<ogc:Filter>"
	  +"<ogc:Within>"
	  +"<ogc:PropertyName>myns:rivers_geom</ogc:PropertyName>"
	  +"<gml:Box>"
	  +"<gml:coordinates>-100,-100 100,100</gml:coordinates>"
	  +"</gml:Box>"
	  +"</ogc:Within>"
	  +"</ogc:Filter>"
	*/
        +"</Query>"
	+"</GetFeature>";

    static String insertFeatureString=
	"<wfs:Insert>"
	+"<rivers>"
	+"<river_name>kollau</river_name>"
	+"<rivers_geom>"
	+"<gml:LineString srsName=\"http://www.opengis.net/gml/srs/epsg.xml#31491\">"
	+"<gml:coordinates >-12.5,1.45 -6.5,3.5</gml:coordinates>"
	+"</gml:LineString>"
	+"</rivers_geom>"
	+"</rivers>"
	+"</wfs:Insert>";
    
    static String updateFeatureString=
	"<wfs:Update typeName=\"rivers\">"
	+"<wfs:Property>"

	
	/*
	+"<wfs:Name>river_name</wfs:Name>"
	+"<wfs:Value>"
	+"Test 19195"
	+"</wfs:Value>"
	*/	

	
	+"<wfs:Name>rivers_geom</wfs:Name>"
	+"<wfs:Value>"
	+"<gml:LineString gid=\"rivers.19195.rivers_geom\" srsName=\"http://www.opengis.net/gml/srs/epsg.xml#31491\">"
	+"<gml:coordinates>10.5,10.5 12.5,12.5</gml:coordinates>"
	+"</gml:LineString>"      
	+"</wfs:Value>"
	
	+"</wfs:Property>"
	
	+"<ogc:Filter>"
	+"<ogc:FeatureId fid=\"rivers.19195\"/>"
	+"</ogc:Filter>"
	
	+"</wfs:Update>";

    static String deleteFeatureString=
	"<wfs:Delete typeName=\"rivers\">"
	+"<ogc:Filter>"
	+"<ogc:FeatureId fid=\"rivers.19196\"/>"
	+"</ogc:Filter>"
	+"</wfs:Delete>";
    
    private static String transactionLeft=
	"<wfs:Transaction version=\"1.0.0\" service=\"WFS\" outputFormat=\"GML2\""
	
	+" xmlns=\"http://destructivus.wb.tu-harburg.de:8080/geoserver/myns\""
	+" xmlns:gml=\"http://www.opengis.net/gml\""
	+" xmlns:ogc=\"http://www.opengis.net/ogc\""
	+" xmlns:wfs=\"http://www.opengis.net/wfs\""
        +" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\""
	+" xsi:schemaLocation=\""
	+" http://destructivus.wb.tu-harburg.de:8080/geoserver/myns"
        +" http://destructivus.wb.tu-harburg.de:8080/geoserver/DescribeFeatureType?typename=myns:rivers"
	+" http://www.opengis.net/wfs ../wfs/1.0.0/WFS-transaction.xsd\""
	+">";
	

    
    private static String transactionRight=
	"</wfs:Transaction>";
    
    /*    
	  public Post()
	  {
	  try
	  {
	  //		perform(getCapabilitiesURL,getCapabilitiesString);
	  
	  //		perform(describeFeatureTypeURL,describeFeatureTypeString);
	  
	  String transactionString=
	  transactionLeft
	  //		    + insertFeatureString
	  + updateFeatureString
	  //      + deleteFeatureString
	  +transactionRight;
	  perform(transactionURL,transactionString);
	  
	  perform(getFeatureURL,getFeatureString);
	  }
	  catch(Exception e)
	  {
	  e.printStackTrace();
	  }
	  }
    */

    public static Document postXML(String url,String data) throws Exception
    {
	return postXML(new URL(url),data);
    }
    
    public static Document postXML(URL url,String data) throws Exception
    {
	//	System.out.println("\n\n<!--\n "+url+"\n-->");
	//	System.out.println("<!--\n"+data+"\n-->\n");
	URLConnection connect = url.openConnection();
	if(connect instanceof HttpURLConnection)
	    {
		HttpURLConnection uc=(HttpURLConnection)connect;
		uc.setRequestMethod("POST");
		uc.setDoInput(true);
		uc.setDoOutput(true);
		uc.setUseCaches(false);
		
		PrintWriter pw=new PrintWriter(uc.getOutputStream());
		pw.print("<?xml version=\"1.0\" encoding=\"UTF-8\"?>"+data);
		pw.flush();
		pw.close();

		/*		BufferedReader br=new BufferedReader(new InputStreamReader(uc.getInputStream()));
		String line;
		while((line=br.readLine()) != null)
		    {
			System.out.println(line);
		    }
		br.close();
		*/

		DocumentBuilderFactory factory=DocumentBuilderFactory.newInstance();
		DocumentBuilder docuBuilder=factory.newDocumentBuilder();

		Document dom=docuBuilder.parse(uc.getInputStream());
		return dom;
	    }
	else
	    throw new Exception("uups, no http connection");	
    }

    public static String toString(Node node)
    {
	try
	    {
		Transformer t = TransformerFactory.newInstance().newTransformer();
		DOMSource src = new DOMSource(node);
		StringWriter sw = new StringWriter();
		StreamResult result = new StreamResult(sw);
		t.transform(src,result);
		return sw.toString();
	    }
	catch(Exception e)
	    {
		
		e.printStackTrace();		
		return "sorry: "+e.getMessage();
	    }
    }

    public static String getXPathContent(String xPathQuery,Node domNode)
    {
	NodeList nl=getXPath(xPathQuery,domNode);
	if(nl==null)
	    return null;
	else
	    {
		String result="test...";
		for(int i=0;i<nl.getLength();i++)
		    {
			Node node=nl.item(i);
			//			System.out.println("xpathcontent:"+toString(node));
			result+=node.getNodeValue();
		    }
		return result;
	    }
    }
    
    public static NodeList getXPath(String xPathQuery,Node domNode)
    {
	NodeList nl = null;
	//	String querySource = "/*/*[1]";
	try 
	    {
		/*
		// Just to get an error message:
		XPath xpath = new XPath( query_.getSource(),
		null,                   //SourceLocator
		null,                   //PrefixResolver
		org.apache.xpath.XPath.SELECT,
		null );                 //ErrorListener
		*/
		nl = org.apache.xpath.XPathAPI.selectNodeList(domNode, xPathQuery );
	    }
	catch(Exception e) 
	    {
		System.out.println(e.getMessage());
		e.printStackTrace();
	    }
	/*
	  if ( nl != null )
	  System.out.println("apache XPath Search: found: "+nl.getLength()+" nodes" );
	*/
	return nl;
    }







    public static String XSL_TEMPLATE_FILTER_ALL_TEXT="<xsl:template match=\"/\">" 
	+ "<xsl:copy-of select=\".\"/>"
	+ "</xsl:template>";

	
    public static String xslTransform(Node domNode, String outputMethod,String xslTemplateString) throws Exception
    {

	String xslString = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
		    + "<xsl:stylesheet version=\"1.0\" "
		    //		    + "xmlns:xalan=\"http://xml.apache.org/xslt\""
		    + " xmlns:xsl=\"http://www.w3.org/1999/XSL/Transform\">"
		    + "<xsl:output method=\""+outputMethod+"\" />"
		    //" indent=\"yes\" xalan:indent-amount=\"4\" />"
		    + xslTemplateString
		    + "</xsl:stylesheet>";
		
		//		System.out.println(xslString);
		//		DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
		//		DocumentBuilder db = dbf.newDocumentBuilder();
	DOMSource xmlSource = new DOMSource(domNode);
	StreamSource xslSource = new StreamSource(new StringReader(xslString));
	return xslTransform(xmlSource,xslSource);
	/*	
		  TransformerFactory transformerFactory = TransformerFactory.newInstance();
		  Transformer transformer =transformerFactory.newTransformer(xslSource);
		  StringWriter resultSW=new StringWriter();
		  transformer.transform( xmlSource, new StreamResult(resultSW));
		  return resultSW.toString();
		  // reuse the transformer with a new Source, which is our identity stylesheet itself
		  //	transformer.transform( new StreamSource(new java.io.StringReader(xslString) ) , new StreamResult(System.out) );
		  */

    }

    public static String xslTransform(File xmlFile,File xslFile) throws Exception
    {
	System.out.println("xml:"+xmlFile.toString());
	System.out.println("xsl:"+xslFile.toString());
	DocumentBuilderFactory factory=DocumentBuilderFactory.newInstance();
	factory.setNamespaceAware(true);
	DocumentBuilder docuBuilder=factory.newDocumentBuilder();
	Document xmlDOM=docuBuilder.parse(xmlFile);	
	Document xslDOM=docuBuilder.parse(xslFile);	
	return xslTransform(new DOMSource(xmlDOM),new DOMSource(xslDOM));
    }

    public static String xslTransform(Source xmlSource, Source xslSource) throws Exception
    {

		//		DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
		//		DocumentBuilder db = dbf.newDocumentBuilder();

		TransformerFactory transformerFactory = TransformerFactory.newInstance();
		//		transformerFactory.setAttribute("version",new String("1.0"));
		Transformer transformer =transformerFactory.newTransformer(xslSource);
		StringWriter resultSW=new StringWriter();
		transformer.transform( xmlSource, new StreamResult(resultSW));
		return resultSW.toString();
		// reuse the transformer with a new Source, which is our identity stylesheet itself
		//	transformer.transform( new StreamSource(new java.io.StringReader(xslString) ) , new StreamResult(System.out) );
    }
}
