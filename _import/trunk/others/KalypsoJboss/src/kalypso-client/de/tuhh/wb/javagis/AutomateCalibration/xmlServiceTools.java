package de.tuhh.wb.javagis.AutomateCalibration;

import org.w3c.dom.*;
import java.io.*;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.DocumentBuilder;

import javax.xml.transform.*;
import javax.xml.transform.stream.*;
import javax.xml.transform.dom.*;

public class xmlServiceTools {

	public xmlServiceTools() {

	}

	/*//method returns value of a certain parameter in a certain riverbasin(ezg)
	public static String getParameter(String ezg, String parameter, Document myDom) {
		String result = "";
		String query =
			"/theme/table[@key=\"rb\"]/o/sp"
				+ "[@m_rbNumber=\""
				+ ezg
				+ "\"]/@"
				+ parameter;
		//String query ="/theme/table[@key=\"rb\"]/o/bp/@x";
		System.out.println("Query: " + query);
		//System.out.println("DOM: "+myDom);
		NodeList nl = getXPath(query, myDom);
	
		for (int i = 0; i < nl.getLength(); i++) {
			result += toString(nl.item(i)) + "\n";
		}
		return result.replaceAll("<?.+?>", "");
	}*/

	/*//method sets value of a certain parameter in a certain riverbasin(ezg)
	public static void setParameter(
		String ezg,
		String parameter,
		String value,
		Document myDom) {
		String query =
			"/theme/table[@key=\"rb\"]/o/sp"
				+ "[@m_rbNumber=\""
				+ ezg
				+ "\"]/@"
				+ parameter;
		System.out.println("Query: " + query);
		Node node = getXPath_singleNode(query, myDom);
		try {
			node.setNodeValue(value);
		} catch (Exception e) {
			System.out.println(e.getMessage());
			e.printStackTrace();
		}
	}*/

	// method returns value of a certain query
	public static String getParameter(String query, Document myDom) {
		String result = "";
		NodeList nl = getXPath(query, myDom);

		for (int i = 0; i < nl.getLength(); i++) {
			result += toString(nl.item(i)) + "\n";
		}
		return result; //.replaceAll("<?.+?>", "");
	}

	public static void setParameter(
		String query,
		String value,
		Document myDom) {
		System.out.println("Query: " + query);
		NodeList nl = getXPath(query, myDom);

		for (int i = 0; i < nl.getLength(); i++) {
			(nl.item(i)).setNodeValue(value);
		}

	}

	public static void setParameter_Factor(
		String query,
		double value,
		Document myDom) {
		NodeList nl = getXPath(query, myDom);

		for (int i = 0; i < nl.getLength(); i++) {
			String nodeValue = (nl.item(i)).getNodeValue();
			double setValue = value*Double.parseDouble(nodeValue);
			(nl.item(i)).setNodeValue(String.valueOf(setValue));
		}
	}

	public static void setParameter_Offset(
		String query,
		double value,
		Document myDom) {
		NodeList nl = getXPath(query, myDom);

		for (int i = 0; i < nl.getLength(); i++) {
			String nodeValue = (nl.item(i)).getNodeValue();
			double setValue = value+Double.parseDouble(nodeValue);
			(nl.item(i)).setNodeValue(String.valueOf(setValue));
		}
	}
	
	public static Document getXML(String file) throws Exception {
		return getXML(new File(file));
	}

	//method returns the Document of a xml-file 
	public static Document getXML(File file) throws Exception {
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		DocumentBuilder docuBuilder = factory.newDocumentBuilder();

		FileInputStream inputStream = new FileInputStream(file);
		Document dom = docuBuilder.parse(inputStream);
		return dom;
	}

	//method transforms value of a node to String
	public static String toString(Node node) {
		try {
			Transformer t = TransformerFactory.newInstance().newTransformer();
			t.setOutputProperty("omit-xml-declaration", "yes");
			DOMSource src = new DOMSource(node);
			StringWriter sw = new StringWriter();
			StreamResult result = new StreamResult(sw);
			t.transform(src, result);
			return sw.toString();
		} catch (Exception e) {

			e.printStackTrace();
			return "sorry: " + e.getMessage();
		}
	}

	//method writes a document(node) to a file
	public static void toFile(File file, Node node) {
		try {
			Transformer t = TransformerFactory.newInstance().newTransformer();
			DOMSource src = new DOMSource(node);
			FileOutputStream outStr = new FileOutputStream(file);
			OutputStreamWriter fw = new OutputStreamWriter(outStr);
			StreamResult result = new StreamResult(fw);
			t.transform(src, result);
		} catch (Exception e) {

			e.printStackTrace();
			System.out.println("sorry: " + e.getMessage());
		}
	}

	//method returns nodeList to a given query
	public static NodeList getXPath(String xPathQuery, Node domNode) {
		NodeList nl = null;
		//	String querySource = "/*/*[1]";
		try {
			/*
			// Just to get an error message:
			XPath xpath = new XPath( query_.getSource(),
			null,                   //SourceLocator
			null,                   //PrefixResolver
			org.apache.xpath.XPath.SELECT,
			null );                 //ErrorListener
			*/
			nl = org.apache.xpath.XPathAPI.selectNodeList(domNode, xPathQuery);
		} catch (Exception e) {
			System.out.println(e.getMessage());
			e.printStackTrace();
		}
		/*
		  if ( nl != null )
		  System.out.println("apache XPath Search: found: "+nl.getLength()+" nodes" );
		*/
		return nl;
	}

	//	method returns node to a given query
	public static Node getXPath_singleNode(String xPathQuery, Node domNode) {
		Node node = null;
		try {
			node =
				org.apache.xpath.XPathAPI.selectSingleNode(domNode, xPathQuery);
		} catch (Exception e) {
			System.out.println(e.getMessage());
			e.printStackTrace();
		}
		return node;
	}

}
