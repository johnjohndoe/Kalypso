package org.deegree_impl.gml.schema;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.PrintWriter;
import java.io.StringReader;
import java.io.StringWriter;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLConnection;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

/**
 * 
 * @author von D?mming
 */
public class XMLHelper {
	public static final String XMLSCHEMA_NS = "http://www.w3.org/2001/XMLSchema";

	public static final String GMLSCHEMA_NS = "http://www.opengis.net/gml";

	public static boolean isGlobalElementDefinition(Node node) {
		Node parentNode = node.getParentNode();
		String ns = parentNode.getNamespaceURI();
		String name = parentNode.getLocalName();
		return (XMLSCHEMA_NS.equals(ns) && "schema".equals(name));
	}

	public static Document getAsDOM(String url) throws Exception {
		return getAsDOM(new URL(url));
	}

	public static Document getAsDOM(File file) throws Exception {
		return getAsDOM(new FileInputStream(file));
	}

	public static Document getAsDOM(final InputStream inStream)
			throws Exception {
		return getAsDOM(new InputSource(inStream));
	}

	public static Document getAsDOM(final InputSource inputSource)
			throws Exception {
		final DocumentBuilderFactory factory = DocumentBuilderFactory
				.newInstance();
		factory.setNamespaceAware(true);

		final DocumentBuilder docuBuilder = factory.newDocumentBuilder();

		final Document dom = docuBuilder.parse(inputSource);
		return dom;
	}

	public static Document getAsDOM(final URL url) throws Exception {
		//    System.out.println( "\n\n<!--\n " + url + "\n-->" );

		final URLConnection connection = url.openConnection();
		final InputSource source = new InputSource(connection.getInputStream());
		final String contentEncoding = connection.getContentEncoding();
		if (contentEncoding != null)
			source.setEncoding(contentEncoding);

		return getAsDOM(source);
	}

	public static Node getAttributeNode(Node node, String attributeName) {
		try {
			NamedNodeMap nodeMap = node.getAttributes();

			return nodeMap.getNamedItem(attributeName);
		} catch (Exception e) {
			return null;
		}
	}

	public static String getAttributeValue(Node node, String attributeName) {
		return getAttributeNode(node, attributeName).getNodeValue();
	}

	public static NodeList getXPath(String xPathQuery, Node domNode) {
		NodeList nl = null;

		try {
			nl = org.apache.xpath.XPathAPI.selectNodeList(domNode, xPathQuery);
		} catch (Exception e) {
			System.out.println(e.getMessage());
			e.printStackTrace();
		}

		return nl;
	}

	public static String getXPathContent(String xPathQuery, Node domNode) {
		NodeList nl = getXPath(xPathQuery, domNode);

		if (nl == null)
			return null;

		String result = "test...";

		for (int i = 0; i < nl.getLength(); i++) {
			Node node = nl.item(i);

			result += node.getNodeValue();
		}

		return result;

	}

	public static Document post(String url, String data) throws Exception {
		return post(new URL(url), data);
	}

	public static Document post(URL url, String data) throws Exception {
		URLConnection connect = url.openConnection();

		if (connect instanceof HttpURLConnection) {
			HttpURLConnection uc = (HttpURLConnection) connect;
			uc.setRequestMethod("POST");
			uc.setDoInput(true);
			uc.setDoOutput(true);
			uc.setUseCaches(false);

			PrintWriter pw = new PrintWriter(uc.getOutputStream());
			pw.print("<?xml version=\"1.0\" encoding=\"UTF-8\"?>" + data);
			pw.flush();
			pw.close();

			return getAsDOM(uc.getInputStream());
		}

		throw new Exception("uups, no http connection");
	}

	public static NodeList reduceByAttribute(NodeList nl, String attributeName,
			String attributeValue) {
		NodeList_Impl result = new NodeList_Impl();

		for (int i = 0; i < nl.getLength(); i++) {
			try {
				NamedNodeMap nodeMap = nl.item(i).getAttributes();

				if (attributeValue.equals(nodeMap.getNamedItem(attributeName)
						.getNodeValue()))
					result.add(nl.item(i));
			} catch (Exception e) {
				// nothing to do
			}
		}

		return result;
	}

	public static String toString(NodeList nl) {
		StringBuffer result = new StringBuffer();

		for (int i = 0; i < nl.getLength(); i++)
			result.append(toString(nl.item(i)));

		return result.toString();
	}

	public static String toString(Node node) {
		try {
			Transformer t = TransformerFactory.newInstance().newTransformer();
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

	public static String xslTransform(Node domNode, String outputMethod,
			String xslTemplateString) {
		try {
			String xslString = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
					+ "<xsl:stylesheet version=\"1.0\" "
					+ " xmlns:xsl=\"http://www.w3.org/1999/XSL/Transform\">"
					+ "<xsl:output method=\"" + outputMethod + "\" />"
					+ xslTemplateString + "</xsl:stylesheet>";

			DOMSource xmlSource = new DOMSource(domNode);
			StreamSource xslSource = new StreamSource(new StringReader(
					xslString));

			return xslTransform(xmlSource, xslSource);

		} catch (Exception e) {
			e.printStackTrace();

			return null;
		}
	}

	public static String xslTransform(Source xmlSource, Source xslSource) {
		try {
			TransformerFactory transformerFactory = TransformerFactory
					.newInstance();

			//		transformerFactory.setAttribute("version",new String("1.0"));
			Transformer transformer = transformerFactory
					.newTransformer(xslSource);
			StringWriter resultSW = new StringWriter();
			transformer.transform(xmlSource, new StreamResult(resultSW));

			return resultSW.toString();

		} catch (Exception e) {
			e.printStackTrace();

			return null;
		}
	}

	public static String xslTransform(final File xmlFile, final File xslFile)
			throws Exception {
		return xslTransform(new FileInputStream(xmlFile), new FileInputStream(
				xslFile));
	}

	public static String xslTransform(final InputStream xmlFile,
			final InputStream xslFile) throws Exception {

		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		factory.setNamespaceAware(true);

		DocumentBuilder docuBuilder = factory.newDocumentBuilder();
		Document xmlDOM = docuBuilder.parse(xmlFile);
		Document xslDOM = docuBuilder.parse(xslFile);

		return xslTransform(new DOMSource(xmlDOM), new DOMSource(xslDOM));
	}

	public static boolean isAbstractElementDefinition(Node node) {
		String abstractStatus = ((Element) node).getAttribute("abstract");
		if (abstractStatus == null)
			return false;
		if ("false".equals(abstractStatus) || "0".equals(abstractStatus)
				|| "".equals(abstractStatus))
			return false;
		return true;
	}

	/**
	 * Helper methode for easy handling obects in switch blocks
	 * 
	 * @return position of object in objectArray TODO move to a general
	 *         HelperClass
	 */
	public static int indexOf(Object object, Object[] objectArray) {
		for (int i = 0; i < objectArray.length; i++)
			if (object.equals(objectArray[i]))
				return i;
		return -1;
	}

	/**
	 * as every Feature must extend from AbstractFeatureType or
	 * AbstractFeatureCollection this methods search for the root.
	 * 
	 * @return root type. e.g. "AbstractFeatureType" or
	 *         "AbstractFeatureCollectionType"
	 */
	public static String getGMLBaseType(GMLSchema schema, Node complexTypeNode) {

		Element element = (Element) complexTypeNode;
		NodeList_Impl nl = new NodeList_Impl();
		nl.add(element.getElementsByTagNameNS(XMLSCHEMA_NS, "restriction"));
		nl.add(element.getElementsByTagNameNS(XMLSCHEMA_NS, "extension"));
		//    System.out.println( toString( nl ) );
		if (nl.getLength() == 0) {
			if (!XMLHelper.GMLSCHEMA_NS.equals(schema.getTargetNS()))
				return null;
			SchemaAttribute typeNameAttribute = new SchemaAttribute(schema,
					getAttributeNode(complexTypeNode, "name"));
			return typeNameAttribute.getValue();
		}
		SchemaAttribute attribute = new SchemaAttribute(schema,
				getAttributeNode(nl.item(0), "base"));

		final String typeName = attribute.getValue();
		final String typeNameSpace = attribute.getValueNS();
		final GMLSchema typeSchema = schema.getGMLSchema(typeNameSpace);
		Node contentNode = typeSchema.getContentNode(typeNameSpace, typeName);
		if (contentNode == null)
			System.out.println("test");
		final String baseName = getGMLBaseType(typeSchema, contentNode);
		return baseName;
	}
}