package org.kalypsodeegree_impl.gml.schema;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLConnection;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import org.kalypsodeegree.xml.XMLTools;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

/**
 * 
 * @author doemming
 */
public class XMLHelper
{
  public static final String XMLSCHEMA_NS = "http://www.w3.org/2001/XMLSchema";

  public static final String GMLSCHEMA_NS = "http://www.opengis.net/gml";

  public static final String DEFAULT_ENCODING = "UTF-8";

  public static boolean isGlobalElementDefinition( Node node )
  {
    Node parentNode = node.getParentNode();
    String ns = parentNode.getNamespaceURI();
    String name = parentNode.getLocalName();
    return ( XMLSCHEMA_NS.equals( ns ) && "schema".equals( name ) );
  }

    public static Document getAsDOM( File file, boolean namespaceaware ) throws Exception
  {
    return getAsDOM( new FileInputStream( file ), namespaceaware );
  }

  public static Document getAsDOM( final InputStream inStream, boolean namespaceaware )
      throws Exception
  {
    return getAsDOM( new InputSource( inStream ), namespaceaware );
  }

  public static Document getAsDOM( final InputSource inputSource, boolean namespaceaware )
      throws Exception
  {
    final DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
    factory.setNamespaceAware( namespaceaware );

    final DocumentBuilder docuBuilder = factory.newDocumentBuilder();
    final Document dom = docuBuilder.parse( inputSource );
    return dom;
  }

  public static Document getAsDOM( final URL url, boolean namespaceaware ) throws Exception
  {
    InputStream inputStream  = null;

    try
    {
      final URLConnection connection = url.openConnection();
      inputStream = new BufferedInputStream( connection.getInputStream() );
      final InputSource source = new InputSource( inputStream );
      final String contentEncoding = connection.getContentEncoding();

      if( contentEncoding != null )
        source.setEncoding( contentEncoding );
      else
        source.setEncoding( DEFAULT_ENCODING );
      // TODO set default encoding to "UTF-8" is this correct???
      return getAsDOM( source, namespaceaware );
    }
    finally
    {
      if( inputStream != null )
        inputStream.close();
    }
  }

  public static void writeDOM( final Document xmlDOM, final String charset, final OutputStream os ) throws TransformerException
  {
    writeDOM( xmlDOM, charset, new StreamResult( os ) );
  }

  public static void writeDOM( final Document xmlDOM, final String charset, final Writer writer ) throws TransformerException
  {
    // sollte nichte benutzt werden, wenn das charset nicht bekannt ist,
    // da sonst Mist rauskommt
    if( charset == null )
      throw new NullPointerException();
    
    writeDOM( xmlDOM, charset, new StreamResult( writer ) );
  }
  
  public static void writeDOM( final Document xmlDOM, final String charset, final StreamResult streamResult ) throws TransformerException
  {
    final TransformerFactory tFactory = TransformerFactory.newInstance();

    final Transformer t = tFactory.newTransformer();

    t.setOutputProperty( "{http://xml.apache.org/xslt}indent-amount", "2" );
    t.setOutputProperty( OutputKeys.INDENT, "yes" );
    if( charset != null )
      t.setOutputProperty( OutputKeys.ENCODING , charset );

    t.transform( new DOMSource( xmlDOM ), streamResult );
  }

  public static Node getAttributeNode( Node node, String attributeName )
  {
    try
    {
      NamedNodeMap nodeMap = node.getAttributes();

      return nodeMap.getNamedItem( attributeName );
    }
    catch( Exception e )
    {
      return null;
    }
  }

  public static String getAttributeValue( Node node, String attributeName )
  {
    return getAttributeNode( node, attributeName ).getNodeValue();
  }

  public static NodeList getXPath( String xPathQuery, Node domNode )
  {
    NodeList nl = null;

    try
    {
      // Note for Gernot: unter Windows: org.apache.xpath.XPathApi in jre/rt.jar
      // TOOD: XPathAPI doesn't seem to bee official part of the JavaAPI (it is not included
      // under linux). Is there another way to do the same?
      // ... die API gehört zu Xalan-j und ist nicht bestandteil
      // der JFC -> sollten wir xalan-j als lib mitaufnehmen?
      //nl = com.sun.org.apache.xpath.internal.XPathAPI.selectNodeList( domNode, xPathQuery );
      nl = XPathAPI.selectNodeList( domNode, xPathQuery );
    }
    catch( Exception e )
    {
      System.out.println( e.getMessage() );
      e.printStackTrace();
    }

    return nl;
  }

  public static String getXPathContent( String xPathQuery, Node domNode )
  {
    NodeList nl = getXPath( xPathQuery, domNode );

    if( nl == null )
      return null;

    String result = "test...";

    for( int i = 0; i < nl.getLength(); i++ )
    {
      Node node = nl.item( i );

      result += node.getNodeValue();
    }

    return result;

  }

  public static Document post( String url, String data, boolean namespaceaware ) throws Exception
  {
    return post( new URL( url ), data, namespaceaware );
  }

  public static Document post( URL url, String data, boolean namespaceaware ) throws Exception
  {
    URLConnection connect = url.openConnection();

    if( connect instanceof HttpURLConnection )
    {
      HttpURLConnection uc = (HttpURLConnection)connect;
      uc.setRequestMethod( "POST" );
      uc.setDoInput( true );
      uc.setDoOutput( true );
      uc.setUseCaches( false );

      PrintWriter pw = new PrintWriter( uc.getOutputStream() );
      pw.print( "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" + data );
      pw.flush();
      pw.close();

      return getAsDOM( uc.getInputStream(), namespaceaware );
    }

    throw new Exception( "uups, no http connection" );
  }

  public static NodeList reduceByAttribute( NodeList nl, String attributeName, String attributeValue )
  {
    NodeList_Impl result = new NodeList_Impl();

    for( int i = 0; i < nl.getLength(); i++ )
    {
      try
      {
        NamedNodeMap nodeMap = nl.item( i ).getAttributes();

        if( attributeValue.equals( nodeMap.getNamedItem( attributeName ).getNodeValue() ) )
          result.add( nl.item( i ) );
      }
      catch( Exception e )
      {
        // nothing to do
      }
    }

    return result;
  }

  public static String toString( NodeList nl )
  {
    StringBuffer result = new StringBuffer();

    for( int i = 0; i < nl.getLength(); i++ )
      result.append( toString( nl.item( i ) ) );

    return result.toString();
  }

  public static String toString( Node node )
  {
    try
    {
      Transformer t = TransformerFactory.newInstance().newTransformer();
      DOMSource src = new DOMSource( node );
      StringWriter sw = new StringWriter();
      StreamResult result = new StreamResult( sw );
      t.transform( src, result );

      return sw.toString();
    }
    catch( Exception e )
    {
      e.printStackTrace();

      return "sorry: " + e.getMessage();
    }
  }

  public static String xslTransform( Node domNode, String outputMethod, String xslTemplateString )
  {
    try
    {
      String xslString = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
          + "<xsl:stylesheet version=\"1.0\" "
          + " xmlns:xsl=\"http://www.w3.org/1999/XSL/Transform\">" + "<xsl:output method=\""
          + outputMethod + "\" />" + xslTemplateString + "</xsl:stylesheet>";

      DOMSource xmlSource = new DOMSource( domNode );
      StreamSource xslSource = new StreamSource( new StringReader( xslString ) );

      return xslTransform( xmlSource, xslSource );

    }
    catch( Exception e )
    {
      e.printStackTrace();

      return null;
    }
  }

  public static String xslTransform( Source xmlSource, Source xslSource )
  {
    try
    {
      TransformerFactory transformerFactory = TransformerFactory.newInstance();

      // transformerFactory.setAttribute("version",new String("1.0"));
      Transformer transformer = transformerFactory.newTransformer( xslSource );
      StringWriter resultSW = new StringWriter();
      transformer.transform( xmlSource, new StreamResult( resultSW ) );

      return resultSW.toString();

    }
    catch( Exception e )
    {
      e.printStackTrace();

      return null;
    }
  }

  public static void xslTransform( final InputStream xmlInputStream, InputStream xslInputStream,
      Writer writer ) throws TransformerException, ParserConfigurationException, SAXException,
      IOException
  {
    DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
    factory.setNamespaceAware( true );
    DocumentBuilder docuBuilder = factory.newDocumentBuilder();
    Document xmlDOM = docuBuilder.parse( xmlInputStream );
    Document xslDOM = docuBuilder.parse( xslInputStream );
    TransformerFactory transformerFactory = TransformerFactory.newInstance();
    Transformer transformer = transformerFactory.newTransformer( new DOMSource( xslDOM ) );
    transformer.transform( new DOMSource( xmlDOM ), new StreamResult( writer ) );
    writer.close();
  }

  public static String xslTransform( final File xmlFile, final File xslFile ) throws Exception
  {
    return xslTransform( new FileInputStream( xmlFile ), new FileInputStream( xslFile ) );
  }

  public static String xslTransform( final InputStream xmlFile, final InputStream xslFile )
      throws Exception
  {

    DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
    factory.setNamespaceAware( true );

    DocumentBuilder docuBuilder = factory.newDocumentBuilder();
    Document xmlDOM = docuBuilder.parse( xmlFile );
    Document xslDOM = docuBuilder.parse( xslFile );

    return xslTransform( new DOMSource( xmlDOM ), new DOMSource( xslDOM ) );
  }

  public static boolean isAbstractElementDefinition( Node node )
  {
    String abstractStatus = ( (Element)node ).getAttribute( "abstract" );
    if( abstractStatus == null )
      return false;
    if( "false".equals( abstractStatus ) || "0".equals( abstractStatus )
        || "".equals( abstractStatus ) )
      return false;
    return true;
  }

  /**
   * Helper methode for easy handling obects in switch blocks
   * 
   * @return position of object in objectArray TODO move to a general
   *         HelperClass
   */
  public static int indexOf( Object object, Object[] objectArray )
  {
    for( int i = 0; i < objectArray.length; i++ )
      if( object.equals( objectArray[i] ) )
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
  public static String getGMLBaseType( GMLSchema schema, Node complexTypeNode )
  {

    Element element = (Element)complexTypeNode;
    NodeList_Impl nl = new NodeList_Impl();
    nl.add( element.getElementsByTagNameNS( XMLSCHEMA_NS, "restriction" ) );
    nl.add( element.getElementsByTagNameNS( XMLSCHEMA_NS, "extension" ) );
    // System.out.println( toString( nl ) );
    if( nl.getLength() == 0 )
    {
      if( !XMLHelper.GMLSCHEMA_NS.equals( schema.getTargetNS() ) )
        return null;
      SchemaAttribute typeNameAttribute = new SchemaAttribute( schema, getAttributeNode(
          complexTypeNode, "name" ) );
      return typeNameAttribute.getValue();
    }
    SchemaAttribute attribute = new SchemaAttribute( schema,
        getAttributeNode( nl.item( 0 ), "base" ) );

    final String typeName = attribute.getValue();
    final String typeNameSpace = attribute.getValueNS();
    final GMLSchema typeSchema = schema.getGMLSchema( typeNameSpace );
    Node contentNode = typeSchema.getContentNode( typeNameSpace, typeName );
    if( contentNode == null )
      return null;
    final String baseName = getGMLBaseType( typeSchema, contentNode );
    return baseName;
  }

  public static String getStringFromChildElement( final Element elt, final String namespace,
      final String eltName )
  {
    final NodeList nlL = elt.getElementsByTagNameNS( namespace, eltName );
    if( nlL.getLength() > 0 )
    {
      final Element innerElt = (Element)nlL.item( 0 );
      return XMLTools.getStringValue( innerElt );
    }

    return null;
  }

  public static Node getFirstChildElement( final Node parentNode, final String ns,
      final String name, int maxDepth )
  {
    final NodeList childNodes = parentNode.getChildNodes();
    for( int i = 0; i < childNodes.getLength(); i++ )
    {
      final Node node = childNodes.item( i );
      switch( node.getNodeType() )
      {
      case Node.ELEMENT_NODE:
        if( ns != null && ns.equals( node.getNamespaceURI() ) && name.equals( node.getLocalName() ) )
          return node;
        else if( ns == null && name.equals( node.getLocalName() ) )
          return node;
        if( maxDepth > 0 )
        {
          Node subNode = getFirstChildElement( node, ns, name, maxDepth - 1 );
          if( subNode != null )
            return subNode;
        }
        break;
      default:
        continue;
      }
    }
    return null;
  }
}