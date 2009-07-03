package org.kalypso.convert.namodel.varymodel;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.io.StringWriter;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.deegree.xml.XMLTools;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;

public class XMLServiceTools
{

  public XMLServiceTools()
  {
  // do not instanciate
  }

  
  public static void setParameter( String[] querys, String value, Document myDom )
      throws TransformerException
  {
    for( int n = 0; n < querys.length; n++ )
    {

      String query = querys[n];
      NodeList nl = getXPath( query, myDom );
      for( int i = 0; i < nl.getLength(); i++ )
      {
        Node node = nl.item( i );
        setTextNode( myDom, node, value );
      }
    }
  }

  private static void setTextNode( Document dom, Node node, String value )
  {

    System.out.println( XMLServiceTools.toString( node ) );

    NodeList cn = node.getChildNodes();
    for( int _n = 0; _n < cn.getLength(); _n++ )
    {
      Node cnode = cn.item( _n );
      short nodeType = cnode.getNodeType();
      if( nodeType == Node.TEXT_NODE )
        cnode.setNodeValue( value );
    }
    if( cn.getLength() == 0 ) // text node does not exist
    {
      Text text = dom.createTextNode( value );
      node.appendChild( text );
    }
    System.out.println( XMLServiceTools.toString( node ) );

  }

  public static void setParameter_Factor( String[] querys, double value, Document myDom )
      throws TransformerException
  {
    for( int n = 0; n < querys.length; n++ )
    {

      String query = querys[n];
      System.out.println( "Query: " + query );
      NodeList nl = getXPath( query, myDom );

      for( int i = 0; i < nl.getLength(); i++ )
      {
        String nodeValue = XMLTools.getStringValue( nl.item( i ) );
        double setValue = value * Double.parseDouble( nodeValue );
        setTextNode( myDom, nl.item( i ), Double.toString( setValue ) );
      }
    }
  }

  public static void setParameter_Offset( String[] querys, double value, Document myDom )
      throws TransformerException
  {
    for( int n = 0; n < querys.length; n++ )
    {

      String query = querys[n];
      System.out.println( "Query: " + query );
      NodeList nl = getXPath( query, myDom );

      for( int i = 0; i < nl.getLength(); i++ )
      {
        String nodeValue = XMLTools.getStringValue( nl.item( i ) );
        double setValue = value + Double.parseDouble( nodeValue );
        setTextNode( myDom, nl.item( i ), Double.toString( setValue ) );
      }
    }
  }

  //method returns the Document of a xml-file
  public static Document getXML( InputStream inputStream ) throws Exception
  {
    DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
    DocumentBuilder docuBuilder = factory.newDocumentBuilder();

    Document dom = docuBuilder.parse( inputStream );
    return dom;
  }

  // method transforms value of a node to String
  public static String toString( Node node )
  {
    try
    {
      Transformer t = TransformerFactory.newInstance().newTransformer();
      t.setOutputProperty( "omit-xml-declaration", "yes" );
      DOMSource src = new DOMSource( node );
      StringWriter sw = new StringWriter();
      StreamResult result = new StreamResult( sw );
      t.transform( src, result );
      return sw.toString();
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
    return null;
  }

  //method writes a document(node) to a file
  public static void toFile( File file, Node node ) throws TransformerException,
      FileNotFoundException
  {
    Transformer t = TransformerFactory.newInstance().newTransformer();
    DOMSource src = new DOMSource( node );
    FileOutputStream outStr = new FileOutputStream( file );
    OutputStreamWriter fw = new OutputStreamWriter( outStr );
    StreamResult result = new StreamResult( fw );
    t.transform( src, result );
  }

  //method returns nodeList to a given query
  public static NodeList getXPath( String xPathQuery, Node domNode ) throws TransformerException
  {
    NodeList nl = null;
    nl = org.apache.xpath.XPathAPI.selectNodeList( domNode, xPathQuery );
    return nl;
  }

  //  method returns node to a given query
  public static Node getXPath_singleNode( String xPathQuery, Node domNode )
  {
    Node node = null;
    try
    {
      node = org.apache.xpath.XPathAPI.selectSingleNode( domNode, xPathQuery );
    }
    catch( Exception e )
    {
      System.out.println( e.getMessage() );
      e.printStackTrace();
    }
    return node;
  }

}