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

import org.deegree_impl.gml.schema.NodeList_Impl;
import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;


/**
 *
 * @author von Dömming
 */
public class XMLHelper
{
 

    public static Document getAsDOM( String url ) throws Exception
    {
        return getAsDOM( new URL( url ) );
    }

    public static Document getAsDOM( File file ) throws Exception
    {
        return getAsDOM( new FileInputStream( file ) );
    }

    public static Document getAsDOM( final InputStream inStream )
        throws Exception
    {
      return getAsDOM( new InputSource( inStream ) );
    }
    
    public static Document getAsDOM( final InputSource inputSource ) throws Exception
    {
      final DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance(  );
      factory.setNamespaceAware( true );

      final DocumentBuilder docuBuilder = factory.newDocumentBuilder(  );

      final Document dom = docuBuilder.parse( inputSource );
      return dom;
    }

    public static Document getAsDOM( URL url ) throws Exception
    {
        System.out.println( "\n\n<!--\n " + url + "\n-->" );

        URLConnection connect = url.openConnection(  );

        if( connect instanceof HttpURLConnection )
        {
            HttpURLConnection uc = (HttpURLConnection)connect;
            uc.setRequestMethod( "GET" );
            uc.setDoInput( true );
            uc.setDoOutput( true );
            uc.setUseCaches( false );

            return getAsDOM( uc.getInputStream(  ) );
        }
      
            throw new Exception( "uups, no http connection" );
    }

    public static Node getAttributeNode( Node node, String attributeName )
    {
        try
        {
            NamedNodeMap nodeMap = node.getAttributes(  );

            return nodeMap.getNamedItem( attributeName );
        }
        catch( Exception e )
        {
            return null;
        }
    }

    public static String getAttributeValue( Node node, String attributeName )
    {
        return getAttributeNode( node, attributeName ).getNodeValue(  );
    }

    public static NodeList getXPath( String xPathQuery, Node domNode )
    {
        NodeList nl = null;

         try
        {
           nl = org.apache.xpath.XPathAPI.selectNodeList( domNode, xPathQuery );
        }
        catch( Exception e )
        {
            System.out.println( e.getMessage(  ) );
            e.printStackTrace(  );
        }

           return nl;
    }

    public static String getXPathContent( String xPathQuery, Node domNode )
    {
        NodeList nl = getXPath( xPathQuery, domNode );

        if( nl == null )
            return null;
     
            String result = "test...";

            for( int i = 0; i < nl.getLength(  ); i++ )
            {
                Node node = nl.item( i );

                 result += node.getNodeValue(  );
            }

            return result;
     
    }

  
    public static Document post( String url, String data )
        throws Exception
    {
        return post( new URL( url ), data );
    }

    public static Document post( URL url, String data )
        throws Exception
    {
       URLConnection connect = url.openConnection(  );

        if( connect instanceof HttpURLConnection )
        {
            HttpURLConnection uc = (HttpURLConnection)connect;
            uc.setRequestMethod( "POST" );
            uc.setDoInput( true );
            uc.setDoOutput( true );
            uc.setUseCaches( false );

            PrintWriter pw = new PrintWriter( uc.getOutputStream(  ) );
            pw.print( "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" + data );
            pw.flush(  );
            pw.close(  );

     
            return getAsDOM( uc.getInputStream(  ) );
        }

            throw new Exception( "uups, no http connection" );
    }

    public static NodeList reduceByAttribute( NodeList nl, String attributeName, String attributeValue )
    {
        NodeList_Impl result = new NodeList_Impl();

        for( int i = 0; i < nl.getLength(  ); i++ )
        {
            try
            {
                NamedNodeMap nodeMap = nl.item( i ).getAttributes(  );

                if( attributeValue.equals( nodeMap.getNamedItem( attributeName ).getNodeValue(  ) ) )
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
        StringBuffer result = new StringBuffer(  );

        for( int i = 0; i < nl.getLength(  ); i++ )
            result.append( toString( nl.item( i ) ) );

        return result.toString(  );
    }

    public static String toString( Node node )
    {
        try
        {
            Transformer t = TransformerFactory.newInstance(  ).newTransformer(  );
            DOMSource src = new DOMSource( node );
            StringWriter sw = new StringWriter(  );
            StreamResult result = new StreamResult( sw );
            t.transform( src, result );

            return sw.toString(  );
        }
        catch( Exception e )
        {
            e.printStackTrace(  );

            return "sorry: " + e.getMessage(  );
        }
    }

    public static String xslTransform( Node domNode, String outputMethod, String xslTemplateString )
    {
        try
        {
            String xslString =
                "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" + "<xsl:stylesheet version=\"1.0\" " +
                " xmlns:xsl=\"http://www.w3.org/1999/XSL/Transform\">" + "<xsl:output method=\"" + outputMethod +
                "\" />" + xslTemplateString + "</xsl:stylesheet>";

            DOMSource xmlSource = new DOMSource( domNode );
            StreamSource xslSource = new StreamSource( new StringReader( xslString ) );

            return xslTransform( xmlSource, xslSource );

      
        }
        catch( Exception e )
        {
            e.printStackTrace(  );

            return null;
        }
    }

    public static String xslTransform( Source xmlSource, Source xslSource )
    {
        try
        {
            TransformerFactory transformerFactory = TransformerFactory.newInstance(  );

            //		transformerFactory.setAttribute("version",new String("1.0"));
            Transformer transformer = transformerFactory.newTransformer( xslSource );
            StringWriter resultSW = new StringWriter(  );
            transformer.transform( xmlSource, new StreamResult( resultSW ) );

            return resultSW.toString(  );

         }
        catch( Exception e )
        {
            e.printStackTrace(  );

            return null;
        }
    }

    public static String xslTransform( final File xmlFile, final File xslFile )
    throws Exception
    {
      return xslTransform( new FileInputStream( xmlFile), new FileInputStream( xslFile) );
    }

    
    public static String xslTransform( final InputStream xmlFile, final InputStream xslFile )
        throws Exception
    {
  
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance(  );
        factory.setNamespaceAware( true );

        DocumentBuilder docuBuilder = factory.newDocumentBuilder(  );
        Document xmlDOM = docuBuilder.parse( xmlFile );
        Document xslDOM = docuBuilder.parse( xslFile );

        return xslTransform( new DOMSource( xmlDOM ), new DOMSource( xslDOM ) );
    }
}
