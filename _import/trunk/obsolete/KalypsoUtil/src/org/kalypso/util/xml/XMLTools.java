/** TODO: license definieren
*/

package org.kalypso.util.xml;

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

import org.kalypso.ogc.gml.JMNodeList;
import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;


/**
 *
 * @author von Dömming
 */
public class XMLTools
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

            /*
              InputStream inStream=uc.getInputStream();
              StringBuffer result=new StringBuffer();
              int n=0;
              byte buffer[]=new byte[100];
              while((n=inStream.read(buffer))>-1)
              {
              result.append(new String(buffer,0,n));
              }
              return result.toString();
            */
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
            nl = org.apache.xpath.XPathAPI.selectNodeList( domNode, xPathQuery );
        }
        catch( Exception e )
        {
            System.out.println( e.getMessage(  ) );
            e.printStackTrace(  );
        }

        /*
          if ( nl != null )
          System.out.println("apache XPath Search: found: "+nl.getLength()+" nodes" );
        */
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

                //			System.out.println("xpathcontent:"+toString(node));
                result += node.getNodeValue(  );
            }

            return result;
     
    }

    /*
          public Post()
          {
          try
          {
          //                perform(getCapabilitiesURL,getCapabilitiesString);

          //                perform(describeFeatureTypeURL,describeFeatureTypeString);

          String transactionString=
          transactionLeft
          //                    + insertFeatureString
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
    public static Document post( String url, String data )
        throws Exception
    {
        return post( new URL( url ), data );
    }

    public static Document post( URL url, String data )
        throws Exception
    {
        //	System.out.println("\n\n<!--\n "+url+"\n-->");
        //	System.out.println("<!--\n"+data+"\n-->\n");
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

            /*                BufferedReader br=new BufferedReader(new InputStreamReader(uc.getInputStream()));
            String line;
            while((line=br.readLine()) != null)
                {
                    System.out.println(line);
                }
            br.close();
            */
            /*
              DocumentBuilderFactory factory=DocumentBuilderFactory.newInstance();
              DocumentBuilder docuBuilder=factory.newDocumentBuilder();
              Document dom=docuBuilder.parse(uc.getInputStream());
            */
            return getAsDOM( uc.getInputStream(  ) );
        }

            throw new Exception( "uups, no http connection" );
    }

    public static NodeList reduceByAttribute( NodeList nl, String attributeName, String attributeValue )
    {
        JMNodeList result = new JMNodeList();

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

            /*
              TransformerFactory transformerFactory = TransformerFactory.newInstance();
              Transformer transformer =transformerFactory.newTransformer(xslSource);
              StringWriter resultSW=new StringWriter();
              transformer.transform( xmlSource, new StreamResult(resultSW));
              return resultSW.toString();
              // reuse the transformer with a new Source, which is our identity stylesheet itself
              //        transformer.transform( new StreamSource(new java.io.StringReader(xslString) ) , new StreamResult(System.out) );
              */
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

            // reuse the transformer with a new Source, which is our identity stylesheet itself
            //	transformer.transform( new StreamSource(new java.io.StringReader(xslString) ) , new StreamResult(System.out) );
        }
        catch( Exception e )
        {
            e.printStackTrace(  );

            return null;
        }
    }

    public static String xslTransform( File xmlFile, File xslFile )
        throws Exception
    {
        System.out.println( "xml:" + xmlFile.toString(  ) );
        System.out.println( "xsl:" + xslFile.toString(  ) );

        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance(  );
        factory.setNamespaceAware( true );

        DocumentBuilder docuBuilder = factory.newDocumentBuilder(  );
        Document xmlDOM = docuBuilder.parse( xmlFile );
        Document xslDOM = docuBuilder.parse( xslFile );

        return xslTransform( new DOMSource( xmlDOM ), new DOMSource( xslDOM ) );
    }

   
    /*    public static Image getMapRequest(URL url) throws Exception
    {
        BufferedImage result = null;

        // get map from the remote service
        NetWorker nw = new NetWorker( url );
        InputStream is = nw.getInputStream();

        String contentType = nw.getContentType();

        if ( MimeTypeMapper.isImageType( contentType ) &&
             MimeTypeMapper.isKnownImageType( contentType ) )
            {
                MemoryCacheSeekableStream mcss = new MemoryCacheSeekableStream( is );
                RenderedOp rop = JAI.create( "stream", mcss );
                result = rop.getAsBufferedImage();
                mcss.close();
            }
        else
            {
                // extract remote (error) message if the response
                // contains a known mime type
                String res = "";

                if ( MimeTypeMapper.isKnownMimeType( contentType ) )
                    {
                        res = "; remote message: ";
                        res += getInputStreamContent( is );
                    }

                throw new OGCWebServiceException_Impl( "RemoteWMS:handleGetMap",
                                                       "Response of the remote " +
                                                       "WMS contains wrong content " +
                                                       "type: " + contentType +
                                                       ";request: " + lparam + res );
            }
        return result;
    }
    */
}
