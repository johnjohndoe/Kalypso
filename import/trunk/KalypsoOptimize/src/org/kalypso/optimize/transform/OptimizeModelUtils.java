/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
  
---------------------------------------------------------------------------------------------------*/
package org.kalypso.optimize.transform;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStreamWriter;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.deegree.xml.XMLTools;
import org.kalypso.java.xml.XMLUtilities;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * Utilities to transform parameters of a data model. (for example to optimize
 * the model)
 * 
 * @author doemming
 */
public class OptimizeModelUtils
{
  public OptimizeModelUtils()
  {
  // do not instanciate
  }

  public static void initializeModel( Document doc, ParameterOptimizeContext[] contexts )
      throws TransformerException
  {
    for( int i = 0; i < contexts.length; i++ )
      initializeModell( doc, contexts[i] );
  }

  public static void initializeModell( Document doc, ParameterOptimizeContext calContext )
      throws TransformerException
  {
    String value = Double.toString( calContext.getInitialValue() );
    setParameter( calContext.getxPaths(), value, doc );
  }

  public static void transformModel( Document doc, double[] values,
      ParameterOptimizeContext[] contexts ) throws TransformerException
  {
    for( int i = 0; i < contexts.length; i++ )
      transformModel( doc, values[i], contexts[i] );
  }

  public static void transformModel( Document doc, double value, ParameterOptimizeContext calContext )
      throws TransformerException
  {
    String mode = calContext.getMode();
    if( ParameterOptimizeContext.MODE_FACTOR.equals( mode ) )
      setParameter_Factor( calContext.getxPaths(), value, doc );
    else if( ParameterOptimizeContext.MODE_OFFSET.equals( mode ) )
      setParameter_Offset( calContext.getxPaths(), value, doc );
    else
      // mode direct
      setParameter( calContext.getxPaths(), ( new Double( value ) ).toString(), doc );
  }

//  public static void setParameter_Factor( String[] querys, double value, Document myDom )
//      throws TransformerException
//  {
//    for( int n = 0; n < querys.length; n++ )
//    {
//      String query = querys[n];
//      NodeList nl = getXPath( query, myDom );
//      for( int i = 0; i < nl.getLength(); i++ )
//      {
//        String nodeValue = ( nl.item( i ) ).getNodeValue();
//        double setValue = value * Double.parseDouble( nodeValue );
//        ( nl.item( i ) ).setNodeValue( String.valueOf( setValue ) );
//      }
//    }
//  }

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
        XMLUtilities.setTextNode( myDom, node, value );
      }
    }
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
        XMLUtilities.setTextNode( myDom, nl.item( i ), Double.toString( setValue ) );
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
        XMLUtilities.setTextNode( myDom, nl.item( i ), Double.toString( setValue ) );
      }
    }
  }

  // TODO
  //method returns nodeList to a given query
  public static NodeList getXPath( String xPathQuery, Document domNode )
      throws TransformerException
  {
    NodeList nl = null;
    Element documentElement = domNode.getDocumentElement();
    nl = org.apache.xpath.XPathAPI.selectNodeList( domNode, xPathQuery, documentElement );
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

  //method returns the Document of a xml-file
  public static Document getXML( InputStream inputStream ) throws Exception
  {
    DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
    DocumentBuilder docuBuilder = factory.newDocumentBuilder();
    Document dom = docuBuilder.parse( inputStream );
    return dom;
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
}

