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

import org.apache.xpath.XPathAPI;
import org.kalypso.contribs.java.xml.XMLUtilities;
import org.kalypsodeegree.xml.XMLTools;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * Utilities to transform parameters of a data model. (for example to optimize the model)
 * 
 * @author doemming
 */
public class OptimizeModelUtils
{
  private OptimizeModelUtils( )
  {
    // do not instantiate
  }

  public static void initializeModel( final Document doc, final ParameterOptimizeContext[] contexts ) throws TransformerException
  {
    for( final ParameterOptimizeContext context : contexts )
      initializeModell( doc, context );
  }

  public static void initializeModell( final Document doc, final ParameterOptimizeContext calContext ) throws TransformerException
  {
    final String value = Double.toString( calContext.getInitialValue() );
    setParameter( calContext.getxPaths(), value, doc );
  }

  public static void transformModel( final Document doc, final double[] values, final ParameterOptimizeContext[] contexts ) throws TransformerException
  {
    for( int i = 0; i < contexts.length; i++ )
      transformModel( doc, values[i], contexts[i] );
  }

  public static void transformModel( final Document doc, final double value, final ParameterOptimizeContext calContext ) throws TransformerException
  {
    final String mode = calContext.getMode();
    if( ParameterOptimizeContext.MODE_FACTOR.equals( mode ) )
      setParameter_Factor( calContext.getxPaths(), value, doc );
    else if( ParameterOptimizeContext.MODE_OFFSET.equals( mode ) )
      setParameter_Offset( calContext.getxPaths(), value, doc );
    else
      // mode direct
      setParameter( calContext.getxPaths(), (new Double( value )).toString(), doc );
  }

  public static void setParameter( final String[] querys, final String value, final Document myDom ) throws TransformerException
  {
    for( final String query : querys )
    {
      final NodeList nl = getXPath( query, myDom );
      if( nl.getLength() == 0 )
        System.out.println( "Empty result for xpath: " + query );

      for( int i = 0; i < nl.getLength(); i++ )
      {
        final Node node = nl.item( i );
        XMLUtilities.setTextNode( myDom, node, value );
      }
    }
  }

  public static void setParameter_Factor( final String[] querys, final double value, final Document myDom ) throws TransformerException
  {
    for( final String query : querys )
    {
      final NodeList nl = getXPath( query, myDom );

      for( int i = 0; i < nl.getLength(); i++ )
      {
        final String nodeValue = XMLTools.getStringValue( nl.item( i ) );
        final double setValue = value * Double.parseDouble( nodeValue );
        XMLUtilities.setTextNode( myDom, nl.item( i ), Double.toString( setValue ) );
      }
    }
  }

  public static void setParameter_Offset( final String[] querys, final double value, final Document myDom ) throws TransformerException
  {
    for( final String query : querys )
    {
      final NodeList nl = getXPath( query, myDom );

      for( int i = 0; i < nl.getLength(); i++ )
      {
        final String nodeValue = XMLTools.getStringValue( nl.item( i ) );
        final double setValue = value + Double.parseDouble( nodeValue );
        XMLUtilities.setTextNode( myDom, nl.item( i ), Double.toString( setValue ) );
      }
    }
  }

  // method returns nodeList to a given query
  public static NodeList getXPath( final String xPathQuery, final Document domNode ) throws TransformerException
  {
    final String newXPathQuery = null;
    try
    {
      return XPathAPI.selectNodeList( domNode, xPathQuery, domNode );
    }
    catch( final TransformerException e )
    {
      System.out.println( "Failed to resolve xpath: " + newXPathQuery );
      throw e;
    }
  }

  // method returns the Document of a xml-file
  public static Document getXML( final InputStream inputStream ) throws Exception
  {
    final DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
    final DocumentBuilder docuBuilder = factory.newDocumentBuilder();
    return docuBuilder.parse( inputStream );
  }

  // method writes a document(node) to a file
  public static void toFile( final File file, final Node node ) throws TransformerException, FileNotFoundException
  {
    final Transformer t = TransformerFactory.newInstance().newTransformer();
    final DOMSource src = new DOMSource( node );
    final FileOutputStream outStr = new FileOutputStream( file );
    final OutputStreamWriter fw = new OutputStreamWriter( outStr );
    final StreamResult result = new StreamResult( fw );
    t.transform( src, result );
  }
}
