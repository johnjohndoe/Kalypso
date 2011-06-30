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

import javax.xml.transform.TransformerException;

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
public final class OptimizeModelUtils
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

  private static void initializeModell( final Document doc, final ParameterOptimizeContext calContext ) throws TransformerException
  {
    final String value = Double.toString( calContext.getInitialValue() );
    setParameter( calContext.getxPaths(), value, doc );
  }

  public static void transformModel( final Node node, final double[] values, final ParameterOptimizeContext[] contexts ) throws TransformerException
  {
    for( int i = 0; i < contexts.length; i++ )
      transformModel( node, values[i], contexts[i] );
  }

  private static void transformModel( final Node node, final double value, final ParameterOptimizeContext calContext ) throws TransformerException
  {
    final String mode = calContext.getMode();
    if( ParameterOptimizeContext.MODE_FACTOR.equals( mode ) )
      setParameter_Factor( calContext.getxPaths(), value, node );
    else if( ParameterOptimizeContext.MODE_OFFSET.equals( mode ) )
      setParameter_Offset( calContext.getxPaths(), value, node );
    else // mode direct
      setParameter( calContext.getxPaths(), (new Double( value )).toString(), node );
  }

  private static void setParameter( final String[] querys, final String value, final Node node ) throws TransformerException
  {
    for( final String query : querys )
    {
      final NodeList nl = getXPath( query, node );
      if( nl.getLength() == 0 )
        System.out.println( "Empty result for xpath: " + query );

      for( int i = 0; i < nl.getLength(); i++ )
        XMLUtilities.setTextNode( nl.item( i ), value );
    }
  }

  private static void setParameter_Factor( final String[] querys, final double value, final Node node ) throws TransformerException
  {
    for( final String query : querys )
    {
      final NodeList nl = getXPath( query, node );

      for( int i = 0; i < nl.getLength(); i++ )
      {
        final String nodeValue = XMLTools.getStringValue( nl.item( i ) );
        final double setValue = value * Double.parseDouble( nodeValue );
        XMLUtilities.setTextNode( nl.item( i ), Double.toString( setValue ) );
      }
    }
  }

  private static void setParameter_Offset( final String[] querys, final double value, final Node node ) throws TransformerException
  {
    for( final String query : querys )
    {
      final NodeList nl = getXPath( query, node );

      for( int i = 0; i < nl.getLength(); i++ )
      {
        final String nodeValue = XMLTools.getStringValue( nl.item( i ) );
        final double setValue = value + Double.parseDouble( nodeValue );
        XMLUtilities.setTextNode( nl.item( i ), Double.toString( setValue ) );
      }
    }
  }

  // method returns nodeList to a given query
  private static NodeList getXPath( final String xPathQuery, final Node node ) throws TransformerException
  {
    final String newXPathQuery = null;
    try
    {
      return XPathAPI.selectNodeList( node, xPathQuery, node );
    }
    catch( final TransformerException e )
    {
      System.out.println( "Failed to resolve xpath: " + newXPathQuery );
      throw e;
    }
  }
}
