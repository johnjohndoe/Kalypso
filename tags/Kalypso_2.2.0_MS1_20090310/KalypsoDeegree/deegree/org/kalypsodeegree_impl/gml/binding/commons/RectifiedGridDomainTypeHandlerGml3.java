/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 * 
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always.
 * 
 * If you intend to use this software in other ways than in kalypso
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree,
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */
package org.kalypsodeegree_impl.gml.binding.commons;

import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.commons.xml.NS;
import org.kalypso.contribs.java.net.IUrlResolver;
import org.kalypso.gmlschema.types.AbstractOldFormatMarshallingTypeHandlerAdapter;
import org.kalypso.gmlschema.types.TypeRegistryException;
import org.kalypsodeegree.model.coverage.GridRange;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridDomain.OffsetVector;
import org.kalypsodeegree_impl.model.cv.GridRange_Impl;
import org.kalypsodeegree_impl.model.geometry.AdapterBindingToValue;
import org.kalypsodeegree_impl.model.geometry.AdapterBindingToValue_GML31;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * Modified Nadja Peiler's RectifiedGridDomainTypeHandler for the GML3 Namespace.
 * <p>
 * We are using this true handler instead of using java-binding, because the java binding classes are not correct. The
 * binding classes do not support getting/setting of the grid-domain.
 * </p>
 * TypeHandler for GridDomain of RectifiedGridCoverage
 * 
 * @author Dejan Antanaskovic
 */
public class RectifiedGridDomainTypeHandlerGml3 extends AbstractOldFormatMarshallingTypeHandlerAdapter
{
  public static final QName TYPENAME = new QName( NS.GML3, "RectifiedGridDomainType" );

  public RectifiedGridDomainTypeHandlerGml3( )
  {
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#getClassName()
   */
  public Class getValueClass( )
  {
    return RectifiedGridDomain.class;
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#getTypeName()
   */
  public QName getTypeName( )
  {
    return TYPENAME;
  }

  /**
   * @see org.kalypso.gmlschema.types.AbstractOldFormatMarshallingTypeHandlerAdapter#marshall(java.lang.Object,
   *      org.w3c.dom.Document, java.net.URL)
   */
  @Deprecated
  @Override
  public Element marshall( final Object object, final Document document, final URL context ) throws TypeRegistryException
  {
    final RectifiedGridDomain gridDomain = (RectifiedGridDomain) object;

    final Element e_rectifiedGrid = document.createElementNS( NS.GML3, "gml:RectifiedGrid" );
    e_rectifiedGrid.setAttribute( "dimension", "2" );

    final Element e_limits = document.createElementNS( NS.GML3, "gml:limits" );
    final Element e_gridEnvelope = document.createElementNS( NS.GML3, "gml:GridEnvelope" );
    final GridRange gridRange = gridDomain.getGridRange();
    final double[] lows = gridRange.getLow();
    final String stringLows = new String( (new Double( lows[0] )).intValue() + " " + (new Double( lows[1] )).intValue() );
    final double[] highs = gridRange.getHigh();
    final String stringHighs = new String( (new Double( highs[0] )).intValue() + " " + (new Double( highs[1] )).intValue() );
    final Element e_low = document.createElementNS( NS.GML3, "gml:low" );
    e_low.appendChild( document.createTextNode( stringLows ) );
    final Element e_high = document.createElementNS( NS.GML3, "gml:high" );
    e_high.appendChild( document.createTextNode( stringHighs ) );
    e_gridEnvelope.appendChild( e_low );
    e_gridEnvelope.appendChild( e_high );
    e_limits.appendChild( e_gridEnvelope );
    e_rectifiedGrid.appendChild( e_limits );

    final Element e_origin = document.createElementNS( NS.GML3, "gml:origin" );
    GM_Point origin;
    try
    {
      origin = gridDomain.getOrigin( null );
    }
    catch( final Exception e1 )
    {
      throw new TypeRegistryException( "origin error", e1 );
    }
    final Element e_point = document.createElementNS( NS.GML3, "gml:Point" );
    try
    {
      final String coordinateSystem = origin.getCoordinateSystem();
      if( coordinateSystem != null )
        e_point.setAttribute( "srsName", coordinateSystem );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    final Element e_coordinates = document.createElementNS( NS.GML3, "gml:pos" );
    // e_coordinates.setAttribute( "cs", "," );
    // e_coordinates.setAttribute( "decimal", "." );
    // e_coordinates.setAttribute( "ts", " " );
    final String stringOrigin = new String( origin.getX() + " " + origin.getY() );
    e_coordinates.appendChild( document.createTextNode( stringOrigin ) );
    e_point.appendChild( e_coordinates );
    e_origin.appendChild( e_point );
    e_rectifiedGrid.appendChild( e_origin );

    final OffsetVector offsetX = gridDomain.getOffsetX();
    final OffsetVector offsetY = gridDomain.getOffsetY();

    final Element e_offsetVector1 = document.createElementNS( NS.GML3, "rgc:offsetVector" );
    final String offsetVector1 = new String( offsetX.getGeoX() + " " + offsetX.getGeoY() );
    e_offsetVector1.appendChild( document.createTextNode( offsetVector1 ) );
    e_rectifiedGrid.appendChild( e_offsetVector1 );
    final Element e_offsetVector2 = document.createElementNS( NS.GML3, "rgc:offsetVector" );
    final String offsetVector2 = new String( offsetY.getGeoX() + " " + offsetY.getGeoY() );
    e_offsetVector2.appendChild( document.createTextNode( offsetVector2 ) );
    e_rectifiedGrid.appendChild( e_offsetVector2 );

    return e_rectifiedGrid;
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#unmarshall(org.w3c.dom.Node, java.net.URL,
   *      org.kalypso.contribs.java.net.IUrlResolver)
   */
  @Deprecated
  @Override
  public Object unmarshall( final Node node, final URL context, final IUrlResolver urlResolver )
  {
    final Node node_limits = ((Element) node).getElementsByTagNameNS( NS.GML3, "limits" ).item( 0 );
    final Node node_gridEnv = ((Element) node_limits).getElementsByTagNameNS( NS.GML3, "GridEnvelope" ).item( 0 );
    final Node n_low = ((Element) node_gridEnv).getElementsByTagNameNS( NS.GML3, "low" ).item( 0 );
    final String[] lows = n_low.getFirstChild().getNodeValue().trim().split( " " );
    final double[] low = new double[lows.length];
    for( int i = 0; i < low.length; i++ )
      low[i] = Double.parseDouble( lows[i] );

    final Node n_high = ((Element) node_gridEnv).getElementsByTagNameNS( NS.GML3, "high" ).item( 0 );
    final String[] highs = n_high.getFirstChild().getNodeValue().trim().split( " " );
    final double[] high = new double[highs.length];
    for( int i = 0; i < high.length; i++ )
      high[i] = Double.parseDouble( highs[i] );

    final GridRange gridRange = new GridRange_Impl( low, high );

    final Node n_origin = ((Element) node).getElementsByTagNameNS( NS.GML3, "origin" ).item( 0 );
    final Node n_point = ((Element) n_origin).getElementsByTagNameNS( NS.GML3, "Point" ).item( 0 );
    try
    {
      final AdapterBindingToValue adapter = new AdapterBindingToValue_GML31();
      final GM_Point origin = (GM_Point) adapter.wrapFromNode( n_point );

      final NodeList nl_offSetVector = ((Element) node).getElementsByTagNameNS( NS.GML3, "offsetVector" );
      final List<RectifiedGridDomain.OffsetVector> offSetVectors = new ArrayList<OffsetVector>();
      for( int i = 0; i < nl_offSetVector.getLength(); i++ )
      {
        final Node n_offsetVector = nl_offSetVector.item( i );
        final String[] vectorCoos = n_offsetVector.getFirstChild().getNodeValue().trim().split( " " );
        final double[] vectorCoo = new double[vectorCoos.length];
        for( int n = 0; n < vectorCoo.length; n++ )
          vectorCoo[n] = Double.parseDouble( vectorCoos[n] );

        if( vectorCoos.length >= 2 )
          offSetVectors.add( new RectifiedGridDomain.OffsetVector( vectorCoo[0], vectorCoo[1] ) );
      }

      if( offSetVectors.size() == 2 )
      {
        final RectifiedGridDomain.OffsetVector offsetX = offSetVectors.get( 0 );
        final RectifiedGridDomain.OffsetVector offsetY = offSetVectors.get( 1 );

        return new RectifiedGridDomain( origin, offsetX, offsetY, gridRange );
      }
      else
        throw new IllegalStateException( "Wrong number of offset vectors" );
    }
    catch( final Throwable e )
    {
      System.out.println( e );
      return null;
    }
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#getShortname()
   */
  public String getShortname( )
  {
    return "rectifiedGridDomain";
  }

  /**
   * @throws CloneNotSupportedException
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#cloneObject(java.lang.Object)
   */
  public Object cloneObject( final Object objectToClone, final String gmlVersion ) throws CloneNotSupportedException
  {
    throw new CloneNotSupportedException( "Clone is not supported!" );
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#parseType(java.lang.String)
   */
  public Object parseType( final String text )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.kalypso.gmlschema.types.ITypeHandler#isGeometry()
   */
  public boolean isGeometry( )
  {
    return false;
  }

}