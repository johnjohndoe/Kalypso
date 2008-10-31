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

import javax.xml.namespace.QName;

import ogc31.www.opengis.net.gml.FileType;

import org.kalypso.commons.xml.NS;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Envelope;

/**
 * TODO: add setters/getters for the coverage-function
 * 
 * @author Dejan Antanaskovic, Gernot Belger
 */
public class RectifiedGridCoverage extends AbstractFeatureBinder implements ICoverage
{
  public static final QName QNAME = new QName( NS.GML3, "RectifiedGridCoverage" );

  public static final QName QNAME_PROP_GRID_DOMAIN = new QName( NS.GML3, "rectifiedGridDomain" );

  private static final QName QNAME_PROP_RANGE_SET = new QName( NS.GML3, "rangeSet" );

  private static final QName QNAME_PROP_BOUNDED_BY = new QName( NS.GML3, "boundedBy" );

  public RectifiedGridCoverage( final Feature feature )
  {
    super( feature, RectifiedGridCoverage.QNAME );
  }

  public static String getNameStatic( )
  {
    return "RectifiedGridCoverage2";
  }

  /**
   * @return Returns the gridDomain.
   */
  public RectifiedGridDomain getGridDomain( )
  {
    return (RectifiedGridDomain) getFeature().getProperty( RectifiedGridCoverage.QNAME_PROP_GRID_DOMAIN );
  }

  /**
   * Sets the grid domain, also updates the boundedBy property.
   * 
   * @param gridDomain
   *            The gridDomain to set.
   */
  public void setGridDomain( final RectifiedGridDomain gridDomain )
  {
    final Feature feature = getFeature();
    feature.setProperty( RectifiedGridCoverage.QNAME_PROP_GRID_DOMAIN, gridDomain );

    try
    {
      final GM_Envelope envelope = gridDomain.getGM_Envelope( gridDomain.getCoordinateSystem() );
      feature.setProperty( QNAME_PROP_BOUNDED_BY, envelope );
      feature.invalidEnvelope();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  /**
   * @return Returns the rangeSet. Can be one of {@link FileType}; TODO: support others
   */
  public Object getRangeSet( )
  {
    return getFeature().getProperty( RectifiedGridCoverage.QNAME_PROP_RANGE_SET );
  }

  /**
   * @param rangeSet
   *          Choice can be a {@link ogc31.www.opengis.net.gml.FileType} or XXX TODO, not yet supported
   */
  public void setRangeSet( final Object rangeSet )
  {
    if( !(rangeSet instanceof FileType) )
      throw new IllegalArgumentException();

    getFeature().setProperty( RectifiedGridCoverage.QNAME_PROP_RANGE_SET, rangeSet );
  }

  /**
   * @see org.kalypsodeegree_impl.gml.binding.commons.ICoverage#getEnvelope()
   */
  public GM_Envelope getEnvelope( )
  {
    return (GM_Envelope) getFeature().getProperty( QNAME_PROP_BOUNDED_BY );
  }
}