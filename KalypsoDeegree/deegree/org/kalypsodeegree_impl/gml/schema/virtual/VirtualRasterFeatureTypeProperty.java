/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 *
 *  and
 *
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Contact:
 *
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *
 *  ---------------------------------------------------------------------------*/package org.kalypsodeegree_impl.gml.schema.virtual;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridDomain;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * @author Nadja
 */
public class VirtualRasterFeatureTypeProperty extends AbstractVirtualPropertyType
{
  public VirtualRasterFeatureTypeProperty( final IFeatureType ftp )
  {
    super( new QName( "virtual", "rasterGridDomainBoundary" ), 0, 1, GeometryUtilities.getPolygonClass() );
  }

  /**
   * @see org.kalypsodeegree_impl.gml.schema.virtual.VirtualFeatureTypeProperty#getVirtuelValue(org.kalypsodeegree.model.feature.Feature)
   */
  public Object getVirtuelValue( final Feature feature )
  {
    final RectifiedGridDomain rgDomain = (RectifiedGridDomain) feature.getProperty( "rectifiedGridDomain" );
    if( rgDomain == null )
      return null;
    try
    {
      // ASK: convert to targetCrs with help of TransformVisitor?!?
      return rgDomain.getGM_Surface( rgDomain.getCoordinateSystem() );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return null;
    }
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureTypeProperty#getType()
   */
  public String getType( )
  {
    return GM_Polygon.class.getName();
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureTypeProperty#isNullable()
   */
  @Override
  public boolean isNullable( )
  {
    return false;
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureTypeProperty#isGeometryProperty()
   */
  public boolean isGeometryProperty( )
  {
    return true;
  }
}