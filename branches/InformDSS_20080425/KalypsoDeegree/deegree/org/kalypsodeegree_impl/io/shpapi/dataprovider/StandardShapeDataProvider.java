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
 *  ---------------------------------------------------------------------------*/
package org.kalypsodeegree_impl.io.shpapi.dataprovider;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_MultiCurve;
import org.kalypsodeegree.model.geometry.GM_MultiPoint;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.io.shpapi.ShapeConst;

/**
 * simulates the "old" shape behavior of kalypso in order that the GeometryType of the first feature defines the output
 * shape type.
 * 
 * @author Thomas Jung
 */
public class StandardShapeDataProvider implements IShapeDataProvider
{

  private Feature[] m_features;

  private byte m_shptype;

  private IPropertyType m_geometryPropertyType;

  public StandardShapeDataProvider( final Feature[] features )
  {
    m_features = features;

    getStandardParameter( features );
  }

  private void getStandardParameter( final Feature[] features )
  {
    // take the default geometry of the first feature to get the shape type.
    final GM_Object object = features[0].getDefaultGeometryProperty();
    if( object instanceof GM_Point )
    {
      m_shptype = ShapeConst.SHAPE_TYPE_POINT;
    }
    else if( object instanceof GM_Curve )
    {
      m_shptype = ShapeConst.SHAPE_TYPE_POLYLINE;
    }
    else if( object instanceof GM_Surface )
    {
      m_shptype = ShapeConst.SHAPE_TYPE_POLYGON;
    }
    else if( object instanceof GM_MultiPoint )
    {
      m_shptype = ShapeConst.SHAPE_TYPE_POINT;
    }
    else if( object instanceof GM_MultiCurve )
    {
      m_shptype = ShapeConst.SHAPE_TYPE_POLYLINE;
    }
    else if( object instanceof GM_MultiSurface )
    {
      m_shptype = ShapeConst.SHAPE_TYPE_POLYGON;
    }
    else
      m_shptype = ShapeConst.SHAPE_TYPE_POINT;

    m_geometryPropertyType = features[0].getFeatureType().getDefaultGeometryProperty();
  }

  /**
   * @see org.kalypsodeegree_impl.io.shpapi.IShapeDataProvider#getFeatures()
   */
  public Feature getFeature( final int index )
  {
    return m_features[index];
  }

  /**
   * @see org.kalypsodeegree_impl.io.shpapi.IShapeDataProvider#getOutputShapeConstant()
   */
  public byte getOutputShapeConstant( )
  {
    return m_shptype;
  }

  /**
   * @see org.kalypsodeegree_impl.io.shpapi.IShapeDataProvider#getFeatureType()
   */
  public IFeatureType getFeatureType( )
  {
    return m_features[0].getFeatureType();
  }

  public IPropertyType getGeometryPropertyType( )
  {
    return m_geometryPropertyType;
  }

  /**
   * @see org.kalypsodeegree_impl.io.shpapi.IShapeDataProvider#getFeaturesLength()
   */
  public int getFeaturesLength( )
  {
    return m_features.length;
  }

  /**
   * @see org.kalypsodeegree_impl.io.shpapi.IShapeDataProvider#setFeatures(org.kalypsodeegree.model.feature.Feature[])
   */
  public void setFeatures( final Feature[] features )
  {
    m_features = features;

  }

  /**
   * @see org.kalypsodeegree_impl.io.shpapi.IShapeDataProvider#setOutputShapeConstant(byte)
   */
  public void setOutputShapeConstant( final byte shapeConstant )
  {
    m_shptype = shapeConstant;

  }

  /**
   * @see org.kalypsodeegree_impl.io.shpapi.IShapeDataProvider#getGeometry(int)
   */
  public GM_Object getGeometry( final int index )
  {
    return (GM_Object) m_features[index].getProperty( getGeometryPropertyType() );
  }

  /**
   * @see org.kalypsodeegree_impl.io.shpapi.IShapeDataProvider#getFeatureProperty(int,
   *      org.kalypso.gmlschema.property.IPropertyType)
   */
  public Object getFeatureProperty( int featureIndex, IPropertyType propertyType )
  {
    return m_features[featureIndex].getProperty( propertyType );
  }

}
