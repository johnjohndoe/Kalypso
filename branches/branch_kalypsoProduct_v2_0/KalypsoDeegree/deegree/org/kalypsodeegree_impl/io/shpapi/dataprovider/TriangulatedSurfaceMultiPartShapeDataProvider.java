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
package org.kalypsodeegree_impl.io.shpapi.dataprovider;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.io.shpapi.ShapeConst;
import org.kalypsodeegree_impl.model.geometry.GM_TriangulatedSurface_Impl;

/**
 * data provider for exporting GM_TriangulatedSurface patches from 1d2d result as one multipart shape (polygonZ). That
 * means all patches as one feature (like it is in its gml file). The result is a shape file which cannot handled in
 * ArcView.<br>
 * If you want to export the TriangulatedSurface for further works in ArcView use
 * {@link TriangulatedSurfaceSinglePartShapeDataProvider} instead.
 * 
 * @author Thomas Jung
 */
public class TriangulatedSurfaceMultiPartShapeDataProvider implements IShapeDataProvider
{
  private Feature[] m_features;

  private byte m_shapeConstant;

  private final IValuePropertyType m_geometryPropertyType;

  private GM_TriangulatedSurface_Impl m_triSurface;

  public TriangulatedSurfaceMultiPartShapeDataProvider( final Feature[] features, final byte shapeType )
  {
    m_features = features;
    m_shapeConstant = shapeType;

    m_geometryPropertyType = features[0].getFeatureType().getDefaultGeometryProperty();

    // get the GM_Objects in order to get the number of patches
    final GM_Object geom = (GM_Object) features[0].getProperty( getGeometryPropertyType() );
    if( geom instanceof GM_TriangulatedSurface_Impl )
    {
      m_triSurface = (GM_TriangulatedSurface_Impl) geom;
    }

  }

  /**
   * @see org.kalypsodeegree_impl.io.shpapi.IShapeDataProvider#getFeature(int)
   */
  public Feature getFeature( int index )
  {
    return m_features[index];

  }

  /**
   * @see org.kalypsodeegree_impl.io.shpapi.IShapeDataProvider#getFeatureType()
   */
  public IFeatureType getFeatureType( )
  {
    return m_features[0].getFeatureType();
  }

  /**
   * @see org.kalypsodeegree_impl.io.shpapi.IShapeDataProvider#getFeaturesLength()
   */
  public int getFeaturesLength( )
  {
    return m_features.length;
  }

  /**
   * @see org.kalypsodeegree_impl.io.shpapi.IShapeDataProvider#getGeometryPropertyType()
   */
  public IPropertyType getGeometryPropertyType( )
  {
    return m_geometryPropertyType;
  }

  /**
   * @see org.kalypsodeegree_impl.io.shpapi.IShapeDataProvider#getOutputShapeConstant()
   */
  public byte getOutputShapeConstant( )
  {
    return m_shapeConstant;
  }

  /**
   * @see org.kalypsodeegree_impl.io.shpapi.IShapeDataProvider#setFeatures(org.kalypsodeegree.model.feature.Feature[])
   */
  public void setFeatures( Feature[] features )
  {
    m_features = features;
  }

  /**
   * @see org.kalypsodeegree_impl.io.shpapi.IShapeDataProvider#setOutputShapeConstant(byte)
   */
  public void setOutputShapeConstant( byte shapeConstant )
  {
    m_shapeConstant = ShapeConst.SHAPE_TYPE_POLYGONZ;
  }

  /**
   * @see org.kalypsodeegree_impl.io.shpapi.IShapeDataProvider#getGeometry(int)
   */
  public GM_Object getGeometry( final int index )
  {
    return m_triSurface;
  }

  /**
   * @see org.kalypsodeegree_impl.io.shpapi.IShapeDataProvider#getFeatureProperty(int,
   *      org.kalypso.gmlschema.property.IPropertyType)
   */
  public Object getFeatureProperty( int featureIndex, IPropertyType propertyType )
  {
    return m_features[0].getProperty( propertyType );
  }

}
