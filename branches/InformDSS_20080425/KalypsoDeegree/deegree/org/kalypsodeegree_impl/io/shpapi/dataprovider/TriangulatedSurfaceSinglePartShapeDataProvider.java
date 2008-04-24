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
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree.model.geometry.GM_Triangle;
import org.kalypsodeegree_impl.io.shpapi.ShapeConst;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.geometry.GM_TriangulatedSurface_Impl;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * data provider for exporting GM_TriangulatedSurface patches from 1d2d result. It converts the existing
 * TriangulatedSurface feature into several patch features. <br>
 * This is because of the very slow handling of MultiShapesZ in ArcView.<br>
 * If you want to export the TriangulatedSurface feature as it is in the gml, use
 * {@link TriangulatedSurfaceMultiPartShapeDataProvider} instead.
 * 
 * @author Thomas Jung
 */
public class TriangulatedSurfaceSinglePartShapeDataProvider implements IShapeDataProvider
{
  private Feature[] m_features;

  private byte m_shapeConstant;

  private GM_TriangulatedSurface_Impl m_triSurface;

  private final int m_length;

  private final IValuePropertyType m_geometryPropertyType;

  public TriangulatedSurfaceSinglePartShapeDataProvider( final Feature[] features, final byte shapeType )
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

    m_length = m_triSurface.size();

  }

  /**
   * @see org.kalypsodeegree_impl.io.shpapi.IShapeDataProvider#getFeature(int)
   */
  public Feature getFeature( int index )
  {
    try
    {
      // TODO return generated feature for each patch
      /* Copy the feature. */
      Feature copiedFeature = FeatureHelper.cloneFeature( m_features[0].getParent(), m_features[0].getParentRelation(), m_features[0] );
      copiedFeature.setProperty( getGeometryPropertyType(), m_triSurface.get( index ) );
      return copiedFeature;
    }
    catch( Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    return null;
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
    // return the number of surfacePatches
    return m_length;
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
    GM_Triangle triangle = m_triSurface.get( index );
    try
    {
      GM_Surface<GM_SurfacePatch> patch = GeometryFactory.createGM_Surface( triangle );
      return patch;
    }
    catch( GM_Exception e )
    {
      e.printStackTrace();
    }
    return null;
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
