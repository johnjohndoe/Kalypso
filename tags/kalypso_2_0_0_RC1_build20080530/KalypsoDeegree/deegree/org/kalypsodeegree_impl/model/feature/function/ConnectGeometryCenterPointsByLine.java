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
package org.kalypsodeegree_impl.model.feature.function;

import java.util.Map;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * connects center points of two geometries by a line
 * 
 * @author thuel2
 */
public class ConnectGeometryCenterPointsByLine extends FeaturePropertyFunction
{
  private QName m_geom1;

  private QName m_geom2;

  private boolean m_doClip;

  /**
   * @see org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction#init(java.util.Map)
   */
  @Override
  public void init( final Map<String, String> properties )
  {

    final String geom1Prop = properties.get( "geometry1Property" );
    final String geom2Prop = properties.get( "geometry2Property" );
    final String doClip = properties.get( "clip" );

    try
    {
      m_geom1 = geom1Prop == null ? null : QName.valueOf( geom1Prop );
      m_geom2 = geom2Prop == null ? null : QName.valueOf( geom2Prop );
      m_doClip = doClip == null ? false : Boolean.valueOf( doClip ).booleanValue();
    }
    catch( final IllegalArgumentException e )
    {
      e.printStackTrace();
    }

  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#getValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  public Object getValue( final Feature feature, final IPropertyType pt, final Object currentValue )
  {
    final IFeatureType featureType = feature.getFeatureType();

    final IPropertyType geom1Property = featureType.getProperty( m_geom1 );
    final IPropertyType geom2Property = featureType.getProperty( m_geom2 );

    if( geom1Property == null || geom2Property == null )
      return null;

    final GM_Object geom1 = (GM_Object) feature.getProperty( geom1Property );
    final GM_Object geom2 = (GM_Object) feature.getProperty( geom2Property );

    if( geom1 == null || geom2 == null )
      return null;

    final GM_Point center1 = geom1.getCentroid();
    final GM_Point center2 = geom2.getCentroid();

    final GM_Position[] positions = new GM_Position[] { center1.getPosition(), center2.getPosition() };

    try
    {
      final GM_Curve connectingCurve = GeometryFactory.createGM_Curve( positions, center1.getCoordinateSystem() );
      if( !m_doClip )
        return connectingCurve;

      final GM_Object diff1 = connectingCurve.difference( geom1 );
      final GM_Object diff2 = diff1.difference( geom2 );

      return diff2;
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
    }
    return null;
  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#setValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  public Object setValue( final Feature feature, final IPropertyType pt, final Object valueToSet )
  {
    // value can't be set by the user
    return null;
  }

}
