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
package org.kalypsodeegree_impl.model.feature.function;

import java.util.Map;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction;
import org.kalypsodeegree_impl.model.geometry.GM_Object_Impl;
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

  /**
   * @see org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction#init(java.util.Map)
   */
  @Override
  public void init( Map<String, String> properties )
  {

    final String geom1Prop = properties.get( "geometry1Property" );
    final String geom2Prop = properties.get( "geometry2Property" );

    try
    {
      m_geom1 = geom1Prop == null ? null : QName.valueOf( geom1Prop );
      m_geom2 = geom2Prop == null ? null : QName.valueOf( geom2Prop );
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
  public Object getValue( Feature feature, IPropertyType pt, Object currentValue )
  {
    final IFeatureType featureType = feature.getFeatureType();

    final IPropertyType geom1Property = featureType.getProperty( m_geom1 );
    final IPropertyType geom2Property = featureType.getProperty( m_geom2 );

    if( geom1Property == null || geom2Property == null )
      return null;

    final GM_Point geom1 = ((GM_Object) feature.getProperty( geom1Property )).getCentroid();
    final GM_Point geom2 = ((GM_Object) feature.getProperty( geom2Property )).getCentroid();

    final GM_Position[] positions = new GM_Position[] { geom1.getPosition(), geom2.getPosition() };

    try
    {
       return GeometryFactory.createGM_Curve( positions, geom1.getCoordinateSystem() );
    }
    catch( GM_Exception e )
    {
      e.printStackTrace();
    }
    return null;
  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#setValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  public Object setValue( Feature feature, IPropertyType pt, Object valueToSet )
  {
    // value can't be set by the user
    return null;
  }

}
