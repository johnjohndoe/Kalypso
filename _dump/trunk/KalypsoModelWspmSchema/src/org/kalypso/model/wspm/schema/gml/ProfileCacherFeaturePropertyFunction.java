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
package org.kalypso.model.wspm.schema.gml;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.model.wspm.core.gml.ProfileFeatureFactory;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.model.wspm.core.util.WspmGeometryUtilities;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author Gernot Belger
 */
public class ProfileCacherFeaturePropertyFunction extends FeaturePropertyFunction
{
  /**
   * @see org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction#init(java.util.Properties)
   */
  @Override
  public void init( final Map<String, String> properties )
  {
  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#setValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  public Object setValue( final Feature feature, final IPropertyType pt, final Object valueToSet )
  {
    // TODO: interpolate geometry onto profile points?

    return null;
  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#getValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  public Object getValue( final Feature feature, final IPropertyType pt, final Object currentValue )
  {
    try
    {
      final IProfil profil = ProfileFeatureFactory.toProfile( feature );

      final LinkedList<POINT_PROPERTY> pointProperties = profil.getPointProperties( false );
      final POINT_PROPERTY ppRW = pointProperties.contains( POINT_PROPERTY.RECHTSWERT ) ? POINT_PROPERTY.RECHTSWERT : null;
      final POINT_PROPERTY ppHW = pointProperties.contains( POINT_PROPERTY.HOCHWERT ) ? POINT_PROPERTY.HOCHWERT : null;
      final POINT_PROPERTY ppB = POINT_PROPERTY.BREITE;
      final POINT_PROPERTY ppH = POINT_PROPERTY.HOEHE;

      final LinkedList<IProfilPoint> points = profil.getPoints();
      final List<GM_Position> positions = new ArrayList<GM_Position>(points.size()); 

      String crsName = null;
      for( final IProfilPoint point : points )
      {
        /* If there are no rw/hw create pseudo geometries from breite and station */
        final double rw;
        final double hw;
        if( ppRW != null && ppHW != null )
        {
          rw = point.getValueFor( ppRW );
          hw = point.getValueFor( ppHW );
        }
        else
        {
          rw = point.getValueFor( ppB );
          hw = profil.getStation() * 1000;
        }
        
        if( rw == 0.0 && hw == 0.0 )
          continue;
        
        final double h = point.getValueFor( ppH );

        /* We assume here that we have a GAUSS-KRUEGER crs in a profile. */
        if( crsName == null )
          crsName = TimeserieUtils.getCoordinateSystemNameForGkr( Double.toString( rw ) );

        positions.add( GeometryFactory.createGM_Position( rw, hw, h ) );
      }

      if( positions.size() < 2 )
        return null;
      
      final CS_CoordinateSystem crs = crsName == null ? null : org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory.getInstance().getOGCCSByName( crsName );

      final GM_Position[] poses = positions.toArray( new GM_Position[positions.size()] );
      final GM_Curve curve = GeometryFactory.createGM_Curve( poses, crs );

      return WspmGeometryUtilities.GEO_TRANSFORMER.transform( curve );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }

    return null;
  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#getValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  public static GM_Point convertPoint( final IProfilPoint profilPoint ) throws Exception
  {
    final Collection<POINT_PROPERTY> pointProperties = profilPoint.getProperties();
    final POINT_PROPERTY ppRW = pointProperties.contains( POINT_PROPERTY.RECHTSWERT ) ? POINT_PROPERTY.RECHTSWERT : null;
    final POINT_PROPERTY ppHW = pointProperties.contains( POINT_PROPERTY.HOCHWERT ) ? POINT_PROPERTY.HOCHWERT : null;
    final POINT_PROPERTY ppH = POINT_PROPERTY.HOEHE;

    if( ppRW == null || ppHW == null || ppH == null )
      return null;

    final double rw = profilPoint.getValueFor( ppRW );
    final double hw = ppHW == null ? 0.0 : profilPoint.getValueFor( ppHW );
    final double h = profilPoint.getValueFor( ppH );

    return WspmGeometryUtilities.pointFromRrHw( rw, hw, h );
  }

}
