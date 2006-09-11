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
package org.kalypso.model.wspm.schema.function;

import java.util.LinkedList;
import java.util.Map;

import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.model.wspm.core.gml.ProfileFeatureFactory;
import org.kalypso.model.wspm.core.gml.WspmProfile;
import org.kalypso.model.wspm.core.gml.WspmReachProfileSegment;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

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
    // TODO: interpolate geometry onto profile points.

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
      final WspmReachProfileSegment segment = new WspmReachProfileSegment( feature );

      final WspmProfile profileMember = segment.getProfileMember();
      if( profileMember == null )
        return currentValue;

      final IProfil profil = ProfileFeatureFactory.toProfile( profileMember.getFeature() );

      segment.setStation( profil.getStation() );

      final LinkedList<POINT_PROPERTY> pointProperties = profil.getPointProperties( false );
      final POINT_PROPERTY ppRW = pointProperties.contains( POINT_PROPERTY.RECHTSWERT ) ? POINT_PROPERTY.RECHTSWERT : null;
      final POINT_PROPERTY ppHW = pointProperties.contains( POINT_PROPERTY.HOCHWERT ) ? POINT_PROPERTY.HOCHWERT : null;
      // final POINT_PROPERTY ppRW = pointProperties.contains( POINT_PROPERTY.RECHTSWERT ) ? POINT_PROPERTY.RECHTSWERT
      // :
      // POINT_PROPERTY.BREITE;
      // final POINT_PROPERTY ppHW = pointProperties.contains( POINT_PROPERTY.HOCHWERT ) ? POINT_PROPERTY.HOCHWERT :
      // null;
      final POINT_PROPERTY ppH = POINT_PROPERTY.HOEHE;

      if( ppRW == null || ppHW == null || ppH == null )
        return null;

      final LinkedList<IProfilPoint> points = profil.getPoints();
      final GM_Position[] positions = new GM_Position[points.size()];
      int count = 0;
      for( final IProfilPoint point : points )
      {
        final double rw = point.getValueFor( ppRW );
        final double hw = ppHW == null ? 0.0 : point.getValueFor( ppHW );
        final double h = point.getValueFor( ppH );

        positions[count++] = GeometryFactory.createGM_Position( rw, hw, h );
      }
      return GeometryFactory.createGM_Curve( positions, null );
    }
    catch( final ProfilDataException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    catch( final GM_Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }

    // TODO Auto-generated method stub
    return null;
  }

}
