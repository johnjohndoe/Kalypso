/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraï¿½e 22
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
import java.util.List;
import java.util.Map;

import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.gml.WspmProfile;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.util.WspmGeometryUtilities;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResultUtilities;
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
    // see IProfilPointProperty.doInterpolation(value1,value2)
    return null;
  }

  // $ANALYSIS-IGNORE
  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#getValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  public Object getValue( final Feature feature, final IPropertyType pt, final Object currentValue )
  {
    try
    {
      final WspmProfile wspmProfile = new WspmProfile( feature );

      final IProfil profil = wspmProfile.getProfil();
      if( profil == null )
        return null;

      final IRecord[] points = profil.getPoints();
      final List<GM_Position> positions = new ArrayList<GM_Position>( points.length );

      final int compRechtswert = TupleResultUtilities.indexOfComponent( profil, IWspmConstants.POINT_PROPERTY_RECHTSWERT );
      final int compHochwert = TupleResultUtilities.indexOfComponent( profil, IWspmConstants.POINT_PROPERTY_HOCHWERT );
      final int compBreite = TupleResultUtilities.indexOfComponent( profil, IWspmConstants.POINT_PROPERTY_BREITE );
      final int compHoehe = TupleResultUtilities.indexOfComponent( profil, IWspmConstants.POINT_PROPERTY_HOEHE );

      String srsName = wspmProfile.getSrsName();
      for( final IRecord point : points )
      {
        /* If there are no rw/hw create pseudo geometries from breite and station */
        final Double rw;
        final Double hw;

        if( compRechtswert != -1 && compHochwert != -1 )
        {
          rw = (Double) point.getValue( compRechtswert );
          hw = (Double) point.getValue( compHochwert );
        }
        else
        {
          if( compBreite == -1 )
            throw new IllegalStateException( "Profil ohne Breitenwerte und ohne RW/HW gefunden, Geometrieermittlung nicht möglich" );

          rw = (Double) point.getValue( compBreite );
          hw = profil.getStation() * 1000;
        }

        if( rw == null || hw == null )
          continue;

        final Double h = compHoehe == -1 ? null : (Double) point.getValue( compHoehe );

        /* We assume here that we have a GAUSS-KRUEGER crs in a profile. */
        if( srsName == null )
          srsName = TimeserieUtils.getCoordinateSystemNameForGkr( Double.toString( rw ) );

        final GM_Position position;
        if( h == null )
          position = GeometryFactory.createGM_Position( rw, hw );
        else
          position = GeometryFactory.createGM_Position( rw, hw, h );

        positions.add( position );
      }

      if( positions.size() < 2 )
        return null;

      final CS_CoordinateSystem crs = srsName == null ? null : org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory.getInstance().getOGCCSByName( srsName );

      final GM_Position[] poses = positions.toArray( new GM_Position[positions.size()] );
      final GM_Curve curve = GeometryFactory.createGM_Curve( poses, crs );

      return WspmGeometryUtilities.GEO_TRANSFORMER.transform( curve );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    return null;
  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#getValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  public static GM_Point convertPoint( final IProfil profile, final IRecord profilPoint ) throws Exception
  {
    final int compRechtswert = TupleResultUtilities.indexOfComponent( profile, IWspmConstants.POINT_PROPERTY_RECHTSWERT );
    final int compHochwert = TupleResultUtilities.indexOfComponent( profile, IWspmConstants.POINT_PROPERTY_HOCHWERT );
    final int compHoehe = TupleResultUtilities.indexOfComponent( profile, IWspmConstants.POINT_PROPERTY_HOEHE );

    if( compRechtswert == -1 || compHochwert == -1 )
      return null;

    final Double rw = (Double) profilPoint.getValue( compRechtswert );
    final Double hw = (Double) profilPoint.getValue( compHochwert );
    final Double h = compHoehe == -1 ? null : (Double) profilPoint.getValue( compHoehe );

    return WspmGeometryUtilities.pointFromRrHw( rw, hw, h );
  }
}
