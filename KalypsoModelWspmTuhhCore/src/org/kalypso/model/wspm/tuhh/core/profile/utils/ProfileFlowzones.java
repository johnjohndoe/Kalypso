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
package org.kalypso.model.wspm.tuhh.core.profile.utils;

import org.apache.commons.lang3.ArrayUtils;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfilePointMarker;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyEdit;
import org.kalypso.model.wspm.core.profil.operation.ProfileOperation;
import org.kalypso.model.wspm.core.profil.operation.ProfileOperationJob;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

/**
 * @author Dirk Kuch
 */
public final class ProfileFlowzones
{

  private ProfileFlowzones( )
  {
  }

  /**
   * if an common is assigned to a left or right flood plain, this method will return this value
   */
  private static Object findCommonValue( final IProfile profile, final IComponent component, final int p1, final int p2 )
  {
    Object value = null;

    final IRecord[] points = profile.getPoints( p1, p2 );
    for( final IRecord point : points )
    {
      final Object object = point.getValue( component );
      if( Objects.isNull( value ) )
        value = object;
      else if( Objects.notEqual( value, object ) )
        return null;
    }

    return value;
  }

  public static String findLeftFloodplainClass( final IProfile profile, final IComponent component )
  {
    final int[] zone = getLeftFloodplainZone( profile );
    if( ArrayUtils.isEmpty( zone ) )
      return null;

    final Object value = findCommonValue( profile, component, zone[0], zone[1] );

    return Objects.firstNonNull( value, "" ).toString(); //$NON-NLS-1$
  }

  public static Double findLeftFloodplainValue( final IProfile profile, final IComponent component )
  {
    final int[] zone = getLeftFloodplainZone( profile );
    if( ArrayUtils.isEmpty( zone ) )
      return null;

    final Object value = findCommonValue( profile, component, zone[0], zone[1] );
    if( value instanceof Number )
      return ((Number) value).doubleValue();

    return null;

  }

  public static String findRightFloodplainClass( final IProfile profile, final IComponent component )
  {
    final int[] zone = getRightFloodplainZone( profile );
    if( ArrayUtils.isEmpty( zone ) )
      return null;

    final Object value = findCommonValue( profile, component, zone[0], zone[1] );

    return Objects.firstNonNull( value, "" ).toString(); //$NON-NLS-1$

  }

  public static Double findRightFloodplainValue( final IProfile profile, final IComponent component )
  {
    final int[] zone = getRightFloodplainZone( profile );
    if( ArrayUtils.isEmpty( zone ) )
      return null;

    final Object value = findCommonValue( profile, component, zone[0], zone[1] );
    if( value instanceof Number )
      return ((Number) value).doubleValue();

    return null;

  }

  public static String findRiverTubeClass( final IProfile profile, final IComponent component )
  {
    final int[] zone = getRiverTube( profile );
    if( ArrayUtils.isEmpty( zone ) )
      return null;

    final Object value = findCommonValue( profile, component, zone[0], zone[1] );

    return Objects.firstNonNull( value, "" ).toString(); //$NON-NLS-1$
  }

  public static Double findRiverTubeValue( final IProfile profile, final IComponent component )
  {
    final int[] zone = getRiverTube( profile );
    if( ArrayUtils.isEmpty( zone ) )
      return null;

    final Object value = findCommonValue( profile, component, zone[0], zone[1] );
    if( value instanceof Number )
      return ((Number) value).doubleValue();

    return null;
  }

  public static Double findRoughnessFactor( final IProfile profile, final IComponent component )
  {
    final int pn = profile.getPoints().length - 1;
    final Object value = findCommonValue( profile, component, 0, pn );
    if( value instanceof Number )
      return ((Number) value).doubleValue();

    return 1.0;
  }

  private static int[] getLeftFloodplainZone( final IProfile profile )
  {
    final IProfilePointMarker[] trennflaechen = profile.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
    if( trennflaechen.length < 2 )
      return null;

    final int p1 = 0;
    final int p2 = trennflaechen[0].getPoint().getIndex() - 1;

    return new int[] { p1, p2 };
  }

  private static int[] getRiverTube( final IProfile profile )
  {
    final IProfilePointMarker[] trennflaechen = profile.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
    if( trennflaechen.length < 2 )
      return null;

    final int p1 = trennflaechen[0].getPoint().getIndex();
    final int p2 = trennflaechen[1].getPoint().getIndex() - 1;

    return new int[] { p1, p2 };
  }

  private static int[] getRightFloodplainZone( final IProfile profile )
  {
    final IProfilePointMarker[] trennflaechen = profile.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
    if( trennflaechen.length < 2 )
      return null;

    final int p1 = trennflaechen[1].getPoint().getIndex();
    final int p2 = profile.getPoints().length - 1;

    return new int[] { p1, p2 };
  }

  private static void setCommonValue( final IProfile profile, final IComponent component, final int p1, final int p2, final Object value )
  {
    final PointPropertyEdit changes = new PointPropertyEdit( profile, profile.getPoints( p1, p2 ), component, value );
    final ProfileOperation operation = new ProfileOperation( "updating profile properties", profile, changes, true ); //$NON-NLS-1$
    new ProfileOperationJob( operation ).schedule();
  }

  public static void setLeftFloodplain( final IProfile profile, final IComponent component, final Object value )
  {
    final int[] zone = getLeftFloodplainZone( profile );
    if( ArrayUtils.isEmpty( zone ) )
      return;

    setCommonValue( profile, component, zone[0], zone[1], value );
  }

  public static void setRightFloodplain( final IProfile profile, final IComponent component, final Object value )
  {
    final int[] zone = getRightFloodplainZone( profile );
    if( ArrayUtils.isEmpty( zone ) )
      return;

    setCommonValue( profile, component, zone[0], zone[1], value );
  }

  public static void setRiverTube( final IProfile profile, final IComponent component, final Object value )
  {
    final int[] zone = getRiverTube( profile );
    if( ArrayUtils.isEmpty( zone ) )
      return;

    setCommonValue( profile, component, zone[0], zone[1], value );
  }

  public static void setRoughnessFactor( final IProfile profile, final IComponent component, final Double factor )
  {
    final int pn = profile.getPoints().length - 1;
    setCommonValue( profile, component, 0, pn, factor );
  }

}
