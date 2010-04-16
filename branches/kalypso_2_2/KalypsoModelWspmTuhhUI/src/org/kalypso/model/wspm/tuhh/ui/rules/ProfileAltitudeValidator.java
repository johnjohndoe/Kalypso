/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.model.wspm.tuhh.ui.rules;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.CoreException;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.reparator.IProfilMarkerResolution;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.core.profil.validator.IValidatorMarkerCollector;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

/**
 * @author kimwerner
 */
public class ProfileAltitudeValidator
{
  private final IProfil m_profil;

  private final String m_pluginId = PluginUtilities.id( KalypsoModelWspmTuhhUIPlugin.getDefault() );

  private final double m_delta;

  private final IComponent m_Breite;

  private final IValidatorMarkerCollector m_collector;

  private final String m_station;

  public ProfileAltitudeValidator( IProfil profil, final IValidatorMarkerCollector collector )
  {
    m_Breite = profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_BREITE );
    m_delta = m_Breite == null ? 0.0001 : m_Breite.getPrecision();
    m_station = String.format( "km %.4f", profil.getStation() );
    m_profil = profil;
    m_collector = collector;
  }

  public IRecord[] getPoints( )
  {
    return m_profil.getPoints();
  }

  public String getPluginId( )
  {
    return m_pluginId;
  }

  public double getDelta( )
  {
    return m_delta;
  }

  public final void createMarker( final String message, final int pos, final String componentID ) throws CoreException
  {
    m_collector.createProfilMarker( IMarker.SEVERITY_ERROR, message, m_station, pos, componentID, m_pluginId );
  }

  public final void createMarker( final String message, final int pos, final String componentID, final IProfilMarkerResolution resolution ) throws CoreException
  {
    m_collector.createProfilMarker( IMarker.SEVERITY_ERROR, message, m_station, pos, componentID, m_pluginId, new IProfilMarkerResolution[] { resolution } );
  }

  public final int isEqual( final int begin, final int end, final String componentID )
  {
    return isEqual( begin, end, componentID, IWspmConstants.POINT_PROPERTY_HOEHE );
  }

  public final int isEqual( final int begin, final int end, final String componentID_1, final String componentID_2 )
  {
    return compare( begin, end, componentID_1, componentID_2, 0 );
  }

  public final int isUpper( final int begin, final int end, final String upperComp, final String lowerComp )
  {
    return compareNot( begin, end, upperComp, lowerComp, -1 );
  }

  public final int isUpper( final int begin, final int end, final String upperComp )
  {
    return isUpper( begin, end, upperComp, IWspmConstants.POINT_PROPERTY_HOEHE );
  }

  public final int isLower( final int begin, final int end, final String lowerComp )
  {
    return isLower( begin, end, IWspmConstants.POINT_PROPERTY_HOEHE, lowerComp );
  }

  public final int isLower( final int begin, final int end, final String upperComp, final String lowerComp )
  {
    return compareNot( begin, end, upperComp, lowerComp, 1 );
  }

  /**
   * @return the index of first point check failed
   * @param check
   *          -1 for componentID_1 < componentID_2
   *          <p>
   *          0 for componentID_1 = componentID_2
   *          <p>
   *          1 for componentID_1 > componentID_2
   */

  public final int compareNot( final int begin, final int end, final String componentID_1, final String componentID_2, final int check )
  {

    final int step = begin < end ? 1 : -1;
    int i = begin;
    final IRecord[] points = m_profil.getPoints();
    while( i != end )
    {
      final IRecord point = points[i];
      final Double h1 = ProfilUtil.getDoubleValueFor( componentID_1, point );
      final Double h2 = ProfilUtil.getDoubleValueFor( componentID_2, point );
      if( h1.isNaN() || !h2.isNaN() || (int) (Math.abs( h1 - h2 ) < m_delta ? 0 : Math.signum( h2 - h1 )) != check )
      {
        i = i + step;
        continue;
      }
      return i;
    }
    return -1;
  }

  public final int compare( final int begin, final int end, final String componentID_1, final String componentID_2, final int check )
  {
    final int step = begin < end ? 1 : -1;
    int i = begin;
    final IRecord[] points = m_profil.getPoints();
    while( i != end )
    {
      final IRecord point = points[i];
      final Double h1 = ProfilUtil.getDoubleValueFor( componentID_1, point );
      final Double h2 = ProfilUtil.getDoubleValueFor( componentID_2, point );
      if( !h1.isNaN() && !h2.isNaN() && (int) (Math.abs( h1 - h2 ) < m_delta ? 0 : Math.signum( h2 - h1 )) == check )
        return i;
      i = i + step;
    }
    return -1;
  }

}
