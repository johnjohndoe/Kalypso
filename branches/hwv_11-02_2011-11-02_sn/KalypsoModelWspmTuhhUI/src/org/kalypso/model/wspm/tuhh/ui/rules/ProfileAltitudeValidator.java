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
package org.kalypso.model.wspm.tuhh.ui.rules;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.CoreException;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.reparator.IProfilMarkerResolution;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.core.profil.validator.IValidatorMarkerCollector;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

/**
 * @author kimwerner
 */
public class ProfileAltitudeValidator
{
  private final IProfil m_profil;

  private final double m_delta;

  private final IComponent m_Breite;

  private final IValidatorMarkerCollector m_collector;

  private final String m_station;

  public ProfileAltitudeValidator( final IProfil profil, final IValidatorMarkerCollector collector )
  {
    m_Breite = profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_BREITE );
    m_delta = m_Breite == null ? 0.0001 : m_Breite.getPrecision();
    m_station = String.format( "km %.4f", profil.getStation() ); //$NON-NLS-1$
    m_profil = profil;
    m_collector = collector;
  }

  public IRecord[] getPoints( )
  {
    return m_profil.getPoints();
  }

  public final void createMarker( final String message, final int pos, final String componentID ) throws CoreException
  {
    m_collector.createProfilMarker( IMarker.SEVERITY_ERROR, message, m_station, pos, componentID );
  }

  public final void createMarker( final String message, final int pos, final String componentID, final IProfilMarkerResolution resolution ) throws CoreException
  {
    m_collector.createProfilMarker( IMarker.SEVERITY_ERROR, message, m_station, pos, componentID, new IProfilMarkerResolution[] { resolution } );
  }

  public final int whileEqual( final int begin, final int end, final String componentID )
  {
    return validate( begin, end, componentID, 0, false );
  }

  public final int whileUpper( final int begin, final int end, final String componentID, final boolean orEqual )
  {
    return validate( begin, end, componentID, orEqual ? -1 : 1, orEqual );
  }

  public final int whileLower( final int begin, final int end, final String componentID, final boolean orEqual )
  {
    return validate( begin, end, componentID, orEqual ? 1 : -1, orEqual );
  }

  public final int whileNaN( final int begin, final int end, final String componentID )
  {
    final int step = begin < end ? 1 : -1;
    int i = begin;
    final IRecord[] points = m_profil.getPoints();
    while( i != end )
    {
      final IRecord point = points[i];
      final Double h = ProfilUtil.getDoubleValueFor( componentID, point );
      if( !h.isNaN() )
        return i;
      i = i + step;
    }
    return -1;
  }

  public final boolean compare( final Double d1, final Double d2, final int signum, final boolean orEqual )
  {
    final int sign = (int) (Math.abs( d1 - d2 ) < m_delta ? 0 : Math.signum( d1 - d2 ));
    return orEqual ^ (sign == signum);
  }

  /**
   * @param check
   *          the signum(id1-id2) result to look for (-1,0,1)
   * @param begin
   *          start validation
   * @param end
   *          will NOT be validated
   * @return the index of last point check fits or -1 if validation succeed
   */
  public final int validate( final int begin, final int end, final String componentID, final int check, final boolean orEqual )
  {
    if( begin < 0 )
      return -1;
    final int step = begin < end ? 1 : -1;
    int i = begin;
    int lastPos = -1;

    final IRecord[] points = m_profil.getPoints();
    while( i != end )
    {
      final IRecord point = points[i];
      final Double h1 = ProfilUtil.getDoubleValueFor( componentID, point );
      if( !h1.isNaN() )
      {
        final Double h2 = ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.POINT_PROPERTY_HOEHE, point );
        if( !compare( h1, h2, check, orEqual ) )
          return lastPos;
        lastPos = i;
      }
      i = i + step;
    }
    return -1;
  }

}
