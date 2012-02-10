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
package org.kalypso.model.wspm.tuhh.ui.resolutions;

import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

/**
 * @author kimwerner
 */

public class MoveDeviderResolution extends AbstractProfilMarkerResolution
{
  private int m_deviderIndex;

  private String m_deviderTyp;

  private int m_pointIndex;

  private boolean m_initialized = false;

  /**
   * verschieben der Trennfläche auf den Profilpunkt IProfil.getPoints().get(index)
   * 
   * @param deviderTyp,deviderIndex
   *            devider=IProfil.getDevider(deviderTyp)[deviderIndex]
   */
  public MoveDeviderResolution( final int deviderIndex, final String deviderTyp, final int pointIndex )
  {
    super( Messages.getString("org.kalypso.model.wspm.tuhh.ui.resolutions.MoveDeviderResolution.0"), null, null ); //$NON-NLS-1$
    m_deviderIndex = deviderIndex;
    m_deviderTyp = deviderTyp;
    m_pointIndex = pointIndex;
    m_initialized = true;
  }

  public MoveDeviderResolution( )
  {
    this(-1,"",-1); //$NON-NLS-1$
    m_initialized = false;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.resolutions.AbstractProfilMarkerResolution#getSerializedParameter()
   */
  @Override
  public String getSerializedParameter( )
  {
    return super.getSerializedParameter() + ";" + m_deviderIndex + ";" + m_deviderTyp + ";" + m_pointIndex; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.resolutions.AbstractProfilMarkerResolution#resolve(org.kalypso.model.wspm.core.profil.IProfil,
   *      org.eclipse.core.resources.IMarker)
   */
  @Override
  public boolean resolve( final IProfil profil )
  {
    if( m_initialized )
    {
      final IComponent comp = profil.hasPointProperty( m_deviderTyp );
      final IComponent cBreite = profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_BREITE );
      final IProfilPointMarker[] markers = profil.getPointMarkerFor( comp );
      final IProfilPointMarker marker = m_deviderIndex < markers.length ? markers[m_deviderIndex] : null;
      final IRecord[] points = profil.getPoints();
      if( marker == null || cBreite == null || m_pointIndex < 0 || m_pointIndex >= points.length )
        return false;
      final IRecord point = points[m_pointIndex];
      marker.setPoint( point );
      profil.setActivePoint( point );
      return true;
    }
    throw new IllegalStateException();
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.resolutions.AbstractProfilMarkerResolution#setData(java.lang.String)
   */
  @Override
  public void setData( String parameterStream )
  {
    final String[] params = getParameter( parameterStream );
    try
    {
      m_deviderIndex = new Integer( params[1] );
      m_deviderTyp = params[2];
      m_pointIndex = new Integer( params[3] );
      m_initialized = true;
    }
    catch( Exception e )
    {
      throw new IllegalArgumentException();
    }

  }

}
