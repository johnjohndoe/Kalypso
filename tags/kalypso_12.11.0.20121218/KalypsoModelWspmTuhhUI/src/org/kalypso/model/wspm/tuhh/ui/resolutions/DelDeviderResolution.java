/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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

import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfilePointMarker;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.observation.result.IComponent;

/**
 * @author kimwerner
 */

public class DelDeviderResolution extends AbstractProfilMarkerResolution
{
  private int m_deviderIndex;

  private String m_deviderTyp;

  private boolean m_initialized = false;

  /**
   * deletes this pointmarker
   * 
   * @param deviderTyp
   *          ,deviderIndex devider=IProfil.getDevider(deviderTyp)[deviderIndex] deviderindex < 0 remove all deviders
   *          (same as remove component)
   */
  public DelDeviderResolution( final int deviderIndex, final String deviderTyp )
  {
    super( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.resolutions.DelDeviderResolution.0" ), null, null ); //$NON-NLS-1$
    m_deviderIndex = deviderIndex;
    m_deviderTyp = deviderTyp;
    m_initialized = true;
  }

  public DelDeviderResolution( )
  {
    this( -1, "" ); //$NON-NLS-1$
    m_initialized = false;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.resolutions.AbstractProfilMarkerResolution#getSerializedParameter()
   */
  @Override
  public String getSerializedParameter( )
  {
    return super.getSerializedParameter() + ";" + m_deviderIndex + ";" + m_deviderTyp; //$NON-NLS-1$ //$NON-NLS-2$
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.resolutions.AbstractProfilMarkerResolution#resolve(org.kalypso.model.wspm.core.profil.IProfil,
   *      org.eclipse.core.resources.IMarker)
   */
  @Override
  public boolean resolve( final IProfile profil )
  {
    if( m_initialized )
    {
      final IComponent comp = profil.hasPointProperty( m_deviderTyp );
      if( m_deviderIndex < 0 )
        return profil.removePointProperty( comp );

      final IProfilePointMarker[] markers = profil.getPointMarkerFor( comp );
      final IProfilePointMarker marker = m_deviderIndex < markers.length ? markers[m_deviderIndex] : null;

      if( marker == null )
        return false;
      profil.removePointMarker( marker );
      return true;
    }
    throw new IllegalStateException();
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.resolutions.AbstractProfilMarkerResolution#setData(java.lang.String)
   */
  @Override
  public void setData( final String parameterStream )
  {
    final String[] params = getParameter( parameterStream );
    try
    {
      m_deviderIndex = new Integer( params[1] );
      m_deviderTyp = params[2];
      m_initialized = true;
    }
    catch( final Exception e )
    {
      throw new IllegalArgumentException();
    }

  }

}
