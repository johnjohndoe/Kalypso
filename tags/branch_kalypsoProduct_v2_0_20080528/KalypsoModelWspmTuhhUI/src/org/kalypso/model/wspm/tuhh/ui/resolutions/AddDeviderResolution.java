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

import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.observation.result.IComponent;

/**
 * @author kimwerner
 */

public class AddDeviderResolution extends AbstractProfilMarkerResolution
{
  private String m_deviderType;

  /**
   * erzeugt fehlende Trennfläche
   */
  public AddDeviderResolution( final String deviderType )
  {
    super( "fehlende Trennflächen erzeugen", null, null );
    m_deviderType = deviderType;
  }

  public AddDeviderResolution( )
  {
    super( "fehlende Trennflächen erzeugen", null, null );
    m_deviderType = "";
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.resolutions.AbstractProfilMarkerResolution#resolve(org.kalypso.model.wspm.core.profil.IProfil,
   *      org.eclipse.core.resources.IMarker)
   */

  public boolean resolve( final IProfil profil )
  {
    if( m_deviderType == "" || profil.getPoints().length <1)
      throw new IllegalStateException();
   
    final IComponent cTarget = profil.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE );
    if( cTarget == null )
    {
      if( profil.getPoints().length > 0 )
      {
        final IProfilPointMarker m1 = profil.createPointMarker( m_deviderType, profil.getPoint( 0 ) );
        final IProfilPointMarker m2 = profil.createPointMarker( m_deviderType, profil.getPoint( profil.getPoints().length - 1 ) );
        m1.setInterpretedValue( true );
        m2.setInterpretedValue( true );
        profil.setActivePoint( profil.getPoint( 0 ) );
      }
      else
        return false;
    }
    else
    {
      final IProfilPointMarker[] markers = profil.getPointMarkerFor( cTarget );
      if( markers.length > 0 )
      {
        final IProfilPointMarker m1 = profil.createPointMarker( m_deviderType, markers[0].getPoint() );
        final IProfilPointMarker m2 = profil.createPointMarker( m_deviderType, markers[markers.length - 1].getPoint() );
        m1.setInterpretedValue( true );
        m2.setInterpretedValue( true );
        profil.setActivePoint( markers[0].getPoint() );
      }
      else
      {
        final IProfilPointMarker m1 = profil.createPointMarker( m_deviderType, profil.getPoint( 0 ) );
        final IProfilPointMarker m2 = profil.createPointMarker( m_deviderType, profil.getPoint( profil.getPoints().length - 1 ) );
        m1.setInterpretedValue( true );
        m2.setInterpretedValue( true );
        profil.setActivePoint( profil.getPoint( 0 ) );
      }
      return true;
    }
    return false;
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
      m_deviderType = params[1];
    }
    catch( Exception e )
    {
      throw new IllegalArgumentException();
    }
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.resolutions.AbstractProfilMarkerResolution#getSerializedParameter()
   */
  @Override
  public String getSerializedParameter( )
  {
    return super.getSerializedParameter() + ";" + m_deviderType;
  }

}
