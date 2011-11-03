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

import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

/**
 * @author kimwerner
 */

public class DelBewuchsResolution extends AbstractProfilMarkerResolution
{
  private Integer m_leftIndex = Integer.MIN_VALUE;

  private Integer m_rightIndex = Integer.MAX_VALUE;

  private boolean m_initialized = false;

  public DelBewuchsResolution( )
  {
    super( Messages.getString("org.kalypso.model.wspm.tuhh.ui.resolutions.DelBewuchsResolution.0"), null, null ); //$NON-NLS-1$

  }

  public DelBewuchsResolution( final int leftIndex, final int rightIndex )
  {
    super( Messages.getString("org.kalypso.model.wspm.tuhh.ui.resolutions.DelBewuchsResolution.1"), null, null ); //$NON-NLS-1$
    m_leftIndex = leftIndex;
    m_rightIndex = rightIndex;
    m_initialized = true;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.resolutions.AbstractProfilMarkerResolution#resolve(org.kalypso.model.wspm.core.profil.IProfil,
   *      org.eclipse.core.resources.IMarker)
   */
  @Override
  public boolean resolve( final IProfil profil )
  {

    if( !m_initialized )
    {
      final IComponent cTrennF = profil.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
      if( cTrennF == null )
        return false;
      final IProfilPointMarker[] deviders = profil.getPointMarkerFor( cTrennF );
      if( deviders.length < 2 )
        return false;

      m_leftIndex = profil.indexOfPoint( deviders[0].getPoint() );
      m_rightIndex = profil.indexOfPoint( deviders[deviders.length - 1].getPoint() );

    }

    if( m_leftIndex >= m_rightIndex )
      return false;
    final int iAX = profil.indexOfProperty( IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AX );
    final int iAY = profil.indexOfProperty( IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AY );
    final int iDP = profil.indexOfProperty( IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_DP );
    for( int i = m_leftIndex; i < m_rightIndex; i++ )
    {
      final IRecord point = profil.getPoint( i );
      point.setValue( iAX, 0.0 );
      point.setValue( iAY, 0.0 );
      point.setValue( iDP, 0.0 );
    }
    return true;
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
      m_leftIndex = new Integer( params[1] );
      m_rightIndex = new Integer( params[2] );
      m_initialized = m_leftIndex > Integer.MIN_VALUE && m_rightIndex < Integer.MAX_VALUE;
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
    return super.getSerializedParameter() + ";" + m_leftIndex + ";" + m_rightIndex; //$NON-NLS-1$ //$NON-NLS-2$
  }
}
