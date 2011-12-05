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

import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

/**
 * @author kimwerner
 */
public class AddBewuchsResolution extends AbstractProfilMarkerResolution
{
  Integer m_pointIndex;

  boolean m_orientationLeft;

  public AddBewuchsResolution( final int pointIndex, final boolean orientationLeft )
  {
    super( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.resolutions.AddBewuchsResolution.0" ), null, null ); //$NON-NLS-1$
    m_pointIndex = pointIndex;
    m_orientationLeft = orientationLeft;
  }

  public AddBewuchsResolution( )
  {
    this( -1, false );
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.resolutions.AbstractProfilMarkerResolution#resolve(org.kalypso.model.wspm.core.profil.IProfil,
   *      org.eclipse.core.resources.IMarker)
   */
  @Override
  public boolean resolve( final IProfil profil )
  {
    final IComponent cAX = profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_BEWUCHS_AX );
    final IComponent cAY = profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_BEWUCHS_AY );
    final IComponent cDP = profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_BEWUCHS_DP );

    final int iAX = profil.indexOfProperty( cAX );
    final int iAY = profil.indexOfProperty( cAY );
    final int iDP = profil.indexOfProperty( cDP );

    final int step = m_orientationLeft ? -1 : 1;

    final IRecord point = profil.getPoint( m_pointIndex );
    Double AX_P = (Double) point.getValue( iAX );
    Double AY_P = (Double) point.getValue( iAY );
    Double DP_P = (Double) point.getValue( iDP );

    for( int i = m_pointIndex + step; i > -1 && i < profil.getPoints().length; i = i + step )
    {

      final IRecord p = profil.getPoint( i );
      final Double AX = (Double) p.getValue( iAX );
      final Double AY = (Double) p.getValue( iAY );
      final Double DP = (Double) p.getValue( iDP );
      if( AX_P == 0.0 & AX != 0.0 )
        AX_P = AX;
      if( AY_P == 0.0 & AY != 0.0 )
        AY_P = AY;
      if( DP_P == 0.0 & DP != 0.0 )
        DP_P = DP;
      if( AX_P * AY_P * DP_P != 0.0 )
      {
        point.setValue( iAX, AX_P );
        point.setValue( iAY, AY_P );
        point.setValue( iDP, DP_P );
        profil.setActivePoint( point );
        break;
      }

    }
    return true;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.resolutions.AbstractProfilMarkerResolution#getSerializedParameter()
   */
  @Override
  public String getSerializedParameter( )
  {
    return super.getSerializedParameter() + ";" + m_pointIndex + ";" + m_orientationLeft; //$NON-NLS-1$ //$NON-NLS-2$
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
      m_pointIndex = new Integer( params[1] );
      m_orientationLeft = new Boolean( params[2] );
    }
    catch( final Exception e )
    {
      throw new IllegalArgumentException();
    }
  }
}
