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

import org.kalypso.model.wspm.core.IWspmPointProperties;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.observation.result.IComponent;

/**
 * @author kimwerner
 */
@Deprecated
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

  @Override
  public boolean resolve( final IProfile profil )
  {
    final IComponent cAX = profil.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_AX );
    final IComponent cAY = profil.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_AY );
    final IComponent cDP = profil.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_DP );

    final int iAX = profil.indexOfProperty( cAX );
    final int iAY = profil.indexOfProperty( cAY );
    final int iDP = profil.indexOfProperty( cDP );

    final int step = m_orientationLeft ? -1 : 1;

    final IProfileRecord point = profil.getPoint( m_pointIndex );
    Double pAx = (Double) point.getValue( iAX );
    Double pAy = (Double) point.getValue( iAY );
    Double pDp = (Double) point.getValue( iDP );

    for( int i = m_pointIndex + step; i > -1 && i < profil.getPoints().length; i = i + step )
    {

      final IProfileRecord record = profil.getPoint( i );
      final Double dAx = (Double) record.getValue( iAX );
      final Double dAy = (Double) record.getValue( iAY );
      final Double dDp = (Double) record.getValue( iDP );

      // FIXME & ?!?
      if( pAx == 0.0 & dAx != 0.0 )
      {
        pAx = dAx;
      }

      // FIXME & ?!?
      if( pAy == 0.0 & dAy != 0.0 )
      {
        pAy = dAy;
      }

      // FIXME & ?!?
      if( pDp == 0.0 & dDp != 0.0 )
      {
        pDp = dDp;
      }
      if( pAx * pAy * pDp != 0.0 )
      {
        point.setValue( iAX, pAx );
        point.setValue( iAY, pAy );
        point.setValue( iDP, pDp );
        profil.getSelection().setActivePoints( point );
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
      m_pointIndex = Integer.valueOf( params[1] );
      m_orientationLeft = Boolean.valueOf( params[2] );
    }
    catch( final Exception e )
    {
      throw new IllegalArgumentException();
    }
  }
}
