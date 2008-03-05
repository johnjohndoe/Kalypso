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

import java.util.ArrayList;

import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyEdit;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

/**
 * @author kimwerner
 */

public class AddBewuchsResolution extends AbstractProfilMarkerResolution
{

  public AddBewuchsResolution( )
  {
    super( "Bewuchsparameter an der Trennfläche erzeugen", null, null );
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.resolutions.AbstractProfilMarkerResolution#resolve(org.kalypso.model.wspm.core.profil.IProfil,
   *      org.eclipse.core.resources.IMarker)
   */
  @Override
  protected void resolve( final IProfil profil )
  {
    final IComponent CTrennF = profil.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
    final IProfilPointMarker[] deviders = profil.getPointMarkerFor( CTrennF );
    if( deviders.length < 2 )
      return;
    final int leftIndex = profil.indexOfPoint( deviders[0].getPoint() );
    final IComponent cAX = profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_BEWUCHS_AX );
    final IComponent cAY = profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_BEWUCHS_AY );
    final IComponent cDP = profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_BEWUCHS_DP );

    final int iAX = profil.indexOfProperty( cAX );
    final int iAY = profil.indexOfProperty( cAY );
    final int iDP = profil.indexOfProperty( cDP );

    if( leftIndex > 0 )
    {
      final IRecord point_l = profil.getPoint( leftIndex - 1 );
      Double AX_l = (Double) point_l.getValue( iAX );
      Double AY_l = (Double) point_l.getValue( iAY );
      Double DP_l = (Double) point_l.getValue( iDP );
      for( int i = leftIndex - 1; i >= 0; i-- )
      {
        if( AX_l * AY_l * DP_l != 0.0 )
        {
          profil.setActivePoint( point_l);
          break;
        }
        final IRecord point = profil.getPoint( i );
        final Double AX = (Double) point.getValue( iAX );
        final Double AY = (Double) point.getValue( iAY );
        final Double DP = (Double) point.getValue( iDP );
        if( AX_l == 0.0 & AX != 0.0 )
          point_l.setValue( iAX, AX );
        if( AY_l == 0.0 & AY != 0.0 )
          point_l.setValue( iAY, AY );
        if( DP_l == 0.0 & DP != 0.0 )
          point_l.setValue( iDP, DP );
      }
    }
    final int rightIndex = profil.indexOfPoint( deviders[0].getPoint() );
    if( rightIndex < profil.getPoints().length )
    {
      final IRecord point_r = profil.getPoint( rightIndex );
      Double AX_r = (Double) point_r.getValue( iAX );
      Double AY_r = (Double) point_r.getValue( iAY );
      Double DP_r = (Double) point_r.getValue( iDP );
      for( int i = rightIndex; i < profil.getPoints().length; i++ )
      {
        if( AX_r * AY_r * DP_r != 0.0 )
        {
          profil.setActivePoint( point_r);
          break;
        }
        final IRecord point = profil.getPoint( i );
        final Double AX = (Double) point.getValue( iAX );
        final Double AY = (Double) point.getValue( iAY );
        final Double DP = (Double) point.getValue( iDP );
        if( AX_r == 0.0 & AX != 0.0 )
          point_r.setValue( iAX, AX );
        if( AY_r == 0.0 & AY != 0.0 )
          point_r.setValue( iAY, AY );
        if( DP_r == 0.0 & DP != 0.0 )
          point_r.setValue( iDP, DP );
      }
    }

  }
}
