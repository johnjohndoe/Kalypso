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
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

/**
 * @author kimwerner
 */

public class DelBewuchsResolution extends AbstractProfilMarkerResolution
{

  public DelBewuchsResolution( )
  {
    super( "Bewuchsparameter im Flu�schlauch entfernen", null, null );
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.resolutions.AbstractProfilMarkerResolution#resolve(org.kalypso.model.wspm.core.profil.IProfil,
   *      org.eclipse.core.resources.IMarker)
   */
  public boolean resolve( final IProfil profil )
  {
    final IComponent cTrennF = profil.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
    if( cTrennF == null )
      return false;
    final IProfilPointMarker[] deviders = profil.getPointMarkerFor( cTrennF );
    if( deviders.length < 2 )
      return false;
    final int l = profil.indexOfPoint( deviders[0].getPoint() );
    final int r = profil.indexOfPoint( deviders[deviders.length - 1].getPoint() );
    if( l >= r )
      return false;
    final int iAX = profil.indexOfProperty( IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AX );
    final int iAY = profil.indexOfProperty( IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AY );
    final int iDP = profil.indexOfProperty( IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_DP );
    for( int i = l; i < r; i++ )
    {
      final IRecord point = profil.getPoint( i );
      point.setValue( iAX, 0.0 );
      point.setValue( iAY, 0.0 );
      point.setValue( iDP, 0.0 );
    }
    return true;
  }

}
