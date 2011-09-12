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
package org.kalypso.model.wspm.tuhh.ui.export.sobek.flowzones;

import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class LeftForeland extends AbstractFlowZoneType
{
  /**
   * @see org.kalypso.model.wspm.tuhh.ui.export.sobek.flowzones.IFlowZoneType#getLabel()
   */
  @Override
  public String getLabel( )
  {
    return Messages.getString( "LeftForeland_0" ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.export.sobek.flowzones.IFlowZoneType#createFlowZone(org.kalypso.model.wspm.core.profil.IProfil)
   */
  @Override
  public FlowZone createFlowZone( final IProfil profile )
  {
    final IProfilPointMarker[] bvMarkers = profile.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_BORDVOLL );
    final IProfilPointMarker[] dbMarkers = profile.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE );
    if( bvMarkers == null || bvMarkers.length != 2 )
      return null;
    if( dbMarkers == null || dbMarkers.length != 2 )
      return null;

    final IProfilPointMarker leftMarker = dbMarkers[0];
    final IProfilPointMarker rightMarker = bvMarkers[0];

    return createZone( profile, leftMarker, rightMarker, getLabel() );
  }

  public static FlowZone createZone( final IProfil profile, final IProfilPointMarker leftMarker, final IProfilPointMarker rightMarker, final String label )
  {
    final int indexWidth = profile.indexOfProperty( IWspmConstants.POINT_PROPERTY_BREITE );
    final double left = ((Number) leftMarker.getPoint().getValue( indexWidth )).doubleValue();
    final double right = ((Number) rightMarker.getPoint().getValue( indexWidth )).doubleValue();
    return new FlowZone( left, right, label );
  }

}
