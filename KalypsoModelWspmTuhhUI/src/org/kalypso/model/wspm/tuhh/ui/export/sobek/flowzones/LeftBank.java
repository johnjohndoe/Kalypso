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
package org.kalypso.model.wspm.tuhh.ui.export.sobek.flowzones;

import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfilePointMarker;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class LeftBank extends AbstractFlowZoneType
{
  /**
   * @see org.kalypso.model.wspm.tuhh.ui.export.sobek.flowzones.IFlowZoneType#getLabel()
   */
  @Override
  public String getLabel( )
  {
    return Messages.getString( "LeftBank_0" ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.export.sobek.flowzones.IFlowZoneType#createFlowZone(org.kalypso.model.wspm.core.profil.IProfil)
   */
  @Override
  public FlowZone createFlowZone( final IProfile profile )
  {
    final IProfilePointMarker[] tfMarkers = profile.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
    final IProfilePointMarker[] bvMarkers = profile.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_BORDVOLL );
    if( tfMarkers == null || tfMarkers.length != 2 )
      return null;
    if( bvMarkers == null || bvMarkers.length != 2 )
      return null;

    final IProfilePointMarker leftMarker = bvMarkers[0];
    final IProfilePointMarker rightMarker = tfMarkers[0];

    return createZone( profile, leftMarker, rightMarker, getLabel() );
  }

  public static FlowZone createZone( final IProfile profile, final IProfilePointMarker leftMarker, final IProfilePointMarker rightMarker, final String label )
  {
    final int indexWidth = profile.indexOfProperty( IWspmConstants.POINT_PROPERTY_BREITE );
    final double left = ((Number) leftMarker.getPoint().getValue( indexWidth )).doubleValue();
    final double right = ((Number) rightMarker.getPoint().getValue( indexWidth )).doubleValue();
    return new FlowZone( left, right, label );
  }

}
