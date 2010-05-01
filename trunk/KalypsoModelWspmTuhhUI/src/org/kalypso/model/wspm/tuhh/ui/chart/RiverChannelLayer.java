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
package org.kalypso.model.wspm.tuhh.ui.chart;

import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.ui.view.ILayerStyleProvider;
import org.kalypso.observation.result.IRecord;

/**
 * @author kimwerner
 */
public class RiverChannelLayer extends PointMarkerLayer
{
  public RiverChannelLayer( final IProfil profil, final ILayerStyleProvider styleProvider, final int offset, final boolean close )
  {
    super( profil, IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE, styleProvider, offset, close );
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.chart.PointMarkerLayer#moveDevider(org.kalypso.model.wspm.core.profil.IProfilPointMarker,
   *      org.kalypso.observation.result.IRecord)
   */
  @Override
  protected void moveDevider( final IProfilPointMarker devider, final IRecord newPoint )
  {
    final boolean isKeepChannelRoughness = true;
    if( isKeepChannelRoughness )
    {
      final RoughnessAdjuster mover = new RoughnessAdjuster( getProfil(), devider );
      mover.move( newPoint );
    }

    super.moveDevider( devider, newPoint );
  }

}
