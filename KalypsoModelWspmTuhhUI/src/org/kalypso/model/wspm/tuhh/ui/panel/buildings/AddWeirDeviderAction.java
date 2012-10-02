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
package org.kalypso.model.wspm.tuhh.ui.panel.buildings;

import org.eclipse.jface.action.Action;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfilePointMarker;
import org.kalypso.model.wspm.core.profil.changes.ActiveObjectEdit;
import org.kalypso.model.wspm.core.profil.changes.PointMarkerEdit;
import org.kalypso.model.wspm.core.profil.operation.ProfileOperation;
import org.kalypso.model.wspm.core.profil.operation.ProfileOperationJob;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingWehr;
import org.kalypso.model.wspm.tuhh.core.util.river.line.WspmSohlpunkte;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIImages;

/**
 * @author Gernot Belger
 */
class AddWeirDeviderAction extends Action
{
  private final int m_deviderID;

  private final IProfile m_profile;

  private final String m_componentID;

  public AddWeirDeviderAction( final IProfile profile, final int deviderID, final String componentID, final boolean canAdd )
  {
    m_profile = profile;
    m_deviderID = deviderID;
    m_componentID = componentID;

    setToolTipText( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.WeirPanel.7" ) ); //$NON-NLS-1$
    setImageDescriptor( KalypsoModelWspmUIImages.ID_BUTTON_WEHR_ADD );
    setEnabled( canAdd && deviderID > -1 );
  }

  @Override
  public void run( )
  {
    final IProfilePointMarker marker = m_profile.getPointMarkerFor( m_componentID )[m_deviderID];

    final IProfileRecord point = m_profile.getPoint( marker.getPoint().getIndex() + 1 );

    final ProfileOperation operation = new ProfileOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.WeirPanel.8" ), m_profile, true ); //$NON-NLS-1$
    final IProfilePointMarker trenner = m_profile.createPointMarker( IWspmTuhhConstants.MARKER_TYP_WEHR, point );

    if( trenner != null )
    {
      final Object objVal = marker.getValue();

      final BuildingWehr building = WspmSohlpunkte.getBuilding( m_profile, BuildingWehr.class );
      if( building == null )
        return;

      final Object dblVal = objVal instanceof Double ? objVal : building.getFormbeiwert();
      operation.addChange( new PointMarkerEdit( trenner, dblVal ) );
      operation.addChange( new ActiveObjectEdit( m_profile, point.getBreiteAsRange(), null ) );
      new ProfileOperationJob( operation ).schedule();
    }
  }
}