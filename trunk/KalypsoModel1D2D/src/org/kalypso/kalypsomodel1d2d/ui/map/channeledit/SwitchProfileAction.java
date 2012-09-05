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
package org.kalypso.kalypsomodel1d2d.ui.map.channeledit;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.jface.action.Action;
import org.eclipse.swt.widgets.Event;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.editdata.ChannelEditProfileData;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.editdata.IProfileData;

/**
 * Swtiches between profiles in the profile view.
 *
 * @author Gernot Belger
 */
class SwitchProfileAction extends Action
{
  private final CreateChannelData m_data;

  private final int m_offset;

  public SwitchProfileAction( final CreateChannelData data, final int offset )
  {
    m_data = data;
    m_offset = offset;

    m_data.addPropertyChangeListener( CreateChannelData.PROPERTY_ACTIVE_PROFILE, new PropertyChangeListener()
    {
      @Override
      public void propertyChange( final PropertyChangeEvent evt )
      {
        handleActiveProfileChanged();
      }
    } );
  }

  protected void handleActiveProfileChanged( )
  {
    final IProfileData nextProfile = findNextProfile();
    final boolean enabled = nextProfile != null;
    setEnabled( enabled );
  }

  @Override
  public void runWithEvent( final Event event )
  {
    final IProfileData nextProfile = findNextProfile();
    m_data.setActiveProfile( nextProfile );
  }

  private IProfileData findNextProfile( )
  {
    final IProfileData activeProfile = m_data.getActiveProfile();
    if( activeProfile == null )
      return null;

    final ChannelEditProfileData editData = m_data.getEditData();
    if( editData == null )
      return null;

    final IProfileData[] profiles = editData.getProfiles();
    final int index = ArrayUtils.indexOf( profiles, activeProfile );
    if( index == -1 )
      return null;

    final int nextIndex = index + m_offset;
    if( nextIndex < 0 || nextIndex >= profiles.length )
      return null;

    return profiles[nextIndex];
  }
}