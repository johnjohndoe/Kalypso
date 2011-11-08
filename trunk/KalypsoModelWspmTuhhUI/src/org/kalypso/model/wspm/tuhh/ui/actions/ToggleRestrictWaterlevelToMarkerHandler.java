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
package org.kalypso.model.wspm.tuhh.ui.actions;

import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.HandlerEvent;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.ui.commands.IElementUpdater;
import org.eclipse.ui.menus.UIElement;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;
import org.kalypso.model.wspm.ui.preferences.Preferences;

/**
 * @author Gernot Belger
 */
public class ToggleRestrictWaterlevelToMarkerHandler extends AbstractHandler implements IElementUpdater
{
  private final IPropertyChangeListener m_changeListener = new IPropertyChangeListener()
  {
    @Override
    public void propertyChange( final PropertyChangeEvent event )
    {
      handlePreferenceChanged();
    }
  };

  public ToggleRestrictWaterlevelToMarkerHandler( )
  {
    final IPreferenceStore store = KalypsoModelWspmUIPlugin.getDefault().getPreferenceStore();

    store.addPropertyChangeListener( m_changeListener );
  }

  @Override
  public void dispose( )
  {
    final IPreferenceStore store = KalypsoModelWspmUIPlugin.getDefault().getPreferenceStore();
    store.removePropertyChangeListener( m_changeListener );

    super.dispose();
  }

  @Override
  public Object execute( final ExecutionEvent event )
  {
    final String oldValue = Preferences.getWaterlevelRestrictionMarker();
    final String newValue = StringUtils.isEmpty( oldValue ) ? IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE : StringUtils.EMPTY;

    Preferences.setWaterlevelRestrictionMarker( newValue );

    return null;
  }

  @Override
  public void updateElement( final UIElement element, @SuppressWarnings("rawtypes") final Map parameters )
  {
    final boolean isOn = Preferences.getWaterlevelRestrictionMarker() != null;
    element.setChecked( isOn );
  }

  protected void handlePreferenceChanged( )
  {
    fireHandlerChanged( new HandlerEvent( this, true, true ) );
  }
}