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
package org.kalypso.model.wspm.tuhh.ui.internal.preferences;

import java.util.ArrayList;
import java.util.Collection;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.ComboFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;
import org.kalypso.model.wspm.ui.preferences.WspmUiPreferences;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;

/**
 * This class represents a preference page that is contributed to the Preferences dialog. By subclassing
 * <samp>FieldEditorPreferencePage </samp>, we can use the field support built into JFace that allows us to create a
 * page that is small and knows how to save, restore and apply itself.
 * <p>
 * This page is used to modify preferences only. They are stored in the preference store that belongs to the main plug-in class. That way, preferences can be accessed directly via the preference
 * store.
 */
public class WspmChartPreferencePage extends FieldEditorPreferencePage implements IWorkbenchPreferencePage
{
  private BooleanFieldEditor m_keepRoughnessEditor;

  private ComboFieldEditor m_waterlevelEditor;

  // FIXME: delete data class
//  private final WspmTuhhUiPreferenceData m_data = new WspmTuhhUiPreferenceData();

  public WspmChartPreferencePage( )
  {
    super( GRID );

    // FIXME: remove properties Messages.getString( "WspmTuhhUiPreferencePage_0" );; //$NON-NLS-1$

    setPreferenceStore( KalypsoModelWspmUIPlugin.getDefault().getPreferenceStore() );
    setDescription( Messages.getString( "WspmTuhhUiPreferencePage_1" ) ); //$NON-NLS-1$
  }

  @Override
  public void init( final IWorkbench workbench )
  {
  }

  /**
   * Initializes all field editors.
   */
  @Override
  protected void initialize( )
  {
    super.initialize();

    m_keepRoughnessEditor.setPreferenceStore( KalypsoModelWspmTuhhUIPlugin.getDefault().getPreferenceStore() );
    m_keepRoughnessEditor.load();
  }

  @Override
  public void createFieldEditors( )
  {
    /* keep roughness */
    final String keepRoughnessLabel = Messages.getString("WspmChartPreferencePage.0"); //$NON-NLS-1$
    m_keepRoughnessEditor = new BooleanFieldEditor( WspmTuhhUiPreferences.KEEP_CHANNEL_ROUGHNESS, keepRoughnessLabel, getFieldEditorParent() ); //$NON-NLS-1$
    addField( m_keepRoughnessEditor );

    /* water level restriction */
    //final String tooltip = String.format( Messages.getString( "WspmTuhhUiPreferencePage_2" ) ); //$NON-NLS-1$
    final String label = Messages.getString( "WspmTuhhUiPreferencePage_3" ); //$NON-NLS-1$

    final String[][] entryNamesAndValues = createWaterlevelRestrictionEntriesAndNames();

    m_waterlevelEditor = new ComboFieldEditor( WspmUiPreferences.WATERLEVEL_RESTRICTION_MARKER, label, entryNamesAndValues, getFieldEditorParent() ); //$NON-NLS-1$
    addField( m_waterlevelEditor );
  }

  private String[][] createWaterlevelRestrictionEntriesAndNames( )
  {
    final Collection<String[]> input = new ArrayList<>();

    input.add( new String[] { Messages.getString( "WspmTuhhUiPreferencePage_4" ), StringUtils.EMPTY } ); //$NON-NLS-1$

    final IComponent markerComponent = ComponentUtilities.getFeatureComponent( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE );
    final String labelDurchstroemte = ComponentUtilities.getComponentName( markerComponent );
    input.add( new String[] { labelDurchstroemte, IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE } );

    return input.toArray( new String[input.size()][] );
  }
}