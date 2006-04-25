/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ui.preferences;

import java.util.Arrays;
import java.util.TimeZone;

import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.kalypso.contribs.eclipse.jface.preference.ComboStringFieldEditor;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * This class represents a preference page that is contributed to the Preferences dialog. By subclassing
 * <samp>FieldEditorPreferencePage </samp>, we can use the field support built into JFace that allows us to create a
 * page that is small and knows how to save, restore and apply itself.
 * <p>
 * This page is used to modify preferences only. They are stored in the preference store that belongs to the main
 * plug-in class. That way, preferences can be accessed directly via the preference store.
 */
public class KalypsoPreferencePage extends FieldEditorPreferencePage implements IWorkbenchPreferencePage
{
  public KalypsoPreferencePage( )
  {
    super( GRID );
    setPreferenceStore( KalypsoGisPlugin.getDefault().getPreferenceStore() );
    setDescription( "Hier können Sie die Kalypso-Einstellungen ändern" );
  }

  /**
   * Creates the field editors. Field editors are abstractions of the common GUI blocks needed to manipulate various
   * types of preferences. Each field editor knows how to save and restore itself.
   */
  @Override
  public void createFieldEditors( )
  {
    final StringFieldEditor sfeSrv = new StringFieldEditor( IKalypsoPreferences.CLIENT_CONF_URLS, "Verfügbare KALYPSO-&Server:", getFieldEditorParent() );
    addField( sfeSrv );
    sfeSrv.getLabelControl( getFieldEditorParent() ).setToolTipText( "Entweder nur ein Server oder eine Komma-getrennte Liste von Server (Beispiel: http://SERVER_NAME:8080/webdav/kalypso-client.ini)" );

    addField( new BooleanFieldEditor( IKalypsoPreferences.HTTP_PROXY_USE, "Http-&Proxy benutzen", getFieldEditorParent() ) );
    addField( new StringFieldEditor( IKalypsoPreferences.HTTP_PROXY_HOST, "Http-Proxy &Hostname:", getFieldEditorParent() ) );
    addField( new StringFieldEditor( IKalypsoPreferences.HTTP_PROXY_PORT, "Http-Proxy Port&nummer:", getFieldEditorParent() ) );
    addField( new StringFieldEditor( IKalypsoPreferences.HTTP_PROXY_USER, "Http-Proxy &Benutzername:", getFieldEditorParent() ) );

    // set echo char because it is a password field
    final StringFieldEditor editor = new StringFieldEditor( IKalypsoPreferences.HTTP_PROXY_PASS, "Http-Proxy Pass&wort:", getFieldEditorParent() );
    editor.getTextControl( getFieldEditorParent() ).setEchoChar( '*' );

    final StringFieldEditor sfeCrs = new StringFieldEditor( IKalypsoPreferences.GLOBAL_CRS, "Globales &Koordinatensystem:", getFieldEditorParent() );
    addField( sfeCrs );
    sfeCrs.getLabelControl( getFieldEditorParent() ).setToolTipText( "" ); // TODO tooltip angeben

    // fetch list of timezone names and sort it
    final String[] ids = TimeZone.getAvailableIDs();
    Arrays.sort( ids );

    final ComboStringFieldEditor timeZoneFieldEditor = new ComboStringFieldEditor( IKalypsoPreferences.DISPLAY_TIMEZONE, "Zeitzone für die Darstellung:", "Gilt für Diagramme und Tabellen, aus der Liste selektieren oder z.B. 'GMT+1' für Winterzeit Deutschland, oder leer lassen", getFieldEditorParent(), false, ids );
    addField( timeZoneFieldEditor );
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
   */
  public void init( IWorkbench workbench )
  {
    // empty
  }

  /**
   * @see org.eclipse.jface.preference.IPreferencePage#performOk()
   */
  @Override
  public boolean performOk( )
  {
    final boolean result = super.performOk();

    // even if on shutdown the preferences are saved, we save them in case of a
    // platfrom crash
    KalypsoGisPlugin.getDefault().savePluginPreferences();

    return result;
  }
}