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
package org.kalypso.ui.repository.factory;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.jface.preference.DirectoryFieldEditor;
import org.eclipse.jface.preference.PreferenceStore;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.plugin.AbstractUIPlugin;

/**
 * Config Dialog for FileRepositoryFactory.
 * 
 * TODO: use plugin's dialog store to store default values
 * 
 * @author schlienger
 */
public class FileRepositoryConfigDialog extends TitleAreaDialog
{
  private final static String msg = "Bitte w�hlen Sie zuerst ein Basisverzeichnis aus.\n"
      + "Geben Sie anschliessend einen Kennzeichen und eine oder mehrere Dateiendungen (Komma getrennt) ein.";

  protected final static String BASEDIR = "FileRepositoryConfigDialog.basedir";

  protected final static String IDENTIFIER = "FileRepositoryConfigDialog.identifier";

  protected final static String FILTER = "FileRepositoryConfigDialog.filter";

  private final AbstractUIPlugin m_plugin;

  private final PreferenceStore m_store;

  private StringFieldEditor m_fFilters;

  private StringFieldEditor m_fIdentifier;

  private DirectoryFieldEditor m_fLocation;

  /**
   * Constructor. If plugin is specified, it uses the values provided by its DialogSettings as default values for this
   * dialog.
   * 
   * @param parentShell
   * @param location
   * @param identifier
   * @param filters
   * @param plugin
   *          [optional] if not null, default values are used
   */
  public FileRepositoryConfigDialog( final Shell parentShell, String location, String identifier, String filters,
      final AbstractUIPlugin plugin )
  {
    super( parentShell );

    m_plugin = plugin;

    if( plugin != null )
    {
      String s = plugin.getDialogSettings().get( BASEDIR );
      location = s == null ? location : s;

      s = plugin.getDialogSettings().get( IDENTIFIER );
      identifier = s == null ? identifier : s;

      s = plugin.getDialogSettings().get( FILTER );
      filters = s == null ? filters : s;
    }

    m_store = new PreferenceStore();
    m_store.setDefault( BASEDIR, location );
    m_store.setDefault( IDENTIFIER, identifier );
    m_store.setDefault( FILTER, filters );
  }

  public void dispose()
  {
    m_fFilters.dispose();
    m_fIdentifier.dispose();
    m_fLocation.dispose();
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
   */
  protected Control createDialogArea( final Composite parent )
  {
    setTitle( "ZML-Repository Konfiguration" );
    setMessage( msg );

    final Composite c = (Composite)super.createDialogArea( parent );

    final Composite sub = new Composite( c, SWT.FILL );

    m_fLocation = new DirectoryFieldEditor( BASEDIR, "Basis-Verzeichnis:", sub );
    m_fIdentifier = new StringFieldEditor( IDENTIFIER, "Kennzeichen:", sub );
    m_fFilters = new StringFieldEditor( FILTER, "Dateiendung:", sub );

    m_fLocation.setPreferenceStore( m_store );
    m_fLocation.loadDefault();
    m_fLocation.setEmptyStringAllowed( false );

    m_fIdentifier.setPreferenceStore( m_store );
    m_fIdentifier.loadDefault();

    m_fFilters.setPreferenceStore( m_store );
    m_fFilters.loadDefault();

    final GridLayout gridLayout = new GridLayout( 5, true );
    sub.setLayout( gridLayout );
    sub.setLayoutData( new GridData( GridData.FILL_BOTH ) );

    m_fLocation.fillIntoGrid( sub, 5 );
    m_fIdentifier.fillIntoGrid( sub, 5 );
    m_fFilters.fillIntoGrid( sub, 3 );

    return c;
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#okPressed()
   */
  protected void okPressed()
  {
    if( !m_fLocation.isValid() )
    {
      MessageDialog.openInformation( getParentShell(), "Basis-Verzeichnis",
          "Bitte geben Sie einen g�ltigen Verzeichnis ein" );
      return;
    }

    if( !m_fIdentifier.isValid() )
    {
      MessageDialog.openInformation( getParentShell(), "Kennzeichen", "Bitte geben Sie einen g�ltigen Kennzeichen ein" );
      return;
    }

    if( !m_fFilters.isValid() )
    {
      MessageDialog.openInformation( getParentShell(), "Dateiendung", m_fFilters.getErrorMessage() );
      return;
    }

    m_fLocation.store();
    m_fIdentifier.store();
    m_fFilters.store();

    if( m_plugin != null )
    {
      m_plugin.getDialogSettings().put( BASEDIR, getLocation() );
      m_plugin.getDialogSettings().put( IDENTIFIER, getIdentifier() );
      m_plugin.getDialogSettings().put( FILTER, getFilters() );
    }

    super.okPressed();
  }

  public String getLocation()
  {
    return m_store.getString( BASEDIR );
  }

  public String getIdentifier()
  {
    return m_store.getString( IDENTIFIER );
  }

  public String getFilters()
  {
    return m_store.getString( FILTER );
  }
}