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
package org.kalypso.model.wspm.tuhh.ui.wizards;

import java.io.File;

import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Text;

/**
 * @author albert This wizard-page imports the hydraulic data for a spezific InformDSS project.
 */
public class WspWinImportSourcePage extends WizardPage
{
  private static final String SETTINGS_FILE_PATH = "filePath";

  private Text m_text;

  protected static String KWERT_PRPOPERTIES_FILE = "kwert-project.properties";

  protected static String HYDRAULIC_PRPOPERTIES_FILE = "hydraulic.properties";

  private File m_dir;

  public WspWinImportSourcePage( final String pageName )
  {
    super( pageName );

    setTitle( "WspWin Daten importieren" );
    setDescription( "In diesem Dialog w‰hlen Sie das WspWin Projekt aus." );

    setPageComplete( false );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    final Composite group = new Composite( parent, SWT.NONE );

    /* Configuring the group. */
    group.setLayout( new GridLayout( 2, false ) );

    /* The label for the path. */
    final Text text = new Text( group, SWT.BORDER );
    m_text = text;
    m_text.setLayoutData( new GridData( SWT.FILL, SWT.NONE, true, false ) );
    m_text.setText( "" );

    /* The button for opening the FileDialog. */
    final Button button = new Button( group, SWT.NONE );
    button.setLayoutData( new GridData( SWT.CENTER, SWT.CENTER, false, false ) );
    button.setText( "..." );

    m_text.addModifyListener( new ModifyListener()
    {
      public void modifyText( ModifyEvent e )
      {
        final File kwert_file = new File( text.getText() );

        /* Existiert die Datei? */
        if( !kwert_file.exists() )
        {
          /* Die Datei wurde nicht gefunden. */
          setErrorMessage( "Die ausgew‰hlte Datei existiert nicht." );

          /* The wizard-page could not be completed. */
          setPageComplete( false );

          setDir( null );

          return;
        }

        /* Ist die Datei ein Verzeichnis? */
        if( kwert_file.isFile() )
        {
          /* Die Datei wurde nicht gefunden. */
          setErrorMessage( "Es wurde muss Verzeichnis ausgew‰hlt werden." );

          /* The wizard-page could not be completed. */
          setPageComplete( false );

          setDir( null );

          return;
        }

        /* Ists ein WspWin Verzeichnis? */
        final File profDir = new File( kwert_file, "prof" );
        final File wspcfgfile = new File( profDir, "wsp.cfg" );
        if( !profDir.exists() || !wspcfgfile.exists() )
        {
          setErrorMessage( "Kein WspWin Projektverzeichnis ausgew‰hlt (Verzeichnis enth‰lt keine Datei prof/wsp.cfg)" ); //$NON-NLS-1$
          setPageComplete( false );

          setDir( null );
          return;
        }

        setErrorMessage( null );

        /* The wizard-page could be completed. */
        setPageComplete( true );
        setDir( kwert_file );
      }
    } );

    final IDialogSettings dialogSettings = getDialogSettings();
    if( dialogSettings != null && (dialogSettings.get( SETTINGS_FILE_PATH ) != null) && (!dialogSettings.get( SETTINGS_FILE_PATH ).equals( "" )) )
      m_text.setText( dialogSettings.get( SETTINGS_FILE_PATH ) );

    button.addSelectionListener( new SelectionListener()
    {
      /* ÷ffnen des FileDialogs und setzen des Pfades in das Textfeld. */
      public void widgetSelected( SelectionEvent e )
      {
        final DirectoryDialog dialog = new DirectoryDialog( group.getShell() );

        dialog.setText( "Pfad zu den Daten der Hydraulik" );

        final File f = new File( getText() );
        dialog.setFilterPath( f.getPath() );
        dialog.setMessage( "W‰hlen Sie ein WspWin PRojekt Verzeichnis aus. Das Verzeichnis muss die Unterverzeichnisse 'prof' und 'dath' enthalten." );

        final String loadPath = dialog.open();

        if( loadPath == null )
          return;

        if( dialogSettings != null )
          dialogSettings.put( SETTINGS_FILE_PATH, loadPath );

        text.setText( loadPath );
      }

      public void widgetDefaultSelected( SelectionEvent e )
      {
      }
    } );

    setControl( group );
  }

  protected void setDir( final File dir )
  {
    m_dir = dir;
  }

  protected String getText( )
  {
    return m_text.getText();
  }

  public File getSourceDirectory( )
  {
    return m_dir;
  }
}
