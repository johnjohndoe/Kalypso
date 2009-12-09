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
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.model.wspm.core.IWspmConstants;

/**
 * @author kimwerner
 */
public class LengthSectionExportPage extends WizardPage
{
  private static final String SETTINGS_FILE_PATH = "filePath"; //$NON-NLS-1$

  private Text m_text;

  private File m_dir;

  private Button m_plotterCheck;

  public LengthSectionExportPage( final String title, final String description, final String error )
  {
    super( "lengthSectionExportPage" );

    setTitle( title );
    setErrorMessage( error );
    setDescription( description );

    setPageComplete( false );
  }

  protected final String findPlotter( final Shell shell )
  {

    String plotterPath = KalypsoCorePlugin.getDefault().getPreferenceStore().getString( IWspmConstants.WSPWIN_PLOTTER_PATH );

    if( !plotterPath.equals( "" ) )
      return plotterPath;

    final FileDialog dlg = new FileDialog( shell );
    plotterPath = dlg.open();
    if( plotterPath == null )
      return "";
    KalypsoCorePlugin.getDefault().getPreferenceStore().setValue( IWspmConstants.WSPWIN_PLOTTER_PATH, plotterPath );
    return plotterPath;
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    final Composite group = new Composite( parent, SWT.NONE );

    /* Configuring the group. */
    group.setLayout( new GridLayout( 3, false ) );

    /* The label for the path */
    final Label label = new Label( group, SWT.NONE );
    label.setText( "LengthSectionFileChooserLabel" );
    label.setToolTipText( "LengthSectionFileChooserDescription" );

    /* The text field for the path. */
    final Text text = new Text( group, SWT.BORDER );
    m_text = text;
    m_text.setLayoutData( new GridData( SWT.FILL, SWT.NONE, true, false ) );
    m_text.setText( "" ); //$NON-NLS-1$
    m_text.setToolTipText( "LengthSectionFileChooserTextDescription" );
    m_text.setEnabled( getErrorMessage().equals( "" ) );//$NON-NLS-1$

    /* The button for opening the FileDialog. */
    final Button button = new Button( group, SWT.NONE );
    button.setLayoutData( new GridData( SWT.CENTER, SWT.CENTER, false, false ) );
    button.setText( "..." ); //$NON-NLS-1$
    button.setEnabled( getErrorMessage().equals( "" ) );//$NON-NLS-1$

    m_text.addModifyListener( new ModifyListener()
    {
      public void modifyText( ModifyEvent e )
      {
        final File export_file = new File( text.getText() );

        setErrorMessage( null );

        /* The wizard-page could be completed. */
        setPageComplete( true );
        setDir( export_file );
      }
    } );

    final IDialogSettings dialogSettings = getDialogSettings();
    if( dialogSettings != null && (dialogSettings.get( SETTINGS_FILE_PATH ) != null) && (!dialogSettings.get( SETTINGS_FILE_PATH ).equals( "" )) ) //$NON-NLS-1$
      m_text.setText( dialogSettings.get( SETTINGS_FILE_PATH ) );

    button.addSelectionListener( new SelectionListener()
    {
      /* ÷ffnen des FileDialogs und setzen des Pfades in das Textfeld. */
      public void widgetSelected( SelectionEvent e )
      {
        final FileDialog dialog = new FileDialog( group.getShell() );

        dialog.setText( "LengthSectionDialogText" );

        final File f = new File( getText() );
        dialog.setFilterPath( f.getPath() );

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
    m_plotterCheck = new Button( group, SWT.CHECK );
    m_plotterCheck.setSelection( false );
    m_plotterCheck.setText( "PlotterCheck?" ); 
    m_plotterCheck.setToolTipText( "PlotterCheck?Description" );
    m_plotterCheck.setEnabled( getErrorMessage().equals( "" ) );//$NON-NLS-1$
    m_plotterCheck.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        text.setText( findPlotter( parent.getShell() ) );
      }
    } );
//    final Label labelcheck = new Label( group, SWT.NONE );
//    labelcheck.setText( "PlotterCheck?" );
//    labelcheck.setToolTipText( "PlotterCheck?Description" );
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

  public boolean OpenPlotter( )
  {
    return m_plotterCheck.getSelection();
  }

  public File getSourceDirectory( )
  {
    return m_dir;
  }
}
