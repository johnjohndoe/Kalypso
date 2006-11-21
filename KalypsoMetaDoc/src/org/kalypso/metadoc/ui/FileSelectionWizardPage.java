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
package org.kalypso.metadoc.ui;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.apache.commons.configuration.Configuration;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.kalypso.contribs.java.io.FileUtilities;
import org.kalypso.metadoc.configuration.IConfigurationListener;
import org.kalypso.metadoc.configuration.IPublishingConfiguration;
import org.kalypso.metadoc.impl.FileExportTarget;

/**
 * @author belger
 */
public class FileSelectionWizardPage extends WizardPage implements IConfigurationListener
{
  private final static String STORE_DESTINATION_NAMES_ID = "FileSelectionWizardPage.STORE_DESTINATION_NAMES_ID"; //$NON-NLS-1$

  protected static final int SIZING_TEXT_FIELD_WIDTH = 250;

  private static final int COMBO_HISTORY_LENGTH = 5;

  private Combo m_destinationNameField;

  private final String m_groupname;

  private final IPublishingConfiguration m_conf;

  public FileSelectionWizardPage( final IPublishingConfiguration conf, final String pageName, final String title,
      final ImageDescriptor titleImage, final String groupname )
  {
    super( pageName, title, titleImage );

    m_conf = conf;
    m_conf.addListener( this ); // removed in dispose()
    m_groupname = groupname;
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#dispose()
   */
  public void dispose()
  {
    m_conf.removeListener( this );

    super.dispose();
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    panel.setLayout( new GridLayout() );

    panel.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    panel.setFont( parent.getFont() );

    createExportTargetGroup( panel );

    setControl( panel );

    restoreWidgetValues();

    updateControl( null );

    updatePageCompletion();
  }

  /**
   * conf -> gui-elements
   * <p>
   * Must be called in swt-thread
   * </p>
   */
  private void updateControl( final String key )
  {
    if( m_destinationNameField.isDisposed() )
      return;

    final File destination = (File)m_conf.getProperty( FileExportTarget.CONF_FILEEXPORT_FILE );
    
    if( destination != null && !m_destinationNameField.getText().equals( destination.getPath() ) )
      m_destinationNameField.setText( destination.getPath() );
    
    if( key == null || key.equals( FileExportTarget.CONF_FILEEXPORT_EXTENSION ) )
    {
      String filename = m_destinationNameField.getText();
      filename = FileUtilities.nameWithoutExtension( filename ) + m_conf.getString( FileExportTarget.CONF_FILEEXPORT_EXTENSION, "" );
      
      m_destinationNameField.setText( filename );
    }
  }

  protected File updatePageCompletion()
  {
    final File file = determinePageCompletion();

    setPageComplete( file != null );
    if( file != null )
    {
      setErrorMessage( null );
      setMessage( null );
    }

    return file;
  }

  private File determinePageCompletion()
  {
    final String destinationValue = getDestinationValue();
    if( destinationValue.length() == 0 )
    {
      setMessage( "Bitte geben Sie den Namen der Exportdatei an." );
      setErrorMessage( null );
      return null;
    }

    if( isContainerConflicting( destinationValue ) )
    {
      setErrorMessage( "Exportdatei liegt innerhalb des Arbeitsbereichs" );
      m_destinationNameField.setFocus();
      return null;
    }

    final File file = new File( destinationValue );
    if( file.isDirectory() )
    {
      setErrorMessage( "Eingegebener Pfad bezeichnet ein bereits bestehendes Verzeichnis" );
      m_destinationNameField.setFocus();
      return null;
    }

    return file;
  }

  /**
   * Add the passed value to self's destination widget's history
   * 
   * @param value
   *          java.lang.String
   */
  protected void addDestinationItem( String value )
  {
    m_destinationNameField.add( value );
  }

  /**
   * Returns the name of a container with a location that encompasses targetDirectory. Returns null if there is no
   * conflict.
   * 
   * @param targetDirectory
   *          the path of the directory to check.
   * @return the conflicting container name or <code>null</code>
   */
  protected boolean isContainerConflicting( String targetDirectory )
  {
    //    IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
    //    IPath testPath = new Path( targetDirectory );
    //
    //    if( root.getLocation().isPrefixOf( testPath ) )
    //      return true;
    //
    //    IProject[] projects = root.getProjects();
    //
    //    for( int i = 0; i < projects.length; i++ )
    //    {
    //      if( projects[i].getLocation().isPrefixOf( testPath ) )
    //        return true;
    //    }
    //
    return false;
  }

  private void createExportTargetGroup( final Composite parent )
  {
    final Font font = parent.getFont();

    final Group targetGroup = new Group( parent, SWT.NONE );
    final GridLayout layout = new GridLayout( 3, false );
    targetGroup.setLayout( layout );
    targetGroup.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    targetGroup.setFont( font );

    targetGroup.setText( m_groupname );

    final Label filenameLabel = new Label( targetGroup, SWT.NONE );
    filenameLabel.setText( "Dateiname:" );

    // destination name entry field
    m_destinationNameField = new Combo( targetGroup, SWT.BORDER );
    m_destinationNameField.addModifyListener( new ModifyListener()
    {
      public void modifyText( final ModifyEvent e )
      {
        handleDestinationNameChanged( m_destinationNameField.getText() );
      }
    } );

    final GridData data = new GridData( GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL );
    data.widthHint = SIZING_TEXT_FIELD_WIDTH;
    m_destinationNameField.setLayoutData( data );
    m_destinationNameField.setFont( font );

    // destination browse button
    final Button destinationBrowseButton = new Button( targetGroup, SWT.PUSH );
    destinationBrowseButton.setText( "Auswählen..." );
    destinationBrowseButton.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      public void widgetSelected( SelectionEvent e )
      {
        handleDestinationBrowseButtonPressed();
      }
    } );
    destinationBrowseButton.setFont( font );
  }

  protected void handleDestinationNameChanged( final String text )
  {
    final File file = updatePageCompletion();

    m_conf.setProperty( FileExportTarget.CONF_FILEEXPORT_FILE, file );
    saveWidgetValues();
  }

  protected void handleDestinationBrowseButtonPressed()
  {
    final FileDialog dialog = new FileDialog( getContainer().getShell(), SWT.SAVE );

    // TODO: fill in filter names and exts
    final String[] filterExts = new String[]
    { "*" + m_conf.getString( FileExportTarget.CONF_FILEEXPORT_EXTENSION ), "*" };
    final String[] filterNames = new String[]
    { "*" + m_conf.getString( FileExportTarget.CONF_FILEEXPORT_EXTENSION ), "Alle Dateien" };

    dialog.setFilterExtensions( filterExts );
    dialog.setFilterNames( filterNames );

    dialog.setText( "Datei speichern" );
    final String currentSourceString = getDestinationValue();
    int lastSeparatorIndex = currentSourceString.lastIndexOf( File.separator );
    if( lastSeparatorIndex != -1 )
    {
      dialog.setFilterPath( currentSourceString.substring( 0, lastSeparatorIndex ) );
      dialog.setFileName( currentSourceString.substring( lastSeparatorIndex + 1 ) );
    }

    final String selectedFileName = dialog.open();

    if( selectedFileName != null )
    {
      setErrorMessage( null );
      setDestinationValue( selectedFileName );
    }
  }

  private void setDestinationValue( final String selectedFileName )
  {
    if( m_destinationNameField.getText().equals( selectedFileName ) )
      return;

    m_destinationNameField.setText( selectedFileName );
  }

  public String getDestinationValue()
  {
    return m_destinationNameField.getText().trim();
  }

  /**
   * Hook method for restoring widget values to the values that they held last time this wizard was used to completion.
   */
  protected void restoreWidgetValues()
  {
    final IDialogSettings settings = getDialogSettings();
    if( settings != null )
    {
      final String[] directoryNames = settings.getArray( STORE_DESTINATION_NAMES_ID );
      if( directoryNames == null || directoryNames.length == 0 )
        return; // ie.- no settings stored

      // destination
      setDestinationValue( directoryNames[0] );
      for( int i = 0; i < directoryNames.length; i++ )
        addDestinationItem( directoryNames[i] );
    }
  }

  public void saveWidgetValues()
  {
    // update directory names history
    final IDialogSettings settings = getDialogSettings();
    if( settings != null )
    {
      final List history = new ArrayList( Arrays.asList( m_destinationNameField.getItems() ) );
      history.remove( getDestinationValue() );
      history.add( 0, getDestinationValue() );

      // since only one new item was added, we can be over the limit
      // by at most one item
      if( history.size() > COMBO_HISTORY_LENGTH )
        history.remove( COMBO_HISTORY_LENGTH );

      settings.put( STORE_DESTINATION_NAMES_ID, (String[])history.toArray( new String[history.size()] ) );
    }
  }

  /**
   * @see org.kalypso.metadoc.configuration.IConfigurationListener#configurationChanged(org.apache.commons.configuration.Configuration,
   *      java.lang.String)
   */
  public void configurationChanged( final Configuration config, final String key )
  {
    final Control control = getControl();
    if( control == null )
      return;
    
    final Display display = control.getDisplay();
    if( !display.isDisposed() )
    {
      display.asyncExec( new Runnable()
      {
        public void run()
        {
          updateControl( key );
        }
      } );
    }
  }
}