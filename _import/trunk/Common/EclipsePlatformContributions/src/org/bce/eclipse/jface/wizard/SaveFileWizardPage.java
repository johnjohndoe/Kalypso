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
package org.bce.eclipse.jface.wizard;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.StructuredViewer;
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
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;

/**
 * @author belger
 */
public class SaveFileWizardPage extends WizardPage
{
  private static final String STORE_DESTINATION_FORMAT_ID = "SaveFileWizardPage.STORE_DESTINATION_FORMAT_ID"; //$NON-NLS-1$

  private final static String STORE_DESTINATION_NAMES_ID = "SaveFileWizardPage.STORE_DESTINATION_NAMES_ID"; //$NON-NLS-1$

  protected static final int SIZING_TEXT_FIELD_WIDTH = 250;

  private static final int COMBO_HISTORY_LENGTH = 5;

  private Combo m_destinationNameField;

  private final String m_groupname;

  private final Map m_formats = new HashMap();

  private StructuredViewer m_formatViewer;

  /**
   * @param formats format objects -> file extension
   */
  public SaveFileWizardPage( final String pageName, final String title,
      final ImageDescriptor titleImage, final String groupname, final Map formats )
  {
    super( pageName, title, titleImage );

    m_groupname = groupname;
    m_formats.putAll( formats );
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

    updatePageCompletion();
  }

  protected void updatePageCompletion()
  {
    boolean pageComplete = determinePageCompletion();

    setPageComplete( pageComplete );
    if( pageComplete )
    {
      setErrorMessage( null );
      setMessage( null );
    }
  }

  private boolean determinePageCompletion()
  {
    final String destinationValue = getDestinationValue();
    if( destinationValue.length() == 0 )
    {
      setMessage( "Bitte geben Sie den Namen der Exportdatei an." );
      setErrorMessage( null );
      return false;
    }

    if( isContainerConflicting( destinationValue ) )
    {
      setErrorMessage( "Exportdatei liegt innerhalb des Arbeitsbereichs" );
      m_destinationNameField.setFocus();
      return false;
    }

    return true;
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
   * Returns the name of a container with a location that encompasses
   * targetDirectory. Returns null if there is no conflict.
   * 
   * @param targetDirectory
   *          the path of the directory to check.
   * @return the conflicting container name or <code>null</code>
   */
  protected boolean isContainerConflicting( String targetDirectory )
  {
    IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
    IPath testPath = new Path( targetDirectory );

    if( root.getLocation().isPrefixOf( testPath ) )
      return true;

    IProject[] projects = root.getProjects();

    for( int i = 0; i < projects.length; i++ )
    {
      if( projects[i].getLocation().isPrefixOf( testPath ) )
        return true;
    }

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
        updatePageCompletion();
      }
    } );
    // TODO?
    // destinationNameField.addListener( SWT.Selection, this );
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
    // setButtonLayoutData( destinationBrowseButton );
    // destinationBrowseButton.setVisible( true );

    final Label label = new Label( targetGroup, SWT.NONE );
    label.setText( "Format:" );

    m_formatViewer = new ComboViewer( targetGroup, SWT.BORDER | SWT.READ_ONLY | SWT.DROP_DOWN );
    m_formatViewer.setContentProvider( new ArrayContentProvider() );
    m_formatViewer.setLabelProvider( new LabelProvider() );
    m_formatViewer.setInput( m_formats.keySet() );
    ( (ComboViewer)m_formatViewer ).getCombo().addSelectionListener( new SelectionAdapter()
    {
      public void widgetSelected( SelectionEvent e )
      {
        handleComboSelected();
      }
    } );
    if( m_formats.size() > 0 )
      m_formatViewer.setSelection( new StructuredSelection( m_formats.keySet().iterator().next() ) );
  }

  protected void handleComboSelected()
  {
    final Object format = getDestinationFormat();
    final String ext = m_formats.get( format ).toString();
    
    final String destinationValue = getDestinationValue();
    final int index = destinationValue.lastIndexOf( '.' );
    if( index == -1 )
      setDestinationValue( destinationValue + "." + ext );
    else
      setDestinationValue( destinationValue.substring( 0, index ) + "." + ext );
  }

   void handleDestinationBrowseButtonPressed()
  {
    final FileDialog dialog = new FileDialog( getContainer().getShell(), SWT.SAVE );

    final String[] filterExts = new String[m_formats.size() + 1];
    final String[] filterNames = new String[m_formats.size() + 1];
    filterExts[0] = "*";
    filterNames[0] = "Alle Dateien";
    int count = 1;
    for( final Iterator fIt = m_formats.entrySet().iterator(); fIt.hasNext(); )
    {
      final Map.Entry entry = (Map.Entry)fIt.next();
      filterNames[count] = entry.getKey().toString();
      filterExts[count] = "*." + entry.getValue().toString();
      
      count++;
    }
    
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
      
      final String newFileName;
      if( selectedFileName.indexOf( '.' ) == -1 )
        newFileName = selectedFileName + "." + m_formats.get( getDestinationFormat() );
      else
        newFileName = selectedFileName;
      
      setDestinationValue( newFileName );
    }
  }

  private void setDestinationValue( final String selectedFileName )
  {
    if( m_destinationNameField.getText().equals( selectedFileName ) )
      return;
    
    m_destinationNameField.setText( selectedFileName );
    
    final int index = selectedFileName.lastIndexOf( '.' );
    if( index != -1 )
    {
      final String ext = selectedFileName.substring( index + 1 );
      for( Iterator fIt = m_formats.entrySet().iterator(); fIt.hasNext(); )
      {
        final Map.Entry entry = (Entry)fIt.next();
        if( entry.getKey().equals( ext ) )
          setDestinationFormat( entry.getKey() );
      }
    }
  }

  public String getDestinationValue()
  {
    return m_destinationNameField.getText().trim();
  }

  /**
   * Hook method for restoring widget values to the values that they held last
   * time this wizard was used to completion.
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

      final String format = settings.get( STORE_DESTINATION_FORMAT_ID );
      setDestinationFormat( format );
    }
  }

  private void setDestinationFormat( final Object format )
  {
    for( final Iterator fIt = m_formats.keySet().iterator(); fIt.hasNext();  )
    {
      final Object key = fIt.next();
      
      if( key.equals( format ) )
      {
        m_formatViewer.setSelection( new StructuredSelection( key ) );
        return;
      }
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

      settings.put( STORE_DESTINATION_NAMES_ID, (String[])history.toArray( new String[history
          .size()] ) );
    }
  }

  public Object getDestinationFormat()
  {
    final IStructuredSelection sel = (IStructuredSelection)m_formatViewer.getSelection();
    return sel.getFirstElement();
  }

}