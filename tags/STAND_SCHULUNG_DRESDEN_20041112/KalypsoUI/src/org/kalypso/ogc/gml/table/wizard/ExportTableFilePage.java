package org.kalypso.ogc.gml.table.wizard;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
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
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;

/**
 * @author belger
 */
public class ExportTableFilePage extends WizardPage
{
  // dialog store id constants
  private final static String STORE_DESTINATION_NAMES_ID = "ExportTableWizardPage.STORE_DESTINATION_NAMES_ID"; //$NON-NLS-1$

  protected static final int SIZING_TEXT_FIELD_WIDTH = 250;

  private static final int COMBO_HISTORY_LENGTH = 5;

  private Combo m_destinationNameField;

  public ExportTableFilePage( final String pageName, final String title,
      final ImageDescriptor titleImage )
  {
    super( pageName, title, titleImage );
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
    targetGroup.setLayoutData( new GridData( GridData.HORIZONTAL_ALIGN_FILL
        | GridData.VERTICAL_ALIGN_FILL ) );
    targetGroup.setFont( font );

    targetGroup.setText( "Export Ziel" );

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
    //    destinationNameField.addListener( SWT.Selection, this );
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
    //setButtonLayoutData( destinationBrowseButton );
    //destinationBrowseButton.setVisible( true );
  }

  protected void handleDestinationBrowseButtonPressed()
  {
    final FileDialog dialog = new FileDialog( getContainer().getShell(), SWT.SAVE );
    dialog.setFilterExtensions( new String[]
    {
        "*.csv", "*.*" } ); //$NON-NLS-1$ //$NON-NLS-2$
    dialog.setText( "Als CSV Exportieren" );
    final String currentSourceString = getDestinationValue();
    int lastSeparatorIndex = currentSourceString.lastIndexOf( File.separator );
    if( lastSeparatorIndex != -1 )
      dialog.setFilterPath( currentSourceString.substring( 0, lastSeparatorIndex ) );
    final String selectedFileName = dialog.open();

    if( selectedFileName != null )
    {
      setErrorMessage( null );
      setDestinationValue( selectedFileName );
    }
  }

  private void setDestinationValue( final String selectedFileName )
  {
    m_destinationNameField.setText( selectedFileName );
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

}