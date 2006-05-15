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
package org.kalypso.ui.model.wspm.importwizard;

import java.io.File;
import java.util.Iterator;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.BusyIndicator;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.dialogs.WizardResourceImportPage;
import org.eclipse.ui.internal.wizards.datatransfer.DataTransferMessages;
import org.eclipse.ui.internal.wizards.datatransfer.MinimizedFileSystemElement;
import org.eclipse.ui.model.WorkbenchContentProvider;
import org.eclipse.ui.wizards.datatransfer.FileSystemStructureProvider;
import org.eclipse.ui.wizards.datatransfer.IImportStructureProvider;

/**
 * @author thuel2
 */
public class WspWinImportPage extends WizardResourceImportPage implements Listener
{
  // widgets
  protected Combo sourceNameField;

  protected Button overwriteExistingResourcesCheckbox;

  protected Button sourceBrowseButton;

  protected Button selectAllButton;

  protected Button deselectAllButton;

  // A boolean to indicate if the user has typed anything
  private boolean entryChanged = false;

  // dialog store id constants
  private final static String STORE_SOURCE_NAMES_ID = "WizardFileSystemResourceImportPage1.STORE_SOURCE_NAMES_ID";//$NON-NLS-1$

  private final static String STORE_OVERWRITE_EXISTING_RESOURCES_ID = "WizardFileSystemResourceImportPage1.STORE_OVERWRITE_EXISTING_RESOURCES_ID";//$NON-NLS-1$

  private final static String STORE_CREATE_CONTAINER_STRUCTURE_ID = "WizardFileSystemResourceImportPage1.STORE_CREATE_CONTAINER_STRUCTURE_ID";//$NON-NLS-1$

  private static final String SELECT_TYPES_TITLE = DataTransferMessages.DataTransfer_selectTypes;

  private static final String SELECT_ALL_TITLE = DataTransferMessages.DataTransfer_selectAll;

  private static final String DESELECT_ALL_TITLE = DataTransferMessages.DataTransfer_deselectAll;

  private static final String SELECT_SOURCE_TITLE = DataTransferMessages.FileImport_selectSourceTitle;

  private static final String SELECT_SOURCE_MESSAGE = DataTransferMessages.FileImport_selectSource;

  protected static final String SOURCE_EMPTY_MESSAGE = DataTransferMessages.FileImport_sourceEmpty;

  private final IWorkbench m_workbench;

  /**
   * Creates an instance of this class
   */
  protected WspWinImportPage( final String name, final IWorkbench workbench, final IStructuredSelection selection )
  {
    super( name, selection );

    m_workbench = workbench;

    setTitle( "WspWin Daten" );
    setDescription( "WspWin Projekt w‰hlen" );
  }

  /**
   * Creates an instance of this class
   * 
   * @param aWorkbench
   *          IWorkbench
   * @param selection
   *          IStructuredSelection
   */
  public WspWinImportPage( final IWorkbench workbench, final IStructuredSelection selection )
  {
    this( "fileSystemImportPage1", workbench, selection );//$NON-NLS-1$
  }

  /**
   * Creates a new button with the given id.
   * <p>
   * The <code>Dialog</code> implementation of this framework method creates a standard push button, registers for
   * selection events including button presses and registers default buttons with its shell. The button id is stored as
   * the buttons client data. Note that the parent's layout is assumed to be a GridLayout and the number of columns in
   * this layout is incremented. Subclasses may override.
   * </p>
   * 
   * @param parent
   *          the parent composite
   * @param id
   *          the id of the button (see <code>IDialogConstants.*_ID</code> constants for standard dialog button ids)
   * @param label
   *          the label from the button
   * @param defaultButton
   *          <code>true</code> if the button is to be the default button, and <code>false</code> otherwise
   */
  protected Button createButton( Composite parent, int id, String label, boolean defaultButton )
  {
    // increment the number of columns in the button bar
    ((GridLayout) parent.getLayout()).numColumns++;

    Button button = new Button( parent, SWT.PUSH );
    button.setFont( parent.getFont() );

    GridData buttonData = new GridData( GridData.FILL_HORIZONTAL );
    button.setLayoutData( buttonData );

    button.setData( new Integer( id ) );
    button.setText( label );

    if( defaultButton )
    {
      Shell shell = parent.getShell();
      if( shell != null )
      {
        shell.setDefaultButton( button );
      }
      button.setFocus();
    }
    return button;
  }

  /**
   * Creates the buttons for selecting specific types or selecting all or none of the elements.
   * 
   * @param parent
   *          the parent control
   */
  protected final void createButtonsGroup( Composite parent )
  {
    // top level group
    Composite buttonComposite = new Composite( parent, SWT.NONE );
    GridLayout layout = new GridLayout();
    layout.numColumns = 2;
    layout.makeColumnsEqualWidth = true;
    buttonComposite.setLayout( layout );
    buttonComposite.setFont( parent.getFont() );
    GridData buttonData = new GridData( GridData.VERTICAL_ALIGN_FILL | GridData.HORIZONTAL_ALIGN_FILL );
    buttonData.horizontalSpan = 2;
    buttonComposite.setLayoutData( buttonData );

    selectAllButton = createButton( buttonComposite, IDialogConstants.SELECT_ALL_ID, SELECT_ALL_TITLE, false );

    final SelectionListener selectAlllistener = new SelectionAdapter()
    {
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        setAllSelections( true );
      }
    };
    selectAllButton.addSelectionListener( selectAlllistener );
    setButtonLayoutData( selectAllButton );

    deselectAllButton = createButton( buttonComposite, IDialogConstants.DESELECT_ALL_ID, DESELECT_ALL_TITLE, false );

    final SelectionListener deselectAllListener = new SelectionAdapter()
    {
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        setAllSelections( false );
      }
    };
    deselectAllButton.addSelectionListener( deselectAllListener );
    setButtonLayoutData( deselectAllButton );

  }

  /**
   * Create the import options specification widgets.
   */
  @Override
  protected void createOptionsGroupButtons( final Group optionsGroup )
  {
    // // overwrite... checkbox
    overwriteExistingResourcesCheckbox = new Button( optionsGroup, SWT.CHECK );
    overwriteExistingResourcesCheckbox.setFont( optionsGroup.getFont() );
    overwriteExistingResourcesCheckbox.setText( DataTransferMessages.FileImport_overwriteExisting );
    
    // not yet implemented, so disable it
    overwriteExistingResourcesCheckbox.setEnabled( false );
  }

  /**
   * Create the group for creating the root directory
   */
  protected void createRootDirectoryGroup( Composite parent )
  {
    Composite sourceContainerGroup = new Composite( parent, SWT.NONE );
    GridLayout layout = new GridLayout();
    layout.numColumns = 3;
    sourceContainerGroup.setLayout( layout );
    sourceContainerGroup.setFont( parent.getFont() );
    sourceContainerGroup.setLayoutData( new GridData( GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL ) );

    Label groupLabel = new Label( sourceContainerGroup, SWT.NONE );
    groupLabel.setText( getSourceLabel() );
    groupLabel.setFont( parent.getFont() );

    // source name entry field
    sourceNameField = new Combo( sourceContainerGroup, SWT.BORDER );
    GridData data = new GridData( GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL );
    data.widthHint = SIZING_TEXT_FIELD_WIDTH;
    sourceNameField.setLayoutData( data );
    sourceNameField.setFont( parent.getFont() );

    sourceNameField.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        updateFromSourceField();
      }
    } );

    sourceNameField.addKeyListener( new KeyListener()
    {
      /*
       * @see KeyListener.keyPressed
       */
      public void keyPressed( KeyEvent e )
      {
        // If there has been a key pressed then mark as dirty
        entryChanged = true;
      }

      /*
       * @see KeyListener.keyReleased
       */
      public void keyReleased( KeyEvent e )
      {
      }
    } );

    sourceNameField.addFocusListener( new FocusListener()
    {
      /*
       * @see FocusListener.focusGained(FocusEvent)
       */
      public void focusGained( FocusEvent e )
      {
        // Do nothing when getting focus
      }

      /*
       * @see FocusListener.focusLost(FocusEvent)
       */
      public void focusLost( FocusEvent e )
      {
        // Clear the flag to prevent constant update
        if( entryChanged )
        {
          entryChanged = false;
          updateFromSourceField();
        }

      }
    } );

    // source browse button
    sourceBrowseButton = new Button( sourceContainerGroup, SWT.PUSH );
    sourceBrowseButton.setText( DataTransferMessages.DataTransfer_browse );
    sourceBrowseButton.addListener( SWT.Selection, this );
    sourceBrowseButton.setLayoutData( new GridData( GridData.HORIZONTAL_ALIGN_FILL ) );
    sourceBrowseButton.setFont( parent.getFont() );
    setButtonLayoutData( sourceBrowseButton );
  }

  /**
   * Update the receiver from the source name field.
   */

  private void updateFromSourceField( )
  {

    setSourceName( sourceNameField.getText() );
    // Update enablements when this is selected
    updateWidgetEnablements();
  }

  /**
   * Creates and returns a <code>FileSystemElement</code> if the specified file system object merits one. The criteria
   * for this are: Also create the children.
   */
  protected MinimizedFileSystemElement createRootElement( final Object fileSystemObject, final IImportStructureProvider provider )
  {
    final boolean isContainer = provider.isFolder( fileSystemObject );
    final String elementLabel = provider.getLabel( fileSystemObject );

    // Use an empty label so that display of the element's full name
    // doesn't include a confusing label
    final MinimizedFileSystemElement dummyParent = new MinimizedFileSystemElement( "", null, true );//$NON-NLS-1$
    dummyParent.setPopulated();

    final MinimizedFileSystemElement result = new MinimizedFileSystemElement( "Auswahl zur Zeit nicht implementiert", dummyParent, isContainer );
    // MinimizedFileSystemElement result = new MinimizedFileSystemElement( elementLabel, dummyParent, isContainer );
    result.setFileSystemObject( fileSystemObject );
    //
    // // Get the files for the element so as to build the first level
    // result.getFiles( provider );

    return dummyParent;
  }

  /**
   * Create the import source specification widgets
   */
  @Override
  protected void createSourceGroup( final Composite parent )
  {
    createRootDirectoryGroup( parent );
    createFileSelectionGroup( parent );
    createButtonsGroup( parent );

    // HACK: hide file selection and buttons groups
    final Control[] children = parent.getChildren();
    ((GridData) children[1].getLayoutData()).exclude = true;
    ((GridData) children[2].getLayoutData()).exclude = true;
  }

  /**
   * Enable or disable the button group.
   */
  protected void enableButtonGroup( boolean enable )
  {
    selectAllButton.setEnabled( enable );
    deselectAllButton.setEnabled( enable );
  }

  /**
   * Answer a boolean indicating whether the specified source currently exists and is valid
   */
  protected boolean ensureSourceIsValid( )
  {
    if( new File( getSourceDirectoryName() ).isDirectory() )
      return true;

    displayErrorDialog( DataTransferMessages.FileImport_invalidSource );
    sourceNameField.setFocus();
    return false;
  }

  /**
   * Returns a content provider for <code>FileSystemElement</code>s that returns only files as children.
   */
  @Override
  protected ITreeContentProvider getFileProvider( )
  {
    return new WorkbenchContentProvider()
    {
      public Object[] getChildren( Object o )
      {
        if( o instanceof MinimizedFileSystemElement )
        {
          MinimizedFileSystemElement element = (MinimizedFileSystemElement) o;
          return element.getFiles( FileSystemStructureProvider.INSTANCE ).getChildren( element );
        }
        return new Object[0];
      }
    };
  }

  /**
   * Answer the root FileSystemElement that represents the contents of the currently-specified source. If this
   * FileSystemElement is not currently defined then create and return it.
   */
  protected MinimizedFileSystemElement getFileSystemTree( )
  {
    final File sourceDirectory = getSourceDirectory();
    if( sourceDirectory == null )
      return null;

    return selectFiles( sourceDirectory, FileSystemStructureProvider.INSTANCE );
  }

  /**
   * Returns a content provider for <code>FileSystemElement</code>s that returns only folders as children.
   */
  @Override
  protected ITreeContentProvider getFolderProvider( )
  {
    return new WorkbenchContentProvider()
    {
      // public Object[] getChildren( Object o )
      // {
      // if( o instanceof MinimizedFileSystemElement )
      // {
      // MinimizedFileSystemElement element = (MinimizedFileSystemElement) o;
      // return element.getFolders( FileSystemStructureProvider.INSTANCE ).getChildren( element );
      // }
      // return new Object[0];
      // }
      //
      // public boolean hasChildren( Object o )
      // {
      // if( o instanceof MinimizedFileSystemElement )
      // {
      // MinimizedFileSystemElement element = (MinimizedFileSystemElement) o;
      // if( element.isPopulated() )
      // return getChildren( element ).length > 0;
      //
      // // If we have not populated then wait until asked
      // return true;
      // }
      // return false;
      // }
    };
  }

  /**
   * Returns a File object representing the currently-named source directory iff it exists as a valid directory, or
   * <code>null</code> otherwise.
   */
  public File getSourceDirectory( )
  {
    return getSourceDirectory( this.sourceNameField.getText() );
  }

  /**
   * Returns a File object representing the currently-named source directory iff it exists as a valid directory, or
   * <code>null</code> otherwise.
   * 
   * @param path
   *          a String not yet formatted for java.io.File compatability
   */
  private File getSourceDirectory( String path )
  {
    File sourceDirectory = new File( getSourceDirectoryName( path ) );
    if( !sourceDirectory.exists() || !sourceDirectory.isDirectory() )
    {
      return null;
    }

    return sourceDirectory;
  }

  /**
   * Answer the directory name specified as being the import source. Note that if it ends with a separator then the
   * separator is first removed so that java treats it as a proper directory
   */
  private String getSourceDirectoryName( )
  {
    return getSourceDirectoryName( this.sourceNameField.getText() );
  }

  /**
   * Answer the directory name specified as being the import source. Note that if it ends with a separator then the
   * separator is first removed so that java treats it as a proper directory
   */
  private String getSourceDirectoryName( String sourceName )
  {
    IPath result = new Path( sourceName.trim() );

    if( result.getDevice() != null && result.segmentCount() == 0 ) // something like "c:"
      result = result.addTrailingSeparator();
    else
      result = result.removeTrailingSeparator();

    return result.toOSString();
  }

  /**
   * Answer the string to display as the label for the source specification field
   */
  protected String getSourceLabel( )
  {
    return "WspWin-Projekt Verzeichnis";
  }

  /**
   * Handle all events and enablements for widgets in this dialog
   * 
   * @param event
   *          Event
   */
  @Override
  public void handleEvent( Event event )
  {
    if( event.widget == sourceBrowseButton )
      handleSourceBrowseButtonPressed();

    super.handleEvent( event );
  }

  /**
   * Open an appropriate source browser so that the user can specify a source to import from
   */
  protected void handleSourceBrowseButtonPressed( )
  {

    String currentSource = this.sourceNameField.getText();
    DirectoryDialog dialog = new DirectoryDialog( sourceNameField.getShell(), SWT.SAVE );
    dialog.setText( SELECT_SOURCE_TITLE );
    dialog.setMessage( SELECT_SOURCE_MESSAGE );
    dialog.setFilterPath( getSourceDirectoryName( currentSource ) );

    String selectedDirectory = dialog.open();
    if( selectedDirectory != null )
    {
      // Just quit if the directory is not valid
      if( (getSourceDirectory( selectedDirectory ) == null) || selectedDirectory.equals( currentSource ) )
        return;
      // If it is valid then proceed to populate
      setErrorMessage( null );
      setSourceName( selectedDirectory );
      selectionGroup.setFocus();
    }
  }

  /**
   * Open a registered type selection dialog and note the selections in the receivers types-to-export field., Added here
   * so that inner classes can have access
   */
  @Override
  protected void handleTypesEditButtonPressed( )
  {

    super.handleTypesEditButtonPressed();
  }

  /**
   * Returns whether the extension provided is an extension that has been specified for export by the user.
   * 
   * @param extension
   *          the resource name
   * @return <code>true</code> if the resource name is suitable for export based upon its extension
   */
  protected boolean isExportableExtension( String extension )
  {
    if( selectedTypes == null ) // ie.- all extensions are acceptable
      return true;

    Iterator itr = selectedTypes.iterator();
    while( itr.hasNext() )
    {
      if( extension.equalsIgnoreCase( (String) itr.next() ) )
        return true;
    }

    return false;
  }

  /**
   * Repopulate the view based on the currently entered directory.
   */
  protected void resetSelection( )
  {
    MinimizedFileSystemElement currentRoot = getFileSystemTree();
    this.selectionGroup.setRoot( currentRoot );

  }

  /**
   * Use the dialog store to restore widget values to the values that they held last time this wizard was used to
   * completion
   */
  @Override
  protected void restoreWidgetValues( )
  {
    final IDialogSettings settings = getDialogSettings();
    if( settings != null )
    {
      String[] sourceNames = settings.getArray( STORE_SOURCE_NAMES_ID );
      if( sourceNames == null )
        return; // ie.- no values stored, so stop

      // set filenames history
      for( int i = 0; i < sourceNames.length; i++ )
        sourceNameField.add( sourceNames[i] );

      // radio buttons and checkboxes
      overwriteExistingResourcesCheckbox.setSelection( settings.getBoolean( STORE_OVERWRITE_EXISTING_RESOURCES_ID ) );
    }
  }

  /**
   * Since Finish was pressed, write widget values to the dialog store so that they will persist into the next
   * invocation of this wizard page
   */
  @Override
  protected void saveWidgetValues( )
  {
    final IDialogSettings settings = getDialogSettings();
    if( settings != null )
    {
      // update source names history
      String[] sourceNames = settings.getArray( STORE_SOURCE_NAMES_ID );
      if( sourceNames == null )
        sourceNames = new String[0];

      sourceNames = addToHistory( sourceNames, getSourceDirectoryName() );
      settings.put( STORE_SOURCE_NAMES_ID, sourceNames );

      // radio buttons and checkboxes
      settings.put( STORE_OVERWRITE_EXISTING_RESOURCES_ID, overwriteExistingResourcesCheckbox.getSelection() );
    }
  }

  /**
   * Invokes a file selection operation using the specified file system and structure provider. If the user specifies
   * files to be imported then this selection is cached for later retrieval and is returned.
   */
  protected MinimizedFileSystemElement selectFiles( final Object rootFileSystemObject, final IImportStructureProvider structureProvider )
  {
    final MinimizedFileSystemElement[] results = new MinimizedFileSystemElement[1];

    BusyIndicator.showWhile( getShell().getDisplay(), new Runnable()
    {
      public void run( )
      {
        // Create the root element from the supplied file system object
        results[0] = createRootElement( rootFileSystemObject, structureProvider );
      }
    } );

    return results[0];
  }

  /**
   * Sets the source name of the import to be the supplied path. Adds the name of the path to the list of items in the
   * source combo and selects it.
   * 
   * @param path
   *          the path to be added
   */
  protected void setSourceName( String path )
  {
    if( path.length() > 0 )
    {

      String[] currentItems = this.sourceNameField.getItems();
      int selectionIndex = -1;
      for( int i = 0; i < currentItems.length; i++ )
      {
        if( currentItems[i].equals( path ) )
          selectionIndex = i;
      }
      if( selectionIndex < 0 )
      {
        int oldLength = currentItems.length;
        String[] newItems = new String[oldLength + 1];
        System.arraycopy( currentItems, 0, newItems, 0, oldLength );
        newItems[oldLength] = path;
        this.sourceNameField.setItems( newItems );
        selectionIndex = oldLength;
      }
      this.sourceNameField.select( selectionIndex );

      resetSelection();
    }
  }

  /**
   * Method declared on IDialogPage. Set the selection up when it becomes visible.
   */
  @Override
  public void setVisible( boolean visible )
  {
    super.setVisible( visible );
    resetSelection();
    if( visible )
      this.sourceNameField.setFocus();
  }

  /**
   * Answer a boolean indicating whether self's source specification widgets currently all contain valid values.
   */
  @Override
  protected boolean validateSourceGroup( )
  {
    final File sourceDirectory = getSourceDirectory();
    if( sourceDirectory == null )
    {
      setMessage( SOURCE_EMPTY_MESSAGE );
      enableButtonGroup( false );
      return false;
    }

    if( sourceConflictsWithDestination( new Path( sourceDirectory.getPath() ) ) )
    {
      setErrorMessage( getSourceConflictMessage() ); //$NON-NLS-1$
      enableButtonGroup( false );
      return false;
    }

    final File profDir = new File( sourceDirectory, "prof" );
    final File wspcfgfile = new File( profDir, "wsp.cfg" );
    // final File dathDir = new File( sourceDirectory, "dath" );
    if( !profDir.exists() || !wspcfgfile.exists() )
    {
      setErrorMessage( "Kein WspWin Projektverzeichnis ausgew‰hlt (Verzeichnis enth‰lt keine Datei prof/wsp.cfg)" ); //$NON-NLS-1$
      enableButtonGroup( false );
      return false;
    }

    enableButtonGroup( true );
    return true;
  }

  /**
   * Returns whether the source location conflicts with the destination resource. This will occur if the source is
   * already under the destination.
   * 
   * @param sourcePath
   *          the path to check
   * @return <code>true</code> if there is a conflict, <code>false</code> if not
   */
  @Override
  protected boolean sourceConflictsWithDestination( IPath sourcePath )
  {
    IContainer container = getSpecifiedContainer();
    if( container == null )
      return false;

    IPath destinationLocation = getSpecifiedContainer().getLocation();
    if( destinationLocation != null )
    {
      return destinationLocation.isPrefixOf( sourcePath );
    }
    // null destination location is handled in
    // WizardResourceImportPage
    return false;
  }

  /**
   * Return the path for the resource field.
   * 
   * @return IPath
   */
  public IContainer getTargetContainer( )
  {
    final IPath path = super.getResourcePath();

    final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();

    if( path.segmentCount() == 1 )
      return root.getProject( path.segment( 0 ) );

    return root.getFolder( path );
  }
}
