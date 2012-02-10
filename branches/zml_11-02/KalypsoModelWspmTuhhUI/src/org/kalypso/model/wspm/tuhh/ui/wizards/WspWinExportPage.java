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
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Font;
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
import org.eclipse.swt.widgets.Widget;
import org.eclipse.ui.dialogs.WizardDataTransferPage;
import org.eclipse.ui.internal.ide.DialogUtil;
import org.eclipse.ui.internal.ide.dialogs.ResourceTreeAndListGroup;
import org.eclipse.ui.model.WorkbenchContentProvider;
import org.eclipse.ui.model.WorkbenchLabelProvider;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;

/**
 * @author thuel2
 */
@SuppressWarnings("restriction")
public class WspWinExportPage extends WizardDataTransferPage implements Listener
{
  private final IStructuredSelection m_selection;

  // widgets
  ResourceTreeAndListGroup resourceGroup;

  private Combo destinationNameField;

  private Button destinationBrowseButton;

  protected Button overwriteExistingFilesCheckbox;

  protected Button selectAllButton;

  protected Button deselectAllButton;

  // messages and labels
  private static final String SELECT_ALL_TITLE = Messages.getString( "org.kalypso.model.wspm.tuhh.ui.wizards.WspWinExportPage.0" ); //$NON-NLS-1$

  private static final String DESELECT_ALL_TITLE = Messages.getString( "org.kalypso.model.wspm.tuhh.ui.wizards.WspWinExportPage.1" ); //$NON-NLS-1$

  private static final String SELECT_DESTINATION_MESSAGE = Messages.getString( "org.kalypso.model.wspm.tuhh.ui.wizards.WspWinExportPage.2" ); //$NON-NLS-1$

  private static final String SELECT_DESTINATION_TITLE = Messages.getString( "org.kalypso.model.wspm.tuhh.ui.wizards.WspWinExportPage.3" ); //$NON-NLS-1$

  private static final String DESTINATION_BROWSE = Messages.getString( "org.kalypso.model.wspm.tuhh.ui.wizards.WspWinExportPage.4" ); //$NON-NLS-1$

  private static final String DESTINATION_LABEL = Messages.getString( "org.kalypso.model.wspm.tuhh.ui.wizards.WspWinExportPage.5" ); //$NON-NLS-1$

  private static final String OVERWRITE_EXISTING_CHECK_LABEL = Messages.getString( "org.kalypso.model.wspm.tuhh.ui.wizards.WspWinExportPage.6" ); //$NON-NLS-1$

  // dialog store id constants
  private final static String STORE_DESTINATION_NAMES_ID = "WspWinExportPage.STORE_DESTINATION_NAMES_ID";//$NON-NLS-1$

  private final static String STORE_OVERWRITE_EXISTING_RESOURCES_ID = "WspWinExportPage.STORE_OVERWRITE_EXISTING_RESOURCES_ID";//$NON-NLS-1$

  /**
   * Creates an instance of this class
   * 
   * @param aWorkbench
   *          IWorkbench
   * @param selection
   *          IStructuredSelection
   */
  protected WspWinExportPage( final String pageName, final IStructuredSelection selection )
  {
    super( pageName );

    m_selection = selection;

    setTitle( SELECT_DESTINATION_TITLE );
    setDescription( SELECT_DESTINATION_MESSAGE );
  }

  /**
   * Creates an instance of this class
   */
  public WspWinExportPage( final IStructuredSelection selection )
  {
    this( "WspWinExportPage", selection ); //$NON-NLS-1$

  }

  /**
   * @see org.eclipse.ui.dialogs.WizardDataTransferPage#allowNewContainerName()
   */
  @Override
  protected boolean allowNewContainerName( )
  {
    return false;
  }

  /**
   * @see org.eclipse.swt.widgets.Listener#handleEvent(org.eclipse.swt.widgets.Event)
   */
  @Override
  public void handleEvent( final Event event )
  {
    final Widget source = event.widget;

    if( source == destinationBrowseButton )
      handleDestinationBrowseButtonPressed();

    updatePageCompletion();
  }

  /**
   * Open an appropriate source browser so that the user can specify a source to import from
   */
  protected void handleDestinationBrowseButtonPressed( )
  {

    final String currentSource = this.destinationNameField.getText();
    final DirectoryDialog dialog = new DirectoryDialog( destinationNameField.getShell(), SWT.SAVE );
    dialog.setText( SELECT_DESTINATION_TITLE );
    dialog.setMessage( SELECT_DESTINATION_MESSAGE );
    dialog.setFilterPath( getDestinationDirectoryName( currentSource ) );

    final String selectedDirectory = dialog.open();
    if( selectedDirectory != null )
    {
      // Just quit if the directory is not valid
      if( (getDestinationDirectory( selectedDirectory ) == null) || selectedDirectory.equals( currentSource ) )
        return;
      // If it is valid then proceed to populate
      setErrorMessage( null );
      setDestinationName( selectedDirectory );
      resourceGroup.setFocus();
    }
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createControl( final Composite parent )
  {

    initializeDialogUnits( parent );

    final Composite composite = new Composite( parent, SWT.NULL );
    composite.setLayout( new GridLayout() );
    composite.setLayoutData( new GridData( GridData.VERTICAL_ALIGN_FILL | GridData.HORIZONTAL_ALIGN_FILL ) );
    composite.setFont( parent.getFont() );

    createResourcesGroup( composite );
    createButtonsGroup( composite );
    // TODO: implement B‰umchen und remove HACK: hide file selection and buttons groups
    final Control[] children = composite.getChildren();
    ((GridData) children[0].getLayoutData()).exclude = true;
    ((GridData) children[1].getLayoutData()).exclude = true;
    createDestinationGroup( composite );

    createOptionsGroup( composite );

    restoreResourceSpecificationWidgetValues(); // ie.- local
    restoreWidgetValues();
    if( m_selection != null )
      setupBasedOnInitialSelections();

    updateWidgetEnablements();
    setPageComplete( determinePageCompletion() );

    setControl( composite );
  }

  /**
   * // * Creates the buttons for selecting specific types or selecting all or none of the // * elements. // * // *
   * 
   * @param parent
   *          the parent control //
   */
  protected final void createButtonsGroup( final Composite parent )
  {

    final Font font = parent.getFont();

    // top level group
    final Composite buttonComposite = new Composite( parent, SWT.NONE );
    buttonComposite.setFont( parent.getFont() );

    final GridLayout layout = new GridLayout();
    layout.numColumns = 3;
    layout.makeColumnsEqualWidth = true;
    buttonComposite.setLayout( layout );
    buttonComposite.setLayoutData( new GridData( GridData.VERTICAL_ALIGN_FILL | GridData.HORIZONTAL_ALIGN_FILL ) );

    selectAllButton = createButton( buttonComposite, IDialogConstants.SELECT_ALL_ID, SELECT_ALL_TITLE, false );

    SelectionListener listener = new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        resourceGroup.setAllSelections( true );
      }
    };
    selectAllButton.addSelectionListener( listener );
    selectAllButton.setFont( font );
    setButtonLayoutData( selectAllButton );

    deselectAllButton = createButton( buttonComposite, IDialogConstants.DESELECT_ALL_ID, DESELECT_ALL_TITLE, false );

    listener = new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        resourceGroup.setAllSelections( false );
      }
    };
    deselectAllButton.addSelectionListener( listener );
    deselectAllButton.setFont( font );
    setButtonLayoutData( deselectAllButton );

  }

  /**
   * Enable or disable the button group.
   */
  protected void enableButtonGroup( final boolean enable )
  {
    selectAllButton.setEnabled( enable );
    deselectAllButton.setEnabled( enable );
  }

  /**
   * Create the export destination specification widgets
   * 
   * @param parent
   *          org.eclipse.swt.widgets.Composite
   */
  protected void createDestinationGroup( final Composite parent )
  {
    final Font font = parent.getFont();
    // destination specification group
    final Composite destinationSelectionGroup = new Composite( parent, SWT.NONE );
    final GridLayout layout = new GridLayout();
    layout.numColumns = 3;
    destinationSelectionGroup.setLayout( layout );
    destinationSelectionGroup.setLayoutData( new GridData( GridData.HORIZONTAL_ALIGN_FILL | GridData.VERTICAL_ALIGN_FILL ) );
    destinationSelectionGroup.setFont( font );

    final Label destinationLabel = new Label( destinationSelectionGroup, SWT.NONE );
    destinationLabel.setText( DESTINATION_LABEL );
    destinationLabel.setFont( font );

    // destination name entry field
    destinationNameField = new Combo( destinationSelectionGroup, SWT.SINGLE | SWT.BORDER );
    destinationNameField.addListener( SWT.Modify, this );
    destinationNameField.addListener( SWT.Selection, this );
    final GridData data = new GridData( GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL );
    data.widthHint = SIZING_TEXT_FIELD_WIDTH;
    destinationNameField.setLayoutData( data );
    destinationNameField.setFont( font );

    // destination browse button
    destinationBrowseButton = new Button( destinationSelectionGroup, SWT.PUSH );
    destinationBrowseButton.setText( DESTINATION_BROWSE );
    destinationBrowseButton.addListener( SWT.Selection, this );
    destinationBrowseButton.setFont( font );
    setButtonLayoutData( destinationBrowseButton );

    new Label( parent, SWT.NONE ); // vertical spacer

  }

  /**
   * Creates the checkbox tree and list for selecting resources.
   * 
   * @param parent
   *          the parent control
   */
  protected final void createResourcesGroup( final Composite parent )
  {

    // create the input element, which has the root resource
    // as its only child
    final List<IProject> input = new ArrayList<IProject>();
    final IProject[] projects = ResourcesPlugin.getWorkspace().getRoot().getProjects();
    for( final IProject project : projects )
    {
      if( project.isOpen() )
        input.add( project );
    }

    this.resourceGroup = new ResourceTreeAndListGroup( parent, input, getResourceProvider( IResource.FOLDER | IResource.PROJECT ), WorkbenchLabelProvider.getDecoratingWorkbenchLabelProvider(), getResourceProvider( IResource.FILE ), WorkbenchLabelProvider.getDecoratingWorkbenchLabelProvider(), SWT.NONE, DialogUtil.inRegularFontMode( parent ) );

  }

  /**
   * Create the buttons in the options group.
   */

  @Override
  protected void createOptionsGroupButtons( final Group optionsGroup )
  {

    final Font font = optionsGroup.getFont();
    createOverwriteExisting( optionsGroup, font );

  }

  /**
   * Create the button for checking if we should ask if we are going to overwrite existing files.
   * 
   * @param optionsGroup
   * @param font
   */
  protected void createOverwriteExisting( final Group optionsGroup, final Font font )
  {
    // overwrite... checkbox
    overwriteExistingFilesCheckbox = new Button( optionsGroup, SWT.CHECK | SWT.LEFT );
    overwriteExistingFilesCheckbox.setText( OVERWRITE_EXISTING_CHECK_LABEL );
    overwriteExistingFilesCheckbox.setFont( font );

    // TODO: not yet implemented: disable
    overwriteExistingFilesCheckbox.setEnabled( false );

  }

  // /*
  // * @see WizardDataTransferPage.getErrorDialogTitle()
  // */
  // protected String getErrorDialogTitle( )
  // {
  // return IDEWorkbenchMessages.WizardExportPage_errorDialogTitle;
  // }

  //
  // /**
  // * Returns a new subcollection containing only those resources which are not
  // * local.
  // *
  // * @param originalList the original list of resources (element type:
  // * <code>IResource</code>)
  // * @return the new list of non-local resources (element type:
  // * <code>IResource</code>)
  // */
  // protected List extractNonLocalResources(List originalList) {
  // Vector result = new Vector(originalList.size());
  // Iterator resourcesEnum = originalList.iterator();
  //
  // while (resourcesEnum.hasNext()) {
  // IResource currentResource = (IResource) resourcesEnum.next();
  // if (!currentResource.isLocal(IResource.DEPTH_ZERO))
  // result.addElement(currentResource);
  // }
  //
  // return result;
  // }
  //
  /**
   * Returns a content provider for <code>IResource</code>s that returns only children of the given resource type.
   */
  private ITreeContentProvider getResourceProvider( final int resourceType )
  {
    return new WorkbenchContentProvider()
    {
      @Override
      public Object[] getChildren( final Object o )
      {
        if( o instanceof IContainer )
        {
          IResource[] members = null;
          try
          {
            members = ((IContainer) o).members();
          }
          catch( final CoreException e )
          {
            // just return an empty set of children
            return new Object[0];
          }

          // filter out the desired resource types
          final ArrayList<IResource> results = new ArrayList<IResource>();
          for( final IResource member : members )
          {
            // And the test bits with the resource types to see if they are what we want
            if( (member.getType() & resourceType) > 0 )
            {
              results.add( member );
            }
          }
          return results.toArray();
        }
        else
        {
          // input element case
          if( o instanceof ArrayList )
          {
            return ((ArrayList< ? >) o).toArray();
          }
          else
          {
            return new Object[0];
          }
        }
      }
    };
  }

  /**
   * Returns this page's collection of currently-specified resources to be exported. This is the primary resource
   * selection facility accessor for subclasses.
   * 
   * @return a collection of resources currently selected for export (element type: <code>IResource</code>)
   */
  protected List<IResource> getSelectedResources( )
  {
    final Iterator<IResource> resourcesToExportIterator = getSelectedResourcesIterator();
    final List<IResource> resourcesToExport = new ArrayList<IResource>();
    while( resourcesToExportIterator.hasNext() )
      resourcesToExport.add( resourcesToExportIterator.next() );
    return resourcesToExport;
  }

  /**
   * Returns this page's collection of currently-specified resources to be exported. This is the primary resource
   * selection facility accessor for subclasses.
   * 
   * @return an iterator over the collection of resources currently selected for export (element type:
   *         <code>IResource</code>). This will include white checked folders and individually checked files.
   */
  @SuppressWarnings("unchecked")
  protected Iterator<IResource> getSelectedResourcesIterator( )
  {
    return this.resourceGroup.getAllCheckedListItems().iterator();
  }

  /**
   * Returns this page's collection of currently-specified resources to be exported. This returns both folders and files
   * - for just the files use getSelectedResources.
   * 
   * @return a collection of resources currently selected for export (element type: <code>IResource</code>)
   */
  protected List< ? > getWhiteCheckedResources( )
  {
    return this.resourceGroup.getAllWhiteCheckedItems();
  }

  //
  // /**
  // * Returns whether the extension of the given resource name is an extension that
  // * has been specified for export by the user.
  // *
  // * @param resourceName the resource name
  // * @return <code>true</code> if the resource name is suitable for export based
  // * upon its extension
  // */
  // protected boolean hasExportableExtension(String resourceName) {
  // if (selectedTypes == null) // ie.- all extensions are acceptable
  // return true;
  //
  // int separatorIndex = resourceName.lastIndexOf("."); //$NON-NLS-1$
  // if (separatorIndex == -1)
  // return false;
  //
  // String extension = resourceName.substring(separatorIndex + 1);
  //
  // Iterator it = selectedTypes.iterator();
  // while (it.hasNext()) {
  // if (extension.equalsIgnoreCase((String) it.next()))
  // return true;
  // }
  //
  // return false;
  // }
  //
  /**
   * Persists additional setting that are to be restored in the next instance of this page.
   * <p>
   * The <code>WizardImportPage</code> implementation of this method does nothing. Subclasses may extend to persist
   * additional settings.
   * </p>
   */
  protected void internalSaveWidgetValues( )
  {
  }

  /**
   * Restores resource specification control settings that were persisted in the previous instance of this page.
   * Subclasses wishing to restore persisted values for their controls may extend.
   */
  protected void restoreResourceSpecificationWidgetValues( )
  {
  }

  /**
   * Persists resource specification control setting that are to be restored in the next instance of this page.
   * Subclasses wishing to persist additional setting for their controls should extend hook method
   * <code>internalSaveWidgetValues</code>.
   */
  @Override
  protected void saveWidgetValues( )
  {

    // allow subclasses to save values
    internalSaveWidgetValues();

    final IDialogSettings settings = getDialogSettings();
    if( settings != null )
    {
      // update source names history
      String[] destinationNames = settings.getArray( STORE_DESTINATION_NAMES_ID );
      if( destinationNames == null )
        destinationNames = new String[0];

      destinationNames = addToHistory( destinationNames, getDestinationDirectoryName() );
      settings.put( STORE_DESTINATION_NAMES_ID, destinationNames );

      // radio buttons and checkboxes
      settings.put( STORE_OVERWRITE_EXISTING_RESOURCES_ID, overwriteExistingFilesCheckbox.getSelection() );
    }

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
      final String[] destinationNames = settings.getArray( STORE_DESTINATION_NAMES_ID );
      if( destinationNames == null )
        return; // ie.- no values stored, so stop

      // set filenames history
      for( final String destinationName : destinationNames )
        destinationNameField.add( destinationName );

      // radio buttons and checkboxes
      overwriteExistingFilesCheckbox.setSelection( settings.getBoolean( STORE_OVERWRITE_EXISTING_RESOURCES_ID ) );
    }
  }

  /**
   * Set the initial selections in the resource group.
   */
  protected void setupBasedOnInitialSelections( )
  {
    final Iterator< ? > it = m_selection.iterator();
    while( it.hasNext() )
    {
      final Object res = it.next();
      if( res instanceof IResource )
      {
        final IResource currentResource = (IResource) res;
        if( currentResource.getType() == IResource.FILE )
          this.resourceGroup.initialCheckListItem( currentResource );
        else
          this.resourceGroup.initialCheckTreeItem( res );
      }
      else
        this.resourceGroup.initialCheckTreeItem( res );
    }
  }

  /**
   * Returns a File object representing the currently-named destination directory iff it exists as a valid directory, or
   * <code>null</code> otherwise.
   */
  public File getDestinationDirectory( )
  {
    return getDestinationDirectory( this.destinationNameField.getText() );
  }

  /**
   * Returns a File object representing the currently-named destination directory iff it exists as a valid directory, or
   * <code>null</code> otherwise.
   * 
   * @param path
   *          a String not yet formatted for java.io.File compatability
   */
  private File getDestinationDirectory( final String path )
  {
    final File destinationDirectory = new File( getDestinationDirectoryName( path ) );
    if( !destinationDirectory.exists() || !destinationDirectory.isDirectory() )
    {
      return null;
    }

    return destinationDirectory;
  }

  /**
   * Answer the directory name specified as being the export destination. Note that if it ends with a separator then the
   * separator is first removed so that java treats it as a proper directory
   */
  private String getDestinationDirectoryName( )
  {
    return getDestinationDirectoryName( this.destinationNameField.getText() );
  }

  /**
   * Answer the directory name specified as being the export destination. Note that if it ends with a separator then the
   * separator is first removed so that java treats it as a proper directory
   */
  private String getDestinationDirectoryName( final String destinationName )
  {
    IPath result = new Path( destinationName.trim() );

    if( result.getDevice() != null && result.segmentCount() == 0 ) // something like "c:"
      result = result.addTrailingSeparator();
    else
      result = result.removeTrailingSeparator();

    return result.toOSString();
  }

  /**
   * Sets the destination name of the export to be the supplied path. Adds the name of the path to the list of items in
   * the destination combo and selects it.
   * 
   * @param path
   *          the path to be added
   */
  protected void setDestinationName( final String path )
  {
    if( path.length() > 0 )
    {

      final String[] currentItems = this.destinationNameField.getItems();
      int selectionIndex = -1;
      for( int i = 0; i < currentItems.length; i++ )
      {
        if( currentItems[i].equals( path ) )
          selectionIndex = i;
      }
      if( selectionIndex < 0 )
      {
        final int oldLength = currentItems.length;
        final String[] newItems = new String[oldLength + 1];
        System.arraycopy( currentItems, 0, newItems, 0, oldLength );
        newItems[oldLength] = path;
        this.destinationNameField.setItems( newItems );
        selectionIndex = oldLength;
      }
      this.destinationNameField.select( selectionIndex );

      // resetSelection();
    }
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
  protected Button createButton( final Composite parent, final int id, final String label, final boolean defaultButton )
  {
    // increment the number of columns in the button bar
    ((GridLayout) parent.getLayout()).numColumns++;

    final Button button = new Button( parent, SWT.PUSH );

    final GridData buttonData = new GridData( GridData.FILL_HORIZONTAL );
    button.setLayoutData( buttonData );

    button.setData( Integer.valueOf( id ) );
    button.setText( label );
    button.setFont( parent.getFont() );

    if( defaultButton )
    {
      final Shell shell = parent.getShell();
      if( shell != null )
      {
        shell.setDefaultButton( button );
      }
      button.setFocus();
    }
    button.setFont( parent.getFont() );
    setButtonLayoutData( button );
    return button;
  }

}
