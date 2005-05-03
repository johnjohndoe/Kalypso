package org.kalypso.ui.createGisMapView;

import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.widgets.*;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.SWT;
import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.Path;
import org.eclipse.swt.events.*;
import org.eclipse.ui.dialogs.ContainerSelectionDialog;
import org.eclipse.jface.viewers.*;
import org.kalypso.ui.ImageProvider;

public class CreateGisMapViewWizardPage extends WizardPage
{
  private Text containerText;

  private Text fileText;

  private ISelection m_selection;

  public CreateGisMapViewWizardPage( ISelection selection )
  {
    super( "wizardPage" );
    setTitle( "GisMapView Editor File" );
    setDescription( "This wizard creates a new file with *.gmt extension that can be opened by a GisMapView editor." );
    this.setImageDescriptor(ImageProvider.IMAGE_ICON_GMT);
    m_selection = selection;
  }

  public void createControl( Composite parent )
  {
    Composite container = new Composite( parent, SWT.NULL );
    GridLayout layout = new GridLayout();
    container.setLayout( layout );
    layout.numColumns = 3;
    layout.verticalSpacing = 9;
    Label label = new Label( container, SWT.NULL );
    label.setText( "&Container:" );

    containerText = new Text( container, SWT.BORDER | SWT.SINGLE );
    GridData gd = new GridData( GridData.FILL_HORIZONTAL );
    containerText.setLayoutData( gd );
    containerText.addModifyListener( new ModifyListener()
    {
      public void modifyText( ModifyEvent e )
      {
        dialogChanged();
      }
    } );

    Button button = new Button( container, SWT.PUSH );
    button.setText( "Browse..." );
    button.addSelectionListener( new SelectionAdapter()
    {
      public void widgetSelected( SelectionEvent e )
      {
        handleBrowse();
      }
    } );
    label = new Label( container, SWT.NULL );
    label.setText( "&File name:" );

    fileText = new Text( container, SWT.BORDER | SWT.SINGLE );
    gd = new GridData( GridData.FILL_HORIZONTAL );
    fileText.setLayoutData( gd );
    fileText.addModifyListener( new ModifyListener()
    {
      public void modifyText( ModifyEvent e )
      {
        dialogChanged();
      }
    } );
    initialize();
    dialogChanged();
    setControl( container );
  }

  /**
   * Tests if the current workbench selection is a suitable container to use.
   */

  private void initialize()
  {
    if( m_selection != null && m_selection.isEmpty() == false
        && m_selection instanceof IStructuredSelection )
    {
      IStructuredSelection ssel = (IStructuredSelection)m_selection;
      if( ssel.size() > 1 )
        return;
      Object obj = ssel.getFirstElement();
      if( obj instanceof IResource )
      {
        IContainer container;
        if( obj instanceof IContainer )
          container = (IContainer)obj;
        else
          container = ( (IResource)obj ).getParent();
        containerText.setText( container.getFullPath().toString() );
      }
    }
    fileText.setText( "Karte.gmt" );
  }

  /**
   * Uses the standard container selection dialog to choose the new value for
   * the container field.
   */

  void handleBrowse()
  {
    ContainerSelectionDialog dialog = new ContainerSelectionDialog( getShell(), ResourcesPlugin
        .getWorkspace().getRoot(), false, "Select new file container" );
    if( dialog.open() == Window.OK )
    {
      Object[] result = dialog.getResult();
      if( result.length == 1 )
      {
        containerText.setText( ( (Path)result[0] ).toOSString() );
      }
    }
  }

  /**
   * Ensures that both text fields are set.
   */

  void dialogChanged()
  {
    String container = getContainerName();
    String fileName = getFileName();

    if( container.length() == 0 )
    {
      updateStatus( "File container must be specified" );
      return;
    }
    if( fileName.length() == 0 )
    {
      updateStatus( "File name must be specified" );
      return;
    }
    int dotLoc = fileName.lastIndexOf( '.' );
    if( dotLoc != -1 )
    {
      String ext = fileName.substring( dotLoc + 1 );
      if( ext.equalsIgnoreCase( "gmt" ) == false )
      {
        updateStatus( "File extension must be \"gmt\"" );
        return;
      }
    }
    updateStatus( null );
  }

  private void updateStatus( String message )
  {
    setErrorMessage( message );
    setPageComplete( message == null );
  }

  public String getContainerName()
  {
    return containerText.getText();
  }

  public String getFileName()
  {
    return fileText.getText();
  }
}