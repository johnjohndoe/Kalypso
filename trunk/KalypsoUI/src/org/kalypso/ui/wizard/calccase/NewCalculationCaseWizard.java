package org.kalypso.ui.wizard.calccase;

import java.lang.reflect.InvocationTargetException;
import java.text.MessageFormat;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceStatus;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.eclipse.ui.dialogs.ContainerGenerator;
import org.eclipse.ui.internal.ide.IDEWorkbenchMessages;
import org.eclipse.ui.internal.ide.IDEWorkbenchPlugin;
import org.eclipse.ui.wizards.newresource.BasicNewResourceWizard;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.nature.ModelNature;

/**
 * 
 * @author belger
 */
public class NewCalculationCaseWizard extends BasicNewResourceWizard
{
  private NewCalculationCaseCreateFolderPage m_createFolderPage;

  private SteuerparameterWizardPage m_createControlPage;

  /**
   * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench,
   *      org.eclipse.jface.viewers.IStructuredSelection)
   */
  public void init( final IWorkbench workbench, final IStructuredSelection currentSelection )
  {
    super.init( workbench, currentSelection );
    setWindowTitle( "neuer Rechenfall" );
    setNeedsProgressMonitor( true );
  }

  /**
   * @see org.eclipse.jface.wizard.IWizard#addPages()
   */
  public void addPages()
  {
    super.addPages();
    m_createFolderPage = new NewCalculationCaseCreateFolderPage( "Rechenfall", getSelection() );
    m_createControlPage = new SteuerparameterWizardPage( m_createFolderPage, false, ImageProvider.IMAGE_KALYPSO_ICON_BIG );
    
    m_createControlPage.setUpdate( true );

    addPage( m_createFolderPage );
    addPage( m_createControlPage );
  }

  /**
   * @see org.eclipse.jface.wizard.IWizard#performFinish()
   */
  public boolean performFinish()
  {
    // zuerst die Grunddaten erzeugen erzeugen
    final IFolder folder = createCalculationCase();
    if( folder == null )
      return false;

    // im Navigator zeigen
    selectAndReveal( folder );

    return true;
  }

  private IFolder createCalculationCase()
  {
    final IFolder newFolderHandle = m_createFolderPage.getFolder();

    final SteuerparameterWizardPage controlPage = m_createControlPage;

    WorkspaceModifyOperation op = new WorkspaceModifyOperation( null )
    {
      public void execute( final IProgressMonitor monitor ) throws CoreException
      {
        try
        {
          monitor.beginTask( IDEWorkbenchMessages
              .getString( "WizardNewFolderCreationPage.progress" ), 5000 ); //$NON-NLS-1$
          final ContainerGenerator generator = new ContainerGenerator( newFolderHandle.getParent()
              .getFullPath() );
          generator.generateContainer( new SubProgressMonitor( monitor, 1000 ) );
          createFolder( newFolderHandle, new SubProgressMonitor( monitor, 1000 ) );
          final ModelNature nature = (ModelNature)newFolderHandle.getProject().getNature(ModelNature.ID);
          nature.createCalculationCaseInFolder( newFolderHandle, new SubProgressMonitor(
              monitor, 1000 ) );
          controlPage.saveChanges( newFolderHandle, new SubProgressMonitor( monitor, 1000 ) );
          if( controlPage.isUpdate() )
            nature.updateCalcCase( newFolderHandle, new SubProgressMonitor( monitor, 1000 ) );
          else
            monitor.worked( 1000 );
        }
        finally
        {
          monitor.done();
        }
      }
    };

    try
    {
      getContainer().run( true, true, op );
    }
    catch( final InterruptedException e )
    {
      cleanup( newFolderHandle );

      return null;
    }
    catch( final InvocationTargetException e )
    {
      e.printStackTrace();

      if( e.getTargetException() instanceof CoreException )
      {
        ErrorDialog.openError( getContainer().getShell(), // Was
            // Utilities.getFocusShell()
            IDEWorkbenchMessages.getString( "WizardNewFolderCreationPage.errorTitle" ), //$NON-NLS-1$
            null, // no special message
            ( (CoreException)e.getTargetException() ).getStatus() );
      }
      else
      {
        // CoreExceptions are handled above, but unexpected runtime exceptions
        // and errors may still occur.

        IDEWorkbenchPlugin
            .log( MessageFormat
                .format(
                    "Exception in {0}.getNewFolder(): {1}", new Object[] { getClass().getName(), e.getTargetException() } ) );//$NON-NLS-1$
        MessageDialog
            .openError(
                getContainer().getShell(),
                IDEWorkbenchMessages.getString( "WizardNewFolderCreationPage.internalErrorTitle" ), IDEWorkbenchMessages.format( "WizardNewFolder.internalError", new Object[] { e.getTargetException().getMessage() } ) ); //$NON-NLS-2$ //$NON-NLS-1$
      }

      cleanup( newFolderHandle );

      return null; // ie.- one of the steps resulted in a core exception
    }

    return newFolderHandle;
  }

  private void cleanup( final IFolder folder )
  {
    try
    {
      folder.delete( true, new NullProgressMonitor() );
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
    }
  }

  /**
   * Creates a folder resource given the folder handle.
   * 
   * @param folderHandle
   *          the folder handle to create a folder resource for
   * @param monitor
   *          the progress monitor to show visual progress with
   * @exception CoreException
   *              if the operation fails
   * @exception OperationCanceledException
   *              if the operation is canceled
   * 
   * TODO: move this code to FolderUtilities
   */
  protected void createFolder( final IFolder folderHandle, final IProgressMonitor monitor )
      throws CoreException
  {
    try
    {
      // Create the folder resource in the workspace
      // Update: Recursive to create any folders which do not exist already
      if( !folderHandle.exists() )
      {
        final IContainer parent = folderHandle.getParent();
        if( parent instanceof IFolder && ( !( (IFolder)parent ).exists() ) )
          createFolder( (IFolder)parent, monitor );

        folderHandle.create( false, true, monitor );
      }
    }
    catch( final CoreException e )
    {
      // If the folder already existed locally, just refresh to get contents
      if( e.getStatus().getCode() == IResourceStatus.PATH_OCCUPIED )
        folderHandle
            .refreshLocal( IResource.DEPTH_INFINITE, new SubProgressMonitor( monitor, 500 ) );
      else
        throw e;
    }

    if( monitor.isCanceled() )
      throw new OperationCanceledException();
  }
  
  /**
   * @see org.eclipse.jface.wizard.IWizard#createPageControls(org.eclipse.swt.widgets.Composite)
   */
  public void createPageControls( final Composite pageContainer )
  {
    // nichts tun, die Seiten sollen lazy initialisiert werden
    // weil die Steuerparameter Seite erst aufgebaut werden kann,
    // wenn das Projekt festliegt
  }
}