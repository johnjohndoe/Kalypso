package org.kalypso.ui.editor;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.IStatusLineManager;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IEditorActionBarContributor;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IStorageEditorInput;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.dialogs.SaveAsDialog;
import org.eclipse.ui.part.EditorActionBarContributor;
import org.eclipse.ui.part.EditorPart;
import org.eclipse.ui.part.FileEditorInput;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.command.DefaultCommandManager;
import org.kalypso.util.command.ICommand;
import org.kalypso.util.command.ICommandTarget;
import org.kalypso.util.command.JobExclusiveCommandTarget;

/**
 * @author bce
 */
public abstract class AbstractEditorPart extends EditorPart implements
    IResourceChangeListener, ICommandTarget
{
  private final Runnable m_dirtyRunnable = new Runnable()
  {
    public void run( )
    {
      getEditorSite().getShell().getDisplay().asyncExec( new Runnable()
      {
        public void run( )
        {
          fireDirty();
        }
      } );
    }
  };

  private boolean m_isSaving = false;

  protected JobExclusiveCommandTarget m_commandTarget = new JobExclusiveCommandTarget(
      new DefaultCommandManager(), m_dirtyRunnable );

  public AbstractEditorPart( )
  {
    ResourcesPlugin.getWorkspace().addResourceChangeListener( this );
  }

  public void dispose( )
  {
    ResourcesPlugin.getWorkspace().removeResourceChangeListener( this );

    m_commandTarget.dispose();

    super.dispose();
  }

  /**
   * @see org.eclipse.ui.part.EditorPart#doSave(org.eclipse.core.runtime.IProgressMonitor)
   */
  public final void doSave( final IProgressMonitor monitor )
  {
    // save only possible when input is a file
    if( !(getEditorInput() instanceof FileEditorInput) )
    {
      // given user a chance to use save-as
      MessageDialog.openInformation( getSite().getShell(), "Speichern", "Der Inhalt kann nicht direkt gespeichert werden.\n" +
      		"Bitte benutzen Sie das 'Speichern als' Kommando." );

      return;
    }

    final IFileEditorInput input = (IFileEditorInput) getEditorInput();

    if( input != null )
    {
      m_isSaving = true;
      try
      {
        doSaveInternal( monitor, input );
        m_commandTarget.resetDirty();
        fireDirty();
      }
      catch( CoreException e )
      {
        e.printStackTrace();

        ErrorDialog.openError( getEditorSite().getShell(), "Fehler",
            "Fehler beim Speichern der Ansicht", e.getStatus() );
      }
      finally
      {
        m_isSaving = false;
      }
    }
  }

  protected abstract void doSaveInternal( final IProgressMonitor monitor,
      final IFileEditorInput input ) throws CoreException;

  /**
   * Returns the status line manager of this editor.
   * 
   * @return the status line manager of this editor
   */
  private IStatusLineManager getStatusLineManager( )
  {
    final IEditorActionBarContributor contributor = getEditorSite()
        .getActionBarContributor();
    if( !(contributor instanceof EditorActionBarContributor) )
      return null;

    final IActionBars actionBars = ((EditorActionBarContributor) contributor)
        .getActionBars();
    if( actionBars == null )
      return null;

    return actionBars.getStatusLineManager();
  }

  /**
   * @see org.eclipse.ui.part.EditorPart#isDirty()
   */
  public boolean isDirty( )
  {
    return m_commandTarget.isDirty();
  }

  /**
   * Returns the progress monitor related to this editor.
   * 
   * @return the progress monitor related to this editor
   */
  protected IProgressMonitor getProgressMonitor( )
  {
    IProgressMonitor pm = null;

    IStatusLineManager manager = getStatusLineManager();
    if( manager != null )
      pm = manager.getProgressMonitor();

    return pm != null ? pm : new NullProgressMonitor();
  }

  /**
   * @see org.eclipse.ui.part.EditorPart#doSaveAs()
   */
  public void doSaveAs( )
  {
    final Shell shell = getSite().getShell();
    final IEditorInput input = getEditorInput();

    final SaveAsDialog dialog = new SaveAsDialog( shell );

    final IFile original = (input instanceof IFileEditorInput) ? ((IFileEditorInput) input)
        .getFile()
        : null;
    if( original != null )
      dialog.setOriginalFile( original );

    dialog.create();

    if( dialog.open() == Window.CANCEL )
      return;

    final IPath filePath = dialog.getResult();
    if( filePath == null )
      return;

    final IProgressMonitor monitor = getProgressMonitor();

    final IWorkspace workspace = ResourcesPlugin.getWorkspace();
    final IFile file = workspace.getRoot().getFile( filePath );
    final IFileEditorInput newInput = new FileEditorInput( file );

    try
    {
      monitor.beginTask( "Save file", 1000 );
      doSaveInternal( new SubProgressMonitor( monitor, 1000 ), newInput );
      m_commandTarget.resetDirty();

      monitor.done();
    }
    catch( final CoreException ce )
    {
      ce.printStackTrace();

      ErrorDialog.openError( shell, "Fehler",
          "Fehlern beim Speichern der Ansicht", ce.getStatus() );
      return;
    }

    setInput( newInput );
  }

  /**
   * @see org.eclipse.ui.part.EditorPart#init(org.eclipse.ui.IEditorSite,
   *      org.eclipse.ui.IEditorInput)
   */
  public void init( IEditorSite site, IEditorInput input )
      throws PartInitException
  {
    // do not check because we have also string storage...
    // TODO: find a long lasting solution to this problem
    //    if( !( input instanceof IFileEditorInput ) )
    //      throw new PartInitException( "Can only use IFileEditorInput" );

    setSite( site );

    setInput( input );
  }

  /**
   * @see org.eclipse.ui.part.EditorPart#isSaveAsAllowed()
   */
  public boolean isSaveAsAllowed( )
  {
    return true;
  }

  /**
   * @see org.eclipse.ui.part.EditorPart#setInput(org.eclipse.ui.IEditorInput)
   */
  protected final void setInput( IEditorInput input )
  {
    super.setInput( input );

    load();
  }

  protected final void load( )
  {
    new Job( "Dokument laden" )
    {
      protected IStatus run( final IProgressMonitor monitor )
      {
        final IStorageEditorInput input = (IStorageEditorInput) getEditorInput();

        try
        {
          loadInternal( monitor, input );
        }
        catch( final CoreException e )
        {
          e.printStackTrace();

          monitor.done();
          return e.getStatus();
        }
        catch( final Exception e )
        {
          e.printStackTrace();

          monitor.done();
          return new Status( IStatus.ERROR, KalypsoGisPlugin.getId(), 0,
              "Fehler beim Laden der Ansicht", e );
        }

        m_commandTarget.resetDirty();

        getEditorSite().getShell().getDisplay().syncExec( new Runnable()
        {
          public void run( )
          {
            setDocumentTitle( input.getName() );
          }
        } );

        monitor.done();
        return Status.OK_STATUS;
      }
    }.schedule();
  }

  protected abstract void loadInternal( final IProgressMonitor monitor,
      final IStorageEditorInput input ) throws Exception, CoreException;

  /**
   * @see org.eclipse.core.resources.IResourceChangeListener#resourceChanged(org.eclipse.core.resources.IResourceChangeEvent)
   */
  public void resourceChanged( final IResourceChangeEvent event )
  {
    final IFileEditorInput input = (IFileEditorInput) getEditorInput();

    if( event.getType() == IResourceChangeEvent.POST_CHANGE && input != null )
    {
      final IResourceDelta delta = event.getDelta().findMember(
          input.getFile().getFullPath() );
      if( delta != null && delta.getKind() == IResourceDelta.CHANGED )
      {
        // TODO: ask user?
        if( !m_isSaving
            && MessageDialog.openQuestion( getSite().getShell(),
                "FeatureEditor",
                "Die Vorlagendatei hat sich geändert. Neu laden?" ) )
          load();
      }
    }
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  public void createPartControl( final Composite parent )
  {
    final IActionBars actionBars = getEditorSite().getActionBars();
    actionBars.setGlobalActionHandler( ActionFactory.UNDO.getId(),
        m_commandTarget.undoAction );
    actionBars.setGlobalActionHandler( ActionFactory.REDO.getId(),
        m_commandTarget.redoAction );

    actionBars.updateActionBars();
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#setFocus()
   */
  public void setFocus( )
  {
    // nix
  }

  public void fireDirty( )
  {
    firePropertyChange( PROP_DIRTY );
  }

  /**
   * @see org.kalypso.util.command.ICommandTarget#postCommand(org.kalypso.util.command.ICommand,
   *      java.lang.Runnable)
   */
  public void postCommand( final ICommand command, final Runnable runnable )
  {
    m_commandTarget.postCommand( command, runnable );
  }

  public JobExclusiveCommandTarget getCommandTarget( )
  {
    return m_commandTarget;
  }

  protected void setDocumentTitle( final String name )
  {
    setContentDescription( name );
    setPartName( name );
  }
}