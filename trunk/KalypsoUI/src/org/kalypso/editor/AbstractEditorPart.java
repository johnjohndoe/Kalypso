package org.kalypso.editor;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.jface.action.IStatusLineManager;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IEditorActionBarContributor;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.dialogs.SaveAsDialog;
import org.eclipse.ui.part.EditorActionBarContributor;
import org.eclipse.ui.part.EditorPart;
import org.eclipse.ui.part.FileEditorInput;
import org.kalypso.util.command.DefaultCommandManager;
import org.kalypso.util.command.ICommandManager;
import org.kalypso.util.command.ICommandManagerListener;
import org.kalypso.util.command.UndoRedoAction;

/**
 * @author bce
 */
public abstract class AbstractEditorPart extends EditorPart implements IResourceChangeListener, ICommandManagerListener
{
  private boolean m_dirty = false;

  private final ICommandManager m_commandManager = new DefaultCommandManager();
  
  public final UndoRedoAction m_undoAction = new UndoRedoAction( m_commandManager, getSchedulingRule(), true );
  public final UndoRedoAction m_redoAction = new UndoRedoAction( m_commandManager, getSchedulingRule(), false );
  
  private final Runnable m_dirtyRunnable = new Runnable()
  {
    public void run()
    {
      fireDirty();
    }
  };

  /** Jeder Editor hat sein eigenes Mutex, so dass Jobs sch?n hintereinander ausgef?hrt werden */
  private Mutex myMutexRule = new Mutex();

  private boolean m_isSaving = false;
  
  public AbstractEditorPart()
  {
    ResourcesPlugin.getWorkspace().addResourceChangeListener( this );
    
    m_commandManager.addCommandManagerListener( this );
  }
  
  public void dispose()
  {
    ResourcesPlugin.getWorkspace().removeResourceChangeListener( this );

    m_undoAction.dispose();
    m_redoAction.dispose();
    
    m_commandManager.removeCommandManagerListener( this );

    super.dispose();
  }
  
  /**
   * @see org.eclipse.core.runtime.IAdaptable#getAdapter(java.lang.Class)
   */
  public Object getAdapter( Class adapter )
  {
    System.out.println( adapter.getName() );
    return null;
  }

  
  /**
   * @see org.eclipse.ui.part.EditorPart#doSave(org.eclipse.core.runtime.IProgressMonitor)
   */
  public final void doSave( final IProgressMonitor monitor )
  {
    final IFileEditorInput input = (IFileEditorInput)getEditorInput();

    if( input != null )
    {
      try
      {
        m_isSaving = true;
        doSaveInternal( monitor, input );
      }
      finally
      {
        m_isSaving = false;
      }
    }
  }
  
  protected abstract void doSaveInternal( final IProgressMonitor monitor, final IFileEditorInput input );

  /**
   * Returns the status line manager of this editor.
   * 
   * @return the status line manager of this editor
   */
  private IStatusLineManager getStatusLineManager()
  {
    final IEditorActionBarContributor contributor = getEditorSite().getActionBarContributor();
    if( !( contributor instanceof EditorActionBarContributor ) )
      return null;

    final IActionBars actionBars = ( (EditorActionBarContributor)contributor ).getActionBars();
    if( actionBars == null )
      return null;

    return actionBars.getStatusLineManager();
  }

  /**
   * Returns the progress monitor related to this editor.
   * 
   * @return the progress monitor related to this editor
   */
  protected IProgressMonitor getProgressMonitor()
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
  public void doSaveAs()
  {
    final Shell shell = getSite().getShell();
    final IEditorInput input = getEditorInput();

    final SaveAsDialog dialog = new SaveAsDialog( shell );

    final IFile original = ( input instanceof IFileEditorInput ) ? ( (IFileEditorInput)input )
        .getFile() : null;
    if( original != null )
      dialog.setOriginalFile( original );

    dialog.create();

    final IProgressMonitor progressMonitor = getProgressMonitor();

    if( dialog.open() == Window.CANCEL )
    {
      if( progressMonitor != null )
        progressMonitor.setCanceled( true );
      return;
    }

    final IPath filePath = dialog.getResult();
    if( filePath == null )
    {
      if( progressMonitor != null )
        progressMonitor.setCanceled( true );
      return;
    }

    final IWorkspace workspace = ResourcesPlugin.getWorkspace();
    final IFile file = workspace.getRoot().getFile( filePath );
    final IFileEditorInput newInput = new FileEditorInput( file );

    doSaveInternal( progressMonitor, newInput );

    if( progressMonitor != null )
      progressMonitor.setCanceled( false );

    setInput( newInput );
  }

  /**
   * @see org.eclipse.ui.part.EditorPart#init(org.eclipse.ui.IEditorSite,
   *      org.eclipse.ui.IEditorInput)
   */
  public void init( IEditorSite site, IEditorInput input ) throws PartInitException
  {
    if( !( input instanceof IFileEditorInput ) )
      throw new PartInitException( "Can only use IFileEditorInput" );

    setSite( site );

    setInput( input );
  }

  /**
   * @see org.eclipse.ui.part.EditorPart#isDirty()
   */
  public boolean isDirty()
  {
    return m_dirty;
  }
  
  public void setDirty( final boolean dirty )
  {
    m_dirty = dirty;

    getEditorSite().getShell().getDisplay().asyncExec( m_dirtyRunnable );
  }

  /**
   * @see org.eclipse.ui.part.EditorPart#isSaveAsAllowed()
   */
  public boolean isSaveAsAllowed()
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
  
  protected abstract void load();

  /**
   * @see org.eclipse.core.resources.IResourceChangeListener#resourceChanged(org.eclipse.core.resources.IResourceChangeEvent)
   */
  public void resourceChanged( final IResourceChangeEvent event )
  {
    final IFileEditorInput input = (IFileEditorInput)getEditorInput();

    if( event.getType() == IResourceChangeEvent.POST_CHANGE && input != null )
    {
      final IResourceDelta delta = event.getDelta().findMember( input.getFile().getFullPath() );
      if( delta != null && delta.getKind() == IResourceDelta.CHANGED )
      {
        if( !m_isSaving )
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
    actionBars.setGlobalActionHandler( ActionFactory.UNDO.getId(), m_undoAction );
    actionBars.setGlobalActionHandler( ActionFactory.REDO.getId(), m_redoAction );

    actionBars.updateActionBars();
  }
  /**
   * @see org.eclipse.ui.IWorkbenchPart#setFocus()
   */
  public void setFocus()
  {
    // nix
  }
  
  public ICommandManager getCommandManager()
  {
    return m_commandManager;
  }

  public ISchedulingRule getSchedulingRule()
  {
    return myMutexRule;
  }
  
  class Mutex implements ISchedulingRule
  {
    public boolean isConflicting( ISchedulingRule rule )
    {
      return rule == this;
    }

    public boolean contains( ISchedulingRule rule )
    {
      return rule == this;
    }
  }

  public void fireDirty()
  {
    firePropertyChange( PROP_DIRTY );
  }

  /**
   * @see org.kalypso.util.command.ICommandManagerListener#onCommandManagerChanged(org.kalypso.util.command.ICommandManager)
   */
  public void onCommandManagerChanged( final ICommandManager source )
  {
    if( source == m_commandManager )
      setDirty( true );
  }
}
