package org.kalypso.editor;

import javax.swing.event.EventListenerList;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.GroupMarker;
import org.eclipse.jface.action.IStatusLineManager;
import org.eclipse.jface.action.IToolBarManager;
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
import org.kalypso.editor.mapeditor.IGisviewEditorListener;
import org.kalypso.editor.mapeditor.WidgetAction;
import org.kalypso.util.command.DefaultCommandManager;
import org.kalypso.util.command.ICommand;
import org.kalypso.util.command.ICommandManager;
import org.kalypso.util.command.ICommandManagerListener;
import org.kalypso.util.command.RedoAction;
import org.kalypso.util.command.UndoAction;

/**
 * @author bce
 */
public abstract class AbstractEditorPart extends EditorPart implements ICommandManager, IResourceChangeListener
{
  private boolean m_dirty = false;

  private EventListenerList m_listeners = new EventListenerList();

  private final ICommandManager m_commandManager = new DefaultCommandManager();
  
  public final UndoAction m_undoAction = new UndoAction( this );

  public final RedoAction m_redoAction = new RedoAction( this );

  /** Jeder Editor hat sein eigenes Mutex, so dass Jobs sch�n hintereinander ausgef�hrt werden */
  private Mutex myMutexRule = new Mutex();
  
  /** Used, to update SWT in DisplayThread */
  private Runnable myUpdater = new Runnable() 
  {
    public void run()
    {
      fireGisviewChanged();
      }};

      
  public AbstractEditorPart()
  {
    ResourcesPlugin.getWorkspace().addResourceChangeListener( this );
  }
  
  protected abstract WidgetAction[] createWidgetActions(  );
      
  public void dispose()
  {
    ResourcesPlugin.getWorkspace().removeResourceChangeListener( this );

    m_undoAction.dispose();
    m_redoAction.dispose();

    super.dispose();
  }

  
  /**
   * @see org.eclipse.ui.part.EditorPart#doSave(org.eclipse.core.runtime.IProgressMonitor)
   */
  public final void doSave( final IProgressMonitor monitor )
  {
    final IFileEditorInput input = (IFileEditorInput)getEditorInput();

    if( input == null )
      doSaveInternal( monitor, input );
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

    //firePropertyChange( PROP_DIRTY );
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
  public void resourceChanged( IResourceChangeEvent event )
  {
    final IFileEditorInput input = (IFileEditorInput)getEditorInput();

    if( event.getType() == IResourceChangeEvent.POST_CHANGE && input != null )
    {
      final IResourceDelta delta = event.getDelta().findMember( input.getFile().getFullPath() );
      if( delta != null && delta.getKind() == IResourceDelta.CHANGED )
        load();
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

    final String groupName = this.toString();
    System.out.println( groupName );
    
    final IToolBarManager toolBarManager = actionBars.getToolBarManager();
    toolBarManager.add( new GroupMarker( groupName ) );

    // TODO: move to createPartcontrol
    final WidgetAction[] widgetActions = createWidgetActions();
    for( int i = 0; i < widgetActions.length; i++ )
      toolBarManager.appendToGroup( groupName, widgetActions[i] );

    actionBars.updateActionBars();
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#setFocus()
   */
  public void setFocus()
  {
    // nix
  }

  /**
   * @see org.kalypso.util.command.ICommandManager#postCommand(org.kalypso.util.command.ICommand)
   */
  public void postCommand( final ICommand c )
  {
    final Job job = new GisviewJob( this, c, m_commandManager, myUpdater );
    job.schedule();
  }

  /**
   * @see org.kalypso.util.command.ICommandManager#redo()
   */
  public void redo()
  {
    m_commandManager.redo();
  
    fireGisviewChanged();
  }

  /**
   * @see org.kalypso.util.command.ICommandManager#undo()
   */
  public void undo()
  {
    m_commandManager.undo();
  
    fireGisviewChanged();
  
    // TODO. geht das?
    //    if( m_commandManager.canUndo() )
    //      setDirty( false );
  }

  /**
   * @see org.kalypso.util.command.ICommandManager#canUndo()
   */
  public boolean canUndo()
  {
    return m_commandManager.canUndo();
  }

  /**
   * @see org.kalypso.util.command.ICommandManager#canRedo()
   */
  public boolean canRedo()
  {
    return m_commandManager.canRedo();
  }

  /**
   * 
   * @see org.kalypso.util.command.ICommandManager#getUndoDescription()
   */
  public String getUndoDescription()
  {
    return m_commandManager.getUndoDescription();
  }

  /**
   * 
   * @see org.kalypso.util.command.ICommandManager#getRedoDescription()
   */
  public String getRedoDescription()
  {
    return m_commandManager.getRedoDescription();
  }

  /**
   * @see org.kalypso.util.command.ICommandManager#addCommandManagerListener(org.kalypso.util.command.ICommandManagerListener)
   */
  public void addCommandManagerListener( ICommandManagerListener l )
  {
    m_commandManager.addCommandManagerListener( l );
  }

  /**
   * @see org.kalypso.util.command.ICommandManager#removeCommandManagerListener(org.kalypso.util.command.ICommandManagerListener)
   */
  public void removeCommandManagerListener( ICommandManagerListener l )
  {
    m_commandManager.removeCommandManagerListener( l );
  }
  
  public void addGisMapEditorListener( final IGisviewEditorListener l )
  {
    m_listeners.add( IGisviewEditorListener.class, l );
  }

  public void removeGisMapEditorListener( final IGisviewEditorListener l )
  {
    m_listeners.remove( IGisviewEditorListener.class, l );
  }

  public void fireGisviewChanged()
  {
    final IGisviewEditorListener[] listeners = (IGisviewEditorListener[])m_listeners
        .getListeners( IGisviewEditorListener.class );
    for( int i = 0; i < listeners.length; i++ )
      listeners[i].onGisviewChanged();
  }
  
  public ISchedulingRule getShedulingRule()
  {
    return myMutexRule;
  }
  
  private final static class GisviewJob extends Job
  {
    private final ICommand myCommand;

    private final AbstractEditorPart myEditor;

    private final ICommandManager myCommandManager;
    
    private final Runnable myUpdater;

    public GisviewJob( final AbstractEditorPart editor, final ICommand command,
        final ICommandManager commandManager, final Runnable updater )
    {
      super( command.getDescription() );

      setRule( editor.getShedulingRule() );
      setUser( true );
      setPriority( Job.SHORT );

      myCommand = command;
      myEditor = editor;
      myCommandManager = commandManager;
      myUpdater = updater;
    }

    protected IStatus run( final IProgressMonitor monitor )
    {
      // TODO try/catch and set error
      myEditor.setDirty( true );

      // TODO use monitor
      myCommandManager.postCommand( myCommand );

      // TODO: this should do the MapView-Modell (by activating a setter)
      myEditor.getEditorSite().getShell().getDisplay().asyncExec( myUpdater );

      return Status.OK_STATUS;
    }
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


}
