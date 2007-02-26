/**
 * 
 */
package org.kalypso.afgui.views;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.commands.Category;
import org.eclipse.core.commands.Command;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ITreeSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreePath;
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.afgui.model.IPhase;
import org.kalypso.afgui.model.ISubTaskGroup;
import org.kalypso.afgui.model.ITask;
import org.kalypso.afgui.model.ITaskGroup;
import org.kalypso.afgui.model.IWorkflow;
import org.kalypso.afgui.model.IWorkflowPart;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;

/**
 * @author Stefan Kurzbach
 */
public class WorkflowControl2
{
  final static Logger logger = Logger.getLogger( WorkflowControl2.class.getName() );

  private static final boolean log = Boolean.parseBoolean( Platform.getDebugOption( "org.kalypso.afgui/debug" ) );

  private static final String MEMENTO_LAST_SELECTION = "lastSelection";

  static
  {
    if( !log )
      logger.setUseParentHandlers( false );
  }

  TreeViewer m_treeViewer;

  TreePath m_lastTreePath;

  private String m_selectionFromMemento;

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    final Composite top = new Composite( parent, SWT.FILL );
    top.setLayout( new FillLayout() );
    m_treeViewer = new TreeViewer( top, SWT.SINGLE );
    m_treeViewer.setContentProvider( new WorkflowContentProvider() );
    m_treeViewer.setLabelProvider( new WorkflowLabelProvider( m_treeViewer ) );
    m_treeViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      private static final String TASKS_COMMANDS_CATEGORY = "org.kalypso.kalypso1d2d.pjt.TasksCommands";

      public void selectionChanged( final SelectionChangedEvent event )
      {
        final ITreeSelection selection = (ITreeSelection) event.getSelection();
        final Object first = selection.getFirstElement();
        if( first != null && m_lastTreePath != null ? m_lastTreePath.getLastSegment() != first : true )
        {
          final TreePath newTreePath = selection.getPathsFor( first )[0];
          if( m_lastTreePath != null )
          {
            final Object segment = m_lastTreePath.getSegment( Math.min( m_lastTreePath.getSegmentCount(), newTreePath.getSegmentCount() ) - 1 );
            m_treeViewer.collapseToLevel( segment, TreeViewer.ALL_LEVELS );
          }
          m_treeViewer.expandToLevel( first, 1 );
          m_lastTreePath = newTreePath;
          if( first instanceof ITask )
          {
            doTaskOrActivity( (ITask) first );
          }
          m_treeViewer.setSelection( new StructuredSelection( first ) );
        }
      }

      private final void doTaskOrActivity( final ITask task )
      {
        final IWorkbench workbench = PlatformUI.getWorkbench();
        final ICommandService commandService = (ICommandService) workbench.getService( ICommandService.class );
        final String name = task.getURI();
        try
        {
          final Command command = getCommand( commandService, name );
          final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
          handlerService.executeCommand( command.getId(), null );
        }
        catch( final Throwable e )
        {
          final IStatus status = StatusUtilities.statusFromThrowable( e );
          ErrorDialog.openError( m_treeViewer.getControl().getShell(), "Workflow Commmand", "Kommando konnte nicht ausgeführt werden: " + name, status );
          KalypsoAFGUIFrameworkPlugin.getDefault().getLog().log( status );
          logger.log( Level.SEVERE, "Failed to execute command: " + name, e );
        }
      }

      Command getCommand( final ICommandService commandService, final String commandId )
      {
        final Command command = commandService.getCommand( commandId );
        if( !command.isDefined() )
        {
          final Category category = commandService.getCategory( "org.kalypso.afgui.tasks" );
          if( !category.isDefined() )
          {
            category.define( TASKS_COMMANDS_CATEGORY, null );
          }
          command.define( commandId, null, category );
        }
        return command;
      }
    } );
  }

  private TreePath findPart( final String uri, final IWorkflow workflow )
  {
    TreePath result = null;
    for( final IPhase phase : workflow.getPhases() )
    {
      if( phase.getURI().equals( uri ) )
      {
        result = new TreePath( new Object[] { phase } );
      }
      else
      {
        for( final ITaskGroup taskGroup : phase.getTaskGroups() )
        {
          if( taskGroup.getURI().equals( uri ) )
          {
            result = new TreePath( new Object[] { phase, taskGroup } );
          }
          else
          {
            for( final ISubTaskGroup subTaskGroup : taskGroup.getSubTaskGroups() )
            {
              if( subTaskGroup.getURI().equals( uri ) )
              {
                result = new TreePath( new Object[] { phase, taskGroup, subTaskGroup } );
              }
              else
              {
                for( final ITask task : subTaskGroup.getTasks() )
                {
                  if( task.getURI().equals( uri ) )
                  {
                    result = new TreePath( new Object[] { phase, taskGroup, subTaskGroup, task } );
                  }
                }
              }
            }
            for( final ITask task : taskGroup.getTasks() )
            {
              if( task.getURI().equals( uri ) )
              {
                result = new TreePath( new Object[] { phase, taskGroup, task } );
              }
            }
          }
        }
      }
    }
    return result;
  }

  public void setVisible( final boolean visible )
  {
    m_treeViewer.getControl().setVisible( visible );
  }

  public void restoreState( final IMemento memento )
  {
    if( memento != null )
    {
      m_selectionFromMemento = memento.getString( MEMENTO_LAST_SELECTION );
    }
  }

  public void saveState( final IMemento memento )
  {
    if( m_lastTreePath != null )
    {
      final Object lastSegment = m_lastTreePath.getLastSegment();
      memento.putString( MEMENTO_LAST_SELECTION, ((IWorkflowPart) lastSegment).getURI() );
    }
  }

  public void setWorkflow( final IWorkflow workflow )
  {
    m_treeViewer.setInput( workflow );
    m_treeViewer.refresh();
    if( m_selectionFromMemento != null && workflow != null )
    {
      final TreePath findPart = findPart( m_selectionFromMemento, workflow );
      if( findPart != null && findPart.getParentPath() != null)
      {        
        m_treeViewer.setSelection( new TreeSelection( findPart.getParentPath() ), true );
      }
      m_selectionFromMemento = null;
    }
  }
}
