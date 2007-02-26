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
import org.kalypso.afgui.model.ITask;
import org.kalypso.afgui.model.IWorkflow;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;

/**
 * @author Stefan Kurzbach
 */
public class WorkflowControl2
{
  final static private Logger logger = Logger.getLogger( WorkflowControl2.class.getName() );

  private static final boolean log = Boolean.parseBoolean( Platform.getDebugOption( "org.kalypso.afgui/debug" ) );

  static
  {
    if( !log )
      logger.setUseParentHandlers( false );
  }

  private TreeViewer m_treeViewer;

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

      private TreePath lastTreePath;

      public void selectionChanged( final SelectionChangedEvent event )
      {
        final ITreeSelection selection = (ITreeSelection) event.getSelection();
        final Object first = selection.getFirstElement();
        if( first != null && lastTreePath != null ? lastTreePath.getLastSegment() != first : true )
        {
          final TreePath newTreePath = selection.getPathsFor( first )[0];
          if( lastTreePath != null )
          {
            final Object segment = lastTreePath.getSegment( Math.min( lastTreePath.getSegmentCount(), newTreePath.getSegmentCount() ) - 1);
            m_treeViewer.collapseToLevel( segment, TreeViewer.ALL_LEVELS );
          }
          m_treeViewer.expandToLevel( first, 1 );
          lastTreePath = newTreePath;
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

  public void setVisible( final boolean visible )
  {
    m_treeViewer.getControl().setVisible( visible );
  }

  public void restoreState( IMemento memento )
  {
    // TODO Auto-generated method stub

  }

  public void saveState( IMemento memento )
  {
    // TODO Auto-generated method stub

  }

  public void setWorkflow( final IWorkflow workflow )
  {
    m_treeViewer.setInput( workflow );
  }
}
