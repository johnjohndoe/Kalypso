/**
 *
 */
package org.kalypso.afgui.views;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.AbstractTreeViewer;
import org.eclipse.jface.viewers.ColumnViewerToolTipSupport;
import org.eclipse.jface.viewers.IOpenListener;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ITreeSelection;
import org.eclipse.jface.viewers.OpenEvent;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreePath;
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;

import de.renew.workflow.base.Task;
import de.renew.workflow.base.Workflow;
import de.renew.workflow.connector.IWorklistChangeListener;
import de.renew.workflow.connector.worklist.ITaskExecutionListener;
import de.renew.workflow.connector.worklist.ITaskExecutor;

/**
 * @author Stefan Kurzbach
 */
public class WorkflowControl implements IWorklistChangeListener, ITaskExecutionListener
{
  private TreeViewer m_treeViewer;

  private TreePath m_lastTreePath;

  private Object m_lastSelectedElement;

  private final ITaskExecutor m_taskExecutor;

  private Composite m_topControl;

  public WorkflowControl( final ITaskExecutor taskExecutor )
  {
    m_taskExecutor = taskExecutor;

    taskExecutor.addTaskExecutionListener( this );
  }

  public ITaskExecutor getTaskExecutor( )
  {
    return m_taskExecutor;
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    m_topControl = new Composite( parent, SWT.FILL );
    m_topControl.setLayout( new FillLayout() );

    m_treeViewer = new TreeViewer( m_topControl, SWT.BORDER | SWT.SINGLE | SWT.FULL_SELECTION );

    // enable tooltips per cell
    ColumnViewerToolTipSupport.enableFor( m_treeViewer );

    // Content provider
    m_treeViewer.setContentProvider( new WorkflowContentProvider() );
    // Label provider
    m_treeViewer.setLabelProvider( new WorkflowLabelProvider( this ) );

    final ITaskExecutor taskExecutor = m_taskExecutor;
    m_treeViewer.getControl().addDisposeListener( new DisposeListener()
    {
      public void widgetDisposed( final DisposeEvent e )
      {
        taskExecutor.removeTaskExecutionListener( WorkflowControl.this );
      }
    } );

    // Listen to open events
    m_treeViewer.addOpenListener( new IOpenListener()
    {
      public void open( final OpenEvent event )
      {
        final ITreeSelection selection = (ITreeSelection) event.getSelection();
        final Object first = selection.getFirstElement();
        if( first != null )
        {
          if( first instanceof Task )
          {
            final Task task = (Task) first;
            doTask( task );
          }
        }
      }
    } );

    // listen to select events
    m_treeViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      public void selectionChanged( final SelectionChangedEvent event )
      {
        handleSelectionChanged( event );
      }
    } );
  }

  public Control getControl( )
  {
    return m_topControl;
  }

  final void doTask( final Task task )
  {
    final IStatus result = m_taskExecutor.execute( task );
    // TODO: error handling should be done by the task executor!; why isn't there a job?
    final Shell shell = m_treeViewer.getControl().getShell();
    final String title = org.kalypso.afgui.views.Messages.getString( "WorkflowControl.2" );//$NON-NLS-1$
    final String message = org.kalypso.afgui.views.Messages.getString( "WorkflowControl.3" ); //$NON-NLS-1$
    if( !result.isOK() )
      KalypsoAFGUIFrameworkPlugin.getDefault().getLog().log( result );
    ErrorDialog.openError( shell, title, message + task.getName(), result, IStatus.WARNING | IStatus.ERROR );
  }

  public void setWorkflow( final Workflow workflow )
  {
    if( m_treeViewer != null && !m_treeViewer.getControl().isDisposed() )
    {
      m_treeViewer.setInput( workflow );
      m_treeViewer.collapseAll();
      final Task activeTask = m_taskExecutor.getActiveTask();
      if( workflow != null && activeTask != null )
        setCurrentTask( workflow, activeTask );
    }
  }

  /**
   * @see de.renew.workflow.event.IWorklistChangeListener#worklistChanged()
   */
  public void worklistChanged( )
  {
    m_treeViewer.refresh();
  }

  public void setFocus( )
  {
    if( m_treeViewer != null && !m_treeViewer.getControl().isDisposed() )
      m_treeViewer.getControl().setFocus();
  }

  /**
   * @see de.renew.workflow.connector.worklist.ITaskExecutionListener#handleTaskExecuted(org.eclipse.core.runtime.IStatus,
   *      de.renew.workflow.base.Task)
   */
  public void handleTaskExecuted( final IStatus result, final Task task )
  {
    final TreeViewer treeViewer = m_treeViewer;
    if( treeViewer == null || treeViewer.getControl() == null || treeViewer.getControl().isDisposed() )
      return;

    new UIJob( "" )
    {
      @Override
      public IStatus runInUIThread( final IProgressMonitor monitor )
      {
        if( !treeViewer.getControl().isDisposed() )
        {
          final Workflow workflow = (Workflow) treeViewer.getInput();
          if( workflow != null && task != null )
            setCurrentTask( workflow, task );

          treeViewer.refresh();
        }
        return Status.OK_STATUS;
      }
    }.schedule();
  }

  /**
   * @see de.renew.workflow.connector.worklist.ITaskExecutionListener#handleTaskStopped(de.renew.workflow.base.Task)
   */
  public void handleTaskStopped( final Task task )
  {

  }

  protected void handleSelectionChanged( final SelectionChangedEvent event )
  {
    final ITreeSelection selection = (ITreeSelection) event.getSelection();
    final Object first = selection.getFirstElement();
    if( first != null && m_lastSelectedElement != first )
    {
      final TreePath newTreePath = selection.getPathsFor( first )[0];
      m_lastSelectedElement = null;
      if( m_lastTreePath != null )
      {
        final int segmentCount = m_lastTreePath.getSegmentCount();
        final int newSegmentCount = newTreePath.getSegmentCount();
        final Object segment = m_lastTreePath.getSegment( Math.min( segmentCount, newSegmentCount ) - 1 );
        final TreePath longerPath;
        final TreePath shorterPath;
        if( segmentCount > newSegmentCount )
        {
          longerPath = m_lastTreePath;
          shorterPath = newTreePath;
        }
        else
        {
          longerPath = newTreePath;
          shorterPath = m_lastTreePath;
        }
        if( !longerPath.startsWith( shorterPath, null ) )
        {
          m_treeViewer.collapseToLevel( segment, AbstractTreeViewer.ALL_LEVELS );
        }
        m_lastSelectedElement = first;
      }
      m_treeViewer.expandToLevel( first, 1 );
      m_lastTreePath = newTreePath;
      m_treeViewer.setSelection( new StructuredSelection( first ) );
    }
  }

  protected void setCurrentTask( final Workflow workflow, final Task task )
  {
    final TreePath findPart = TaskHelper.findPart( task.getURI(), workflow );
    if( findPart != null )
    {
      final TreeSelection newSelection = new TreeSelection( findPart );
      m_treeViewer.setSelection( newSelection, true );
    }
  }
}
