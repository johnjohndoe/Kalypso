/**
 *
 */
package org.kalypso.afgui.views;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.dialogs.ErrorDialog;
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
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IMemento;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;

import de.renew.workflow.base.Task;
import de.renew.workflow.base.Workflow;
import de.renew.workflow.connector.IWorklistChangeListener;
import de.renew.workflow.connector.cases.TaskExecutionException;
import de.renew.workflow.connector.worklist.ITaskExecutor;

/**
 * @author Stefan Kurzbach
 */
public class WorkflowControl implements IWorklistChangeListener
{
  private static final Logger logger = Logger.getLogger( WorkflowControl.class.getName() );

  private static final boolean log = Boolean.parseBoolean( Platform.getDebugOption( "org.kalypso.afgui/debug" ) ); //$NON-NLS-1$

  private static final String MEMENTO_LAST_SELECTION = "lastSelection"; //$NON-NLS-1$

  static
  {
    if( !log )
      logger.setUseParentHandlers( false );
  }

  protected TreeViewer m_treeViewer;

  protected TreePath m_lastTreePath;

  private String m_selectionFromMemento;

  private final ITaskExecutor m_taskExecutor;

  private Composite m_topControl;

  public WorkflowControl( final ITaskExecutor taskExecutor )
  {
    m_taskExecutor = taskExecutor;
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

    // Tree columns
    // final TreeColumn column1 = new TreeColumn( tree, SWT.NONE );
    // column1.setWidth( 500 );
    // final TreeColumn column2 = new TreeColumn( tree, SWT.NONE );
    // column2.setWidth( 18 );

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
      private Object m_lastSelectedElement;

      public void selectionChanged( final SelectionChangedEvent event )
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
              m_treeViewer.collapseToLevel( segment, TreeViewer.ALL_LEVELS );
            }
            m_lastSelectedElement = first;
          }
          m_treeViewer.expandToLevel( first, 1 );
          m_lastTreePath = newTreePath;
          m_treeViewer.setSelection( new StructuredSelection( first ) );
        }
      }
    } );
  }

  public Control getControl( )
  {
    return m_topControl;
  }

  final void doTask( final Task task )
  {
    try
    {
      m_taskExecutor.execute( task );
    }
    catch( final TaskExecutionException e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      ErrorDialog.openError( m_treeViewer.getControl().getShell(), org.kalypso.afgui.views.Messages.getString( "WorkflowControl.2" ), org.kalypso.afgui.views.Messages.getString( "WorkflowControl.3" ) //$NON-NLS-1$ //$NON-NLS-2$
          + task.getURI(), status, IStatus.WARNING | IStatus.ERROR );
      KalypsoAFGUIFrameworkPlugin.getDefault().getLog().log( status );
      logger.log( Level.SEVERE, org.kalypso.afgui.views.Messages.getString( "WorkflowControl.4" ) + task.getURI(), e ); //$NON-NLS-1$
    }
    finally
    {
      m_treeViewer.refresh();
    }
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
      memento.putString( MEMENTO_LAST_SELECTION, ((Task) lastSegment).getURI() );
    }
  }

  public void setWorkflow( final Workflow workflow )
  {
    if( m_treeViewer != null && !m_treeViewer.getControl().isDisposed() )
    {
      m_treeViewer.setInput( workflow );
      m_treeViewer.collapseAll();
      if( m_selectionFromMemento != null && workflow != null )
      {
        final TreePath findPart = TaskHelper.findPart( m_selectionFromMemento, workflow );
        if( findPart != null && findPart.getParentPath() != null && findPart.getParentPath().getSegmentCount() != 0 )
        {
          final TreeSelection newSelection = new TreeSelection( findPart.getParentPath() );
          m_treeViewer.setSelection( newSelection, true );
          // final Task task = (Task) findPart.getLastSegment();
          // final UIJob job = new UIJob( "Letzten Arbeitskontext wiederherstellen" )
          // {
          // @SuppressWarnings("unchecked")
          // @Override
          // public IStatus runInUIThread( final IProgressMonitor monitor )
          // {
          // try
          // {
          // m_taskExecutor.execute( task );
          // }
          // catch( final TaskExecutionException e )
          // {
          // final Display display = m_treeViewer.getControl().getDisplay();
          // final Shell activeShell = display.getActiveShell();
          // final IStatus status = StatusUtilities.statusFromThrowable( e );
          // ErrorDialog.openError( activeShell, "Warnung", "Die letzte Tätigkeit konnte nicht wiederhergestellt
          // werden", status
          // );
          // return Status.CANCEL_STATUS;
          // }
          // return Status.OK_STATUS;
          // }
          // };
          // job.schedule();
        }
        m_selectionFromMemento = null;
      }
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
}
