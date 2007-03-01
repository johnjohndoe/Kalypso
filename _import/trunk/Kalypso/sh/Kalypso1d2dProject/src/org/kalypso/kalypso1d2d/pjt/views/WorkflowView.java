package org.kalypso.kalypso1d2d.pjt.views;

import java.util.logging.Logger;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.ISelectionListener;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.afgui.db.IWorkflowDB;
import org.kalypso.afgui.model.IWorkflow;
import org.kalypso.afgui.model.IWorkflowData;
import org.kalypso.afgui.model.IWorkflowSystem;
import org.kalypso.afgui.views.WorkflowControl2;
import org.kalypso.kalypso1d2d.pjt.ActiveWorkContext;
import org.kalypso.kalypso1d2d.pjt.IActiveContextChangeListener;
import org.kalypso.workflow.Workflow;

/**
 * @author Patrice Congo
 */
public class WorkflowView extends ViewPart
{
  final static public String ID = "org.kalypso.kalypso1d2d.pjt.views.WorkflowView";

  static Logger LOGGER = Logger.getLogger( WorkflowView.class.getName() );

  static
  {
    final boolean log = Boolean.parseBoolean( Platform.getDebugOption( "org.kalypso.kalypso1d2d.pjt/debug" ) );
    if( !log )
      LOGGER.setUseParentHandlers( false );
  }

  WorkflowControl2 m_workflowControl;

  final ActiveWorkContext m_activeWorkContext = ActiveWorkContext.getInstance();

  private final IActiveContextChangeListener workContextChangeListener = new IActiveContextChangeListener()
  {

    public void activeProjectChanged( IProject newProject, IProject oldProject, IWorkflowDB oldDB, IWorkflowSystem oldWorkflowSystem )
    {
      final Workflow workflow = m_activeWorkContext.getCurrentWorkflow();
      LOGGER.info( "New Workflow:" + workflow );
      m_workflowControl.setWorkflow( workflow );
    }

  };

  private final ISelectionListener workflowDataSelectionListener = new ISelectionListener()
  {

    public void selectionChanged( final IWorkbenchPart part, final ISelection selection )
    {
      if( part instanceof SimulationModelDBView )
      {
        if( selection.isEmpty() )
        {
          {
            setCurrentSzenario( null, null );
          }
        }
        else if( selection instanceof IStructuredSelection )
        {
          final Object first = ((IStructuredSelection) selection).getFirstElement();
          if( first instanceof IWorkflowData )
          {
            final IWorkflowData data = (IWorkflowData) first;
            setCurrentSzenario( m_activeWorkContext.getActiveProject(), data );
          }
          else
          {
            setCurrentSzenario( null, null );
          }
        }

      }
    }

  };

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createPartControl( final Composite parent )
  {
    m_workflowControl.createControl( parent );
    final IWorkbenchWindow workbenchWindow = getSite().getWorkbenchWindow();
    workbenchWindow.getSelectionService().addPostSelectionListener( workflowDataSelectionListener );
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#dispose()
   */
  @Override
  public void dispose( )
  {
    m_activeWorkContext.removeActiveContextChangeListener( workContextChangeListener );
    final IWorkbenchWindow workbenchWindow = getSite().getWorkbenchWindow();
    workbenchWindow.getSelectionService().removePostSelectionListener( workflowDataSelectionListener );
    super.dispose();
  }

  /**
   * @see org.eclipse.ui.part.ViewPart#init(org.eclipse.ui.IViewSite, org.eclipse.ui.IMemento)
   */
  @Override
  public void init( final IViewSite site, final IMemento memento ) throws PartInitException
  {
    super.init( site, memento );
    m_activeWorkContext.addActiveContextChangeListener( workContextChangeListener );
    m_workflowControl = new WorkflowControl2();
    // m_workflowControl = new WorkflowControl( m_activeWorkContext.getCurrentWorkflow() );
    m_workflowControl.restoreState( memento );
  }

  /**
   * @see org.eclipse.ui.part.ViewPart#saveState(org.eclipse.ui.IMemento)
   */
  @Override
  public void saveState( final IMemento memento )
  {
    m_workflowControl.saveState( memento );
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
   */
  @Override
  public void setFocus( )
  {

  }

  void setCurrentSzenario( final IProject project, final IWorkflowData data )
  {
    m_workflowControl.setVisible( project != null && data != null );
    m_activeWorkContext.setCurrentSzenario( project, data );
  }
}
