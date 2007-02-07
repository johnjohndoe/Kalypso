package org.kalypso.kalypso1d2d.pjt.views;

import java.util.logging.Logger;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.ISelectionListener;
import org.eclipse.ui.ISelectionService;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.afgui.db.IWorkflowDB;
import org.kalypso.afgui.model.IWorkflow;
import org.kalypso.afgui.model.IWorkflowData;
import org.kalypso.afgui.model.IWorkflowSystem;
import org.kalypso.afgui.viz.WorkflowControl;
import org.kalypso.kalypso1d2d.pjt.ActiveWorkContext;
import org.kalypso.kalypso1d2d.pjt.IActiveContextChangeListener;

/**
 * @author Patrice Congo
 */
public class WorkflowView extends ViewPart
{

  final static public String ID = "org.kalypso.kalypso1d2d.pjt.views.WorkflowView";

  private static Logger LOGGER = Logger.getLogger( WorkflowView.class.getName() );

  static
  {
    final boolean log = Boolean.parseBoolean( Platform.getDebugOption( "org.kalypso.kalypso1d2d.pjt/debug" ) );
    if( log )
      LOGGER.setUseParentHandlers( false );
  }

  private WorkflowControl workflowControl;

  private ActiveWorkContext activeWorkContext = ActiveWorkContext.getInstance();

  private IPartListener partListener = new IPartListener()
  {

    public void partActivated( IWorkbenchPart part )
    {
      // TODO Auto-generated method stub

    }

    public void partBroughtToTop( IWorkbenchPart part )
    {

    }

    public void partClosed( IWorkbenchPart part )
    {
      if( part instanceof SimulationModelDBView )
      {
        workflowControl.setVisible( false );
        setCurrentSzenario( null, null );
      }
    }

    public void partDeactivated( IWorkbenchPart part )
    {

    }

    public void partOpened( IWorkbenchPart part )
    {

    }

  };

  private IActiveContextChangeListener workContextChangeListener = new IActiveContextChangeListener()
  {

    public void activeProjectChanged( IProject newProject, IProject oldProject, IWorkflowDB oldDB, IWorkflowSystem oldWorkflowSystem )
    {
      IWorkflow workflow = activeWorkContext.getCurrentWorkflow();
      LOGGER.info( "New Workflow:" + workflow );
      workflowControl.setWorkflow( workflow );
      workflowControl.setActiveProject( newProject );
      workflowControl.setVisible( false );
      setCurrentSzenario( null, null );

    }

  };

  private ISelectionListener workflowDataSelectionListener = new ISelectionListener()
  {

    public void selectionChanged( IWorkbenchPart part, ISelection selection )
    {
      if( part instanceof SimulationModelDBView )
      {
        if( selection.isEmpty() )
        {
          {
            workflowControl.setVisible( false );
            setCurrentSzenario( null, null );
          }
        }
        else if( selection instanceof IStructuredSelection )
        {
          Object first = ((IStructuredSelection) selection).getFirstElement();
          if( first instanceof IWorkflowData )
          {
            workflowControl.setVisible( true );

            final IWorkflowData data = (IWorkflowData) first;
            setCurrentSzenario( activeWorkContext
                .getActiveProject(), data );
          }
          else
          {
            workflowControl.setVisible( false );
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
  public void createPartControl( Composite parent )
  {
    // TODO change hard coding to test workflow
    // logger.warn("Using SH Test workflow");
    //		
    // workflowControl=
    // new WorkflowControl(TestRDFModel.getTesWorkflow());

    // TODO: remove all listeners when disposing this view

    activeWorkContext.addActiveContextChangeListener( workContextChangeListener );
    workflowControl = new WorkflowControl( activeWorkContext.getCurrentWorkflow() );
    workflowControl.createControl( parent );
    workflowControl.setVisible( false );
    setCurrentSzenario( null, null );
    ISelectionService selServ = getSite().getWorkbenchWindow().getSelectionService();
    selServ.addPostSelectionListener( workflowDataSelectionListener );
    getSite().getWorkbenchWindow().getPartService().addPartListener( partListener );

  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
   */
  @Override
  public void setFocus( )
  {

  }

  private void setCurrentSzenario( final IProject project, final IWorkflowData data )
  {
    activeWorkContext.setCurrentSzenario( project, data );
  }
}
