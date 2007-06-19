package org.kalypso.kalypso1d2d.pjt.views;

import java.util.logging.Logger;

import org.eclipse.core.runtime.Platform;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.handlers.IHandlerService;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.afgui.scenarios.Scenario;
import org.kalypso.afgui.views.WorkflowControl;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;

import de.renew.workflow.connector.context.ActiveWorkContext;
import de.renew.workflow.connector.context.CaseHandlingProjectNature;
import de.renew.workflow.connector.context.IActiveContextChangeListener;
import de.renew.workflow.connector.worklist.ITaskExecutor;
import de.renew.workflow.connector.worklist.TaskExecutionListener;
import de.renew.workflow.contexts.WorkflowContextHandlerFactory;

/**
 * @author Patrice Congo, Stefan Kurzbach
 */
public class WorkflowView extends ViewPart
{
  final static public String ID = "org.kalypso.kalypso1d2d.pjt.views.WorklistView";

  static Logger LOGGER = Logger.getLogger( WorkflowView.class.getName() );

  static
  {
    final boolean log = Boolean.parseBoolean( Platform.getDebugOption( "org.kalypso.kalypso1d2d.pjt/debug" ) );
    if( !log )
      LOGGER.setUseParentHandlers( false );
  }

  private WorkflowControl m_workflowControl;

  protected ActiveWorkContext<Scenario> m_activeWorkContext;

  private final IActiveContextChangeListener<Scenario> m_contextListener = new IActiveContextChangeListener<Scenario>()
  {
    /**
     * @see org.kalypso.kalypso1d2d.pjt.IActiveContextChangeListener#activeProjectChanged(org.eclipse.core.resources.IProject)
     */
    public void activeContextChanged( final CaseHandlingProjectNature newProject, final Scenario scenario )
    {
      handleContextChanged( newProject, scenario );
    }
  };

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createPartControl( final Composite parent )
  {
    m_workflowControl.createControl( parent );
    handleContextChanged( m_activeWorkContext.getCurrentProject(), m_activeWorkContext.getCurrentCase() );
  }

  protected void handleContextChanged( final CaseHandlingProjectNature newProject, final Scenario scenario )
  {
    if( scenario != null )
    {
      setContentDescription( "Aktives Szenario: " + scenario.getName() + " (" + newProject.getProject().getName() + ")" );
      m_workflowControl.setWorkflow( m_activeWorkContext.getCurrentWorklist() );
    }
    else
    {
      setContentDescription( "Kein Szenario aktiv." );
      m_workflowControl.setWorkflow( null );
    }
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#dispose()
   */
  @Override
  public void dispose( )
  {
    m_activeWorkContext.removeActiveContextChangeListener( m_contextListener );
    super.dispose();
  }

  /**
   * @see org.eclipse.ui.part.ViewPart#init(org.eclipse.ui.IViewSite, org.eclipse.ui.IMemento)
   */
  @Override
  public void init( final IViewSite site, final IMemento memento ) throws PartInitException
  {
    super.init( site, memento );
    m_activeWorkContext = Kalypso1d2dProjectPlugin.getDefault().getActiveWorkContext();
    m_activeWorkContext.addActiveContextChangeListener( m_contextListener );
    final WorkflowContextHandlerFactory workflowContextHandlerFactory = new WorkflowContextHandlerFactory();
    final IWorkbench workbench = site.getWorkbenchWindow().getWorkbench();
    final ICommandService commandService = (ICommandService) workbench.getService( ICommandService.class );
    final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
    new TaskExecutionListener( commandService );
    final ITaskExecutor taskExecutor = new TaskExecutor( workflowContextHandlerFactory, new TaskExecutionAuthority(), commandService, handlerService );
    m_workflowControl = new WorkflowControl( taskExecutor );
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
    if( m_workflowControl != null )
      m_workflowControl.setFocus();
  }
}
