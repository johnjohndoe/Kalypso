package de.renew.workflow.connector;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.handlers.IHandlerService;
import org.eclipse.ui.part.ViewPart;

import de.renew.workflow.cases.Case;
import de.renew.workflow.connector.context.ActiveWorkContext;
import de.renew.workflow.connector.context.CaseHandlingProjectNature;
import de.renew.workflow.connector.context.IActiveContextChangeListener;
import de.renew.workflow.contexts.WorkflowContextHandlerFactory;

/**
 * @author Patrice Congo, Stefan Kurzbach
 */
public class WorklistView extends ViewPart
{
  final static public String ID = "de.renew.workflow.WorklistView";

  private WorkflowControl m_workflowControl;

  protected ActiveWorkContext<Case> m_activeWorkContext;

  private final IActiveContextChangeListener<Case> m_contextListener = new IActiveContextChangeListener<Case>()
  {
    /**
     * @see org.kalypso.kalypso1d2d.pjt.IActiveContextChangeListener#activeProjectChanged(org.eclipse.core.resources.IProject)
     */
    public void activeContextChanged( final CaseHandlingProjectNature<Case> newProject, final Case scenario )
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

  protected void handleContextChanged( final CaseHandlingProjectNature<Case> newProject, final Case scenario )
  {
    if( scenario != null )
    {
      setContentDescription( "Aktives Szenario: " + scenario.getName() + " (" + newProject.getProject().getName() + ")" );
      m_workflowControl.setWorkflow( m_activeWorkContext.getCaseManager().getCurrentWorkflow() );
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
    m_activeWorkContext = WorkflowConnectorPlugin.getDefault().getActiveWorkContext();
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
