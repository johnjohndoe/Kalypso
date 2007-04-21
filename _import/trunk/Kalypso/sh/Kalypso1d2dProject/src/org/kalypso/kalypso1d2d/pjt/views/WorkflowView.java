package org.kalypso.kalypso1d2d.pjt.views;

import java.util.logging.Logger;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.Platform;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.afgui.scenarios.Scenario;
import org.kalypso.afgui.views.WorkflowControl;
import org.kalypso.kalypso1d2d.pjt.ActiveWorkContext;
import org.kalypso.kalypso1d2d.pjt.IActiveContextChangeListener;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;

import de.renew.workflow.connector.TaskExecutionListener;
import de.renew.workflow.contexts.WorkflowContextHandlerFactory;

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

  private WorkflowControl m_workflowControl;

  protected ActiveWorkContext m_activeWorkContext;

  private final IActiveContextChangeListener m_contextListener = new IActiveContextChangeListener()
  {
    /**
     * @see org.kalypso.kalypso1d2d.pjt.IActiveContextChangeListener#activeProjectChanged(org.eclipse.core.resources.IProject)
     */
    public void activeContextChanged( final IProject newProject, final Scenario scenario )
    {
      handleContextChanged( scenario );
    }
  };

  private TaskExecutionListener m_taskExecutionListener;

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createPartControl( final Composite parent )
  {
    m_workflowControl.createControl( parent );
    m_activeWorkContext = Kalypso1d2dProjectPlugin.getDefault().getActiveWorkContext();
    m_activeWorkContext.addActiveContextChangeListener( m_contextListener );
    handleContextChanged(m_activeWorkContext.getCurrentScenario());    
  }

  private void handleContextChanged( final Scenario scenario )
  {
    if( scenario != null )
    {
      setContentDescription( "Aktives Szenario: " + scenario.getName() );
    }
    else
    {
      setContentDescription( "Kein Szenario aktiv." );
    }
    m_workflowControl.setWorkflow( m_activeWorkContext.getCurrentWorkflow() );
    if( m_taskExecutionListener != null )
    {
      m_workflowControl.unsetTaskExecutionListener( m_taskExecutionListener );
    }
    m_taskExecutionListener = new TaskExecutionListener( new WorkflowContextHandlerFactory(), m_activeWorkContext, m_activeWorkContext.getDataProvider() );
    m_workflowControl.setTaskExecutionListener( m_taskExecutionListener );
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
    m_workflowControl = new WorkflowControl();
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
}
