package org.kalypso.afgui.views;

import java.util.logging.Logger;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.afgui.i18n.Messages;
import org.kalypso.afgui.scenarios.IScenario;
import org.kalypso.afgui.scenarios.ScenarioHelper;

import de.renew.workflow.base.IWorkflow;
import de.renew.workflow.connector.cases.CaseHandlingProjectNature;
import de.renew.workflow.connector.context.ActiveWorkContext;
import de.renew.workflow.connector.context.IActiveScenarioChangeListener;

/**
 * @author Patrice Congo, Stefan Kurzbach
 */
public class WorkflowView extends ViewPart
{
  final static public String ID = "org.kalypso.kalypso1d2d.pjt.views.WorklistView"; //$NON-NLS-1$

  static Logger LOGGER = Logger.getLogger( WorkflowView.class.getName() );

  static
  {
    final boolean log = Boolean.parseBoolean( Platform.getDebugOption( "org.kalypso.kalypso1d2d.pjt/debug" ) ); //$NON-NLS-1$
    if( !log )
    {
      LOGGER.setUseParentHandlers( false );
    }
  }

  private WorkflowControl m_workflowControl;

  protected ActiveWorkContext<IScenario> m_activeWorkContext;

  private final IActiveScenarioChangeListener<IScenario> m_contextListener = new IActiveScenarioChangeListener<IScenario>()
  {
    /**
     * @see org.kalypso.kalypso1d2d.pjt.IActiveContextChangeListener#activeProjectChanged(org.eclipse.core.resources.IProject)
     */
    public void activeScenarioChanged( final CaseHandlingProjectNature newProject, final IScenario scenario )
    {
      handleScenarioChanged( newProject, scenario );
    }
  };

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createPartControl( final Composite parent )
  {
    m_workflowControl.createControl( parent );
    handleScenarioChanged( m_activeWorkContext.getCurrentProject(), m_activeWorkContext.getCurrentCase() );
  }

  protected void handleScenarioChanged( final CaseHandlingProjectNature newProject, final IScenario scenario )
  {
    final String projectName = newProject == null ? null : newProject.getProject().getName();

    final String contentDescription;
    if( scenario == null || newProject == null )
    {
      contentDescription = Messages.getString("org.kalypso.afgui.views.WorkflowView.0"); //$NON-NLS-1$
    }
    else
    {
      contentDescription = Messages.getString("org.kalypso.afgui.views.WorkflowView.1") + projectName; //$NON-NLS-1$
    }

    final UIJob job = new UIJob( Messages.getString("org.kalypso.afgui.views.WorkflowView.2") ) //$NON-NLS-1$
    {
      @SuppressWarnings("synthetic-access")
      @Override
      public IStatus runInUIThread( final IProgressMonitor monitor )
      {
        setContentDescription( contentDescription );
        final IWorkflow workflow = ScenarioHelper.findWorkflow( scenario, newProject );
        m_workflowControl.setWorkflow( workflow );
        return Status.OK_STATUS;
      }
    };
    job.setUser( false );
    job.schedule();
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
    m_activeWorkContext = KalypsoAFGUIFrameworkPlugin.getDefault().getActiveWorkContext();
    m_activeWorkContext.addActiveContextChangeListener( m_contextListener );
    m_workflowControl = new WorkflowControl( KalypsoAFGUIFrameworkPlugin.getDefault().getTaskExecutor() );
  }

  /**
   * @see org.eclipse.ui.part.ViewPart#saveState(org.eclipse.ui.IMemento)
   */
  @Override
  public void saveState( final IMemento memento )
  {
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
   */
  @Override
  public void setFocus( )
  {
    if( m_workflowControl != null )
    {
      m_workflowControl.setFocus();
    }
  }
}
