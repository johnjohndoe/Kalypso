package org.kalypso.afgui.views;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.logging.Logger;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Platform;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.afgui.scenarios.Scenario;

import de.renew.workflow.connector.WorkflowProjectNature;
import de.renew.workflow.connector.cases.CaseHandlingProjectNature;
import de.renew.workflow.connector.context.ActiveWorkContext;
import de.renew.workflow.connector.context.IActiveContextChangeListener;

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
    PlatformUI.getWorkbench().getDisplay().asyncExec( new Runnable()
    {
      public void run( )
      {
        if( scenario != null && newProject != null )
        {
          String scenarioPathName;
          try
          {
            final URI uri = new URI( scenario.getURI() );
            scenarioPathName = uri.getPath();
          }
          catch( final URISyntaxException e )
          {
            scenarioPathName = "<Fehler>";
            e.printStackTrace();
          }
          final String projectName = newProject.getProject().getName();
          setContentDescription( "Aktives Szenario: " + projectName + scenarioPathName );
          try
          {
            final WorkflowProjectNature workflowNature = WorkflowProjectNature.toThisNature( newProject.getProject() );
            if( workflowNature != null )
            {
              m_workflowControl.setWorkflow( workflowNature.getCurrentWorklist() );
            }
            else
            {
              m_workflowControl.setWorkflow( null );
            }
          }
          catch( final CoreException e )
          {
            // TODO: do NOT eat exceptions!
            // TODO: better error handling
            // Consider using a UIJob instead, that thrown exceptions are shown as error dialog

            // project is not open or such
            m_workflowControl.setWorkflow( null );
          }
        }
        else
        {
          setContentDescription( "Kein Szenario aktiv." );
          m_workflowControl.setWorkflow( null );
        }
      }
    } );
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
