/**
 * 
 */
package org.kalypso.risk.plugin;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.kalypso.afgui.application.SimulationProjectsContentProvider;

import de.renew.workflow.base.Workflow;
import de.renew.workflow.connector.WorkflowProjectNature;

/**
 * Content provider for intro page. <br>
 * Shows all risk-projects in a list.
 * 
 * @author Gernot Belger
 */
public class KalypsoModelRiskProjectsContentProvider extends SimulationProjectsContentProvider
{
  /**
   * @see org.kalypso.afgui.application.SimulationProjectsContentProvider#appliesToProject(org.eclipse.core.resources.IProject)
   */
  @Override
  protected boolean appliesToProject( final IProject project ) throws CoreException
  {
    final WorkflowProjectNature nature = WorkflowProjectNature.toThisNature( project );
    if( nature == null )
      return false;

    final Workflow workflow = nature.getCurrentWorklist();
    final String uri = workflow.getURI();

    return uri.contains( "http___www.tu-harburg.de_wb_kalypso_risk__WF_KalypsoRisk" ); //$NON-NLS-1$
  }

}
