package org.kalypso.kalypso1d2d.pjt.views.contentprov;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.ui.model.WorkbenchContentProvider;
import org.kalypso.afgui.scenarios.Scenario;
import org.kalypso.afgui.scenarios.ScenarioList;
import org.kalypso.kalypso1d2d.pjt.Kalypso1D2DProjectNature;
import org.kalypso.kalypso1d2d.pjt.actions.ScenarioHelper;

import de.renew.workflow.connector.cases.ICaseManager;
import de.renew.workflow.connector.cases.ICaseManagerListener;

/**
 * @author Stefan Kurzbach
 * 
 */
public class ScenarioContentProvider extends WorkbenchContentProvider implements ICaseManagerListener<Scenario>
{
  private Viewer m_viewer;

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getChildren(java.lang.Object)
   */
  @Override
  public Object[] getChildren( final Object parentElement )
  {
    final Object[] children = super.getChildren( parentElement );
    if( parentElement instanceof IProject )
    {
      final IProject project = (IProject) parentElement;
      if( !project.isOpen() )
      {
        // project is closed or does not exist
        return null;
      }
      else
        try
        {
          if( Kalypso1D2DProjectNature.isOfThisNature( project ) )
          {
            // is of correct nature
            final Kalypso1D2DProjectNature nature = Kalypso1D2DProjectNature.toThisNature( project );
            final List<Object> resultList = new ArrayList<Object>( children.length + 3 );
            resultList.addAll( Arrays.asList( children ) );
            final ICaseManager<Scenario> caseManager = nature.getCaseManager();
            caseManager.addCaseManagerListener( this );
            resultList.addAll( caseManager.getCases() );
            return resultList.toArray();
          }
        }
        catch( final CoreException e )
        {
          // cannot happen, all cases checked?
          e.printStackTrace();
        }
    }
    else if( parentElement instanceof Scenario )
    {
      final Scenario scenario = (Scenario) parentElement;
      final ScenarioList derivedScenarios = scenario.getDerivedScenarios();
      if( derivedScenarios != null )
      {
        final List<Scenario> list = derivedScenarios.getScenarios();
        return list.toArray();
      }
    }
    return children;
  }

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#hasChildren(java.lang.Object)
   */
  @Override
  public boolean hasChildren( final Object element )
  {
    final boolean hasChildren = super.hasChildren( element );
    if( element != null && element instanceof IProject )
    {
      final IProject project = (IProject) element;
      if( !project.isOpen() )
      {
        // project is closed or does not exist
        return false;
      }
      else
      {
        try
        {
          if( Kalypso1D2DProjectNature.isOfThisNature( project ) )
          {
            final Kalypso1D2DProjectNature nature = Kalypso1D2DProjectNature.toThisNature( project );
            final ICaseManager<Scenario> workflowData = nature.getCaseManager();
            final List<Scenario> rootScenarios = workflowData.getCases();
            return hasChildren || (rootScenarios != null && !rootScenarios.isEmpty());
          }
        }
        catch( final CoreException e )
        {
          // cannot happen, all cases checked?
          e.printStackTrace();
        }
      }
    }
    else if( element instanceof Scenario )
    {
      Scenario workflowData = (Scenario) element;
      final ScenarioList derivedScenarios = workflowData.getDerivedScenarios();
      if( derivedScenarios != null )
        return !derivedScenarios.getScenarios().isEmpty();
    }
    return hasChildren;
  }

  /**
   * @see org.eclipse.ui.model.WorkbenchContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer, java.lang.Object,
   *      java.lang.Object)
   */
  @Override
  public void inputChanged( final Viewer viewer, final Object oldInput, final Object newInput )
  {
    m_viewer = viewer;
    super.inputChanged( viewer, oldInput, newInput );
  }

  /**
   * @see de.renew.workflow.connector.context.ICaseManagerListener#caseAdded(de.renew.workflow.cases.Case)
   */
  public void caseAdded( final Scenario caze )
  {
    refreshViewer( caze );
  }

  /**
   * @see de.renew.workflow.connector.context.ICaseManagerListener#caseRemoved(de.renew.workflow.cases.Case)
   */
  public void caseRemoved( final Scenario caze )
  {
    refreshViewer( caze );
  }

  private void refreshViewer( final Scenario caze )
  {
    if( m_viewer instanceof StructuredViewer )
    {
      final String projectName = ScenarioHelper.getProjectName( caze );
      final IProject project = ResourcesPlugin.getWorkspace().getRoot().getProject( projectName );      
      final StructuredViewer viewer = (StructuredViewer) m_viewer;
      final Scenario parentScenario = caze.getParentScenario();
      if( parentScenario != null )
      {
        viewer.refresh( parentScenario );        
      }
      else
      {
        if( project != null )
        {
          viewer.refresh( project );
        }
      }
      final IFolder folder = ScenarioHelper.getFolder( caze );
      viewer.refresh(folder.getParent());
    }
  }

}
