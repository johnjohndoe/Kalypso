package org.kalypso.kalypso1d2d.pjt.views.contentprov;

import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.kalypso.afgui.scenarios.IScenarioManager;
import org.kalypso.afgui.scenarios.Scenario;
import org.kalypso.afgui.scenarios.ScenarioList;
import org.kalypso.kalypso1d2d.pjt.Kalypso1D2DProjectNature;

public class ScenarioContentProvider implements ITreeContentProvider
{
  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getChildren(java.lang.Object)
   */
  public Object[] getChildren( final Object parentElement )
  {
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
            final IScenarioManager scenarioManager = nature.getScenarioManager();
            if( scenarioManager != null )
            {
              final List<Scenario> data = scenarioManager.getRootScenarios();
              return data.toArray();
            }
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
      Scenario workflowData = (Scenario) parentElement;
      final List<Scenario> list = workflowData.getDerivedScenarios().getScenarios();
      return list.toArray();
    }
    return new Object[0];
  }

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getParent(java.lang.Object)
   */
  public Object getParent( final Object element )
  {
    return null;
  }

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#hasChildren(java.lang.Object)
   */
  public boolean hasChildren( final Object element )
  {
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
            final IScenarioManager workflowData = nature.getScenarioManager();
            final List<Scenario> rootScenarios = workflowData.getRootScenarios();
            return rootScenarios != null && !rootScenarios.isEmpty();
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
    return false;
  }

  /**
   * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
   */
  public Object[] getElements( final Object inputElement )
  {
    return getChildren( inputElement );
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#dispose()
   */
  public void dispose( )
  {
    // nothing to do yet
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer, java.lang.Object,
   *      java.lang.Object)
   */
  public void inputChanged( final Viewer viewer, final Object oldInput, final Object newInput )
  {
  }

}
