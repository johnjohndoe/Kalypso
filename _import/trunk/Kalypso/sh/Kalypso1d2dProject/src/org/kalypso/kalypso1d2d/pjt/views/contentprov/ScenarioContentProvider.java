package org.kalypso.kalypso1d2d.pjt.views.contentprov;

import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.kalypso.afgui.scenarios.IScenarioManager;
import org.kalypso.afgui.scenarios.IScenarioManagerListener;
import org.kalypso.afgui.scenarios.Scenario;
import org.kalypso.afgui.scenarios.ScenarioList;
import org.kalypso.kalypso1d2d.pjt.Kalypso1D2DProjectNature;

public class ScenarioContentProvider implements ITreeContentProvider
{
  /**
   * @author Stefan Kurzbach
   */
  private final class ScenarioChangeListener implements IScenarioManagerListener
  {
    private final Viewer m_viewer;

    ScenarioChangeListener( final Viewer viewer )
    {
      m_viewer = viewer;
    }

    public void scenariosChanged( )
    {
      if( m_viewer != null )
      {
        m_viewer.refresh();
      }
    }
  }

  private IScenarioManagerListener m_scenarioManagerListerner;

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

  public Object getParent( final Object element )
  {
    return null;
  }

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
            return !workflowData.getRootScenarios().isEmpty();
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

  public Object[] getElements( final Object inputElement )
  {
    return getChildren( inputElement );
  }

  public void dispose( )
  {
    // nothing to do yet
  }

  public void inputChanged( final Viewer viewer, final Object oldInput, final Object newInput )
  {
    if( oldInput != null && oldInput instanceof IProject )
    {
      final IProject project = (IProject) oldInput;
      if( !project.isOpen() )
      {
        // project is closed or does not exist
        return;
      }
      else
      {
        try
        {
          if( Kalypso1D2DProjectNature.isOfThisNature( project ) )
          {
            final Kalypso1D2DProjectNature nature = Kalypso1D2DProjectNature.toThisNature( project );
            final IScenarioManager oldManager = nature.getScenarioManager();
            oldManager.removeScenarioManagerListener( m_scenarioManagerListerner );
          }
        }
        catch( final CoreException e )
        {
          // cannot happen, all cases checked?
          e.printStackTrace();
        }
      }
    }

    if( newInput != null && newInput instanceof IProject )
    {
      final IProject project = (IProject) newInput;
      if( !project.isOpen() )
      {
        // project is closed or does not exist
        return;
      }
      else
      {
        try
        {
          if( Kalypso1D2DProjectNature.isOfThisNature( project ) )
          {
            final Kalypso1D2DProjectNature nature = Kalypso1D2DProjectNature.toThisNature( project );
            final IScenarioManager newManager = nature.getScenarioManager();
            if( m_scenarioManagerListerner == null )
            {
              m_scenarioManagerListerner = new ScenarioChangeListener( viewer );
            }
            newManager.addScenarioManagerListener( m_scenarioManagerListerner );
          }
        }
        catch( final CoreException e )
        {
          // cannot happen, all cases checked?
          e.printStackTrace();
        }
      }
    }
  }

}
