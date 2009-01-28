package org.kalypso.afgui.views;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.ui.model.WorkbenchContentProvider;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.afgui.ScenarioHandlingProjectNature;
import org.kalypso.afgui.scenarios.IScenario;
import org.kalypso.afgui.scenarios.IScenarioList;
import org.kalypso.afgui.scenarios.Scenario;
import org.kalypso.afgui.scenarios.ScenarioList;
import org.kalypso.contribs.eclipse.jface.viewers.ViewerUtilities;

import de.renew.workflow.connector.cases.CaseHandlingProjectNature;
import de.renew.workflow.connector.cases.ICaseManager;
import de.renew.workflow.connector.cases.ICaseManagerListener;
import de.renew.workflow.connector.context.ActiveWorkContext;
import de.renew.workflow.connector.context.IActiveScenarioChangeListener;

/**
 * @author Stefan Kurzbach
 */
public class ScenarioContentProvider extends WorkbenchContentProvider implements ICaseManagerListener<IScenario>, IActiveScenarioChangeListener<IScenario>
{
  private Viewer m_viewer;

  private final boolean m_showResources;

  public ScenarioContentProvider( )
  {
    this( true );
  }

  public ScenarioContentProvider( final boolean showResources )
  {
    m_showResources = showResources;

    final ActiveWorkContext<IScenario> activeWorkContext = KalypsoAFGUIFrameworkPlugin.getDefault().getActiveWorkContext();
    activeWorkContext.addActiveContextChangeListener( this );
  }

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getChildren(java.lang.Object)
   */
  @Override
  public Object[] getChildren( final Object parentElement )
  {
    final Object[] children = m_showResources ? super.getChildren( parentElement ) : new Object[] {};

    // TODO: does that really belong here??? should'nt it be more correct to add the resource content provider to the
    // scenario view in order to allow the displayment of resources?

    if( parentElement instanceof IProject )
    {
      final IProject project = (IProject) parentElement;
      if( !project.isOpen() )
      {
        // project is closed or does not exist
        return new Object[0];
      }
      else
      {
        try
        {
          final ScenarioHandlingProjectNature nature = ScenarioHandlingProjectNature.toThisNature( project );
          if( nature != null )
          {
            // is of correct nature
            final List<Object> resultList = new ArrayList<Object>( children.length + 3 );
            resultList.addAll( Arrays.asList( children ) );
            final ICaseManager<IScenario> caseManager = nature.getCaseManager();
            if( caseManager != null )
            {
              caseManager.addCaseManagerListener( this );
              resultList.addAll( caseManager.getCases() );
            }
            return resultList.toArray();
          }
        }
        catch( final CoreException e )
        {
          // cannot happen, all cases checked?
          e.printStackTrace();
        }
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
    if( hasChildren )
    {
      return true;
    }
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
          final ScenarioHandlingProjectNature nature = ScenarioHandlingProjectNature.toThisNature( project );
          if( nature != null )
          {
            final ICaseManager<IScenario> caseManager = nature.getCaseManager();
            if( caseManager != null )
            {
              final List<IScenario> rootScenarios = caseManager.getCases();
              return rootScenarios != null && !rootScenarios.isEmpty();
            }
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
      final Scenario workflowData = (Scenario) element;
      final ScenarioList derivedScenarios = workflowData.getDerivedScenarios();
      if( derivedScenarios != null )
      {
        return !derivedScenarios.getScenarios().isEmpty();
      }
    }
    return false;
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
  public void caseAdded( final IScenario caze )
  {
    refreshViewer( caze );
  }

  /**
   * @see de.renew.workflow.connector.context.ICaseManagerListener#caseRemoved(de.renew.workflow.cases.Case)
   */
  public void caseRemoved( final IScenario caze )
  {
    refreshViewer( caze );
  }

  /**
   * @see de.renew.workflow.connector.context.IActiveContextChangeListener#activeContextChanged(de.renew.workflow.connector.cases.CaseHandlingProjectNature,
   *      de.renew.workflow.cases.Case)
   */
  public void activeScenarioChanged( final CaseHandlingProjectNature newProject, final IScenario caze )
  {
    refreshViewer( null );
  }

  private void refreshViewer( final IScenario caze )
  {
    if( m_viewer instanceof StructuredViewer )
    {
      if( caze == null )
      {
        ViewerUtilities.refresh( m_viewer, true );
      }
      else
      {
        final IProject project = caze.getProject();
        final StructuredViewer viewer = (StructuredViewer) m_viewer;
        final IScenario parentScenario = caze.getParentScenario();
        if( parentScenario != null )
        {
          ViewerUtilities.refresh( viewer, parentScenario, true );
        }
        else
        {
          if( project != null )
          {
            ViewerUtilities.refresh( viewer, project, true );
          }
        }
        final IFolder folder = caze.getFolder();
        ViewerUtilities.refresh( viewer, folder.getParent(), true );
      }
    }
  }

  /**
   * @see org.eclipse.ui.model.WorkbenchContentProvider#dispose()
   */
  @Override
  public void dispose( )
  {
    final ActiveWorkContext<IScenario> activeWorkContext = KalypsoAFGUIFrameworkPlugin.getDefault().getActiveWorkContext();
    activeWorkContext.removeActiveContextChangeListener( this );
    super.dispose();
  }

  /**
   * @see org.eclipse.ui.model.BaseWorkbenchContentProvider#getElements(java.lang.Object)
   */
  @Override
  public Object[] getElements( final Object element )
  {
    if( element instanceof IResource )
    {
      return super.getElements( element );
    }

    if( !(element instanceof IScenario) )
    {
      return new Object[0];
    }

    final IScenario scenario = (IScenario) element;
    final IScenarioList derivedScenarios = scenario.getDerivedScenarios();
    if( derivedScenarios == null )
    {
      return new Object[0];
    }

    final List<IScenario> scenarios = derivedScenarios.getScenarios();
    if( scenarios == null || scenarios.size() == 0 )
    {
      return new Object[0];
    }

    return scenarios.toArray();
  }
}