/**
 * 
 */
package org.kalypso.kalypso1d2d.pjt.views.contentprov;

import java.util.List;
import java.util.logging.Logger;

import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.kalypso.afgui.scenarios.IScenarioManager;
import org.kalypso.afgui.scenarios.IScenarioManagerListener;
import org.kalypso.scenarios.Scenario;
import org.kalypso.scenarios.ScenarioList;

/**
 * Content provider for the simulation model based data view
 * 
 * @author Patrice Congo, Stefan Kurzbach
 */
public class SimModelBasedContentProvider implements ITreeContentProvider
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

  private static final Logger logger = Logger.getLogger( SimModelBasedContentProvider.class.getName() );

  private static final boolean log = Boolean.parseBoolean( Platform.getDebugOption( "org.kalypso.kalypso1d2d.pjt/debug" ) );

  private IScenarioManagerListener m_dbChangeListerner;

  static
  {
    if( !log )
      logger.setUseParentHandlers( false );
  }

  public Object[] getChildren( final Object parentElement )
  {
    if( parentElement instanceof IScenarioManager )
    {
      final IScenarioManager workflowDB = (IScenarioManager) parentElement;
      if( workflowDB != null )
      {
        final List<Scenario> data = workflowDB.getRootScenarios();
        return data.toArray();
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
    if( element instanceof IScenarioManager )
    {
      final IScenarioManager workflowData = (IScenarioManager) element;
      return !workflowData.getRootScenarios().isEmpty();
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
  public Object[] getElements( final Object parentElement )
  {
    return getChildren( parentElement );
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#dispose()
   */
  public void dispose( )
  {

  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer, java.lang.Object,
   *      java.lang.Object)
   */
  public void inputChanged( final Viewer viewer, final Object oldInput, final Object newInput )
  {
    final IScenarioManager oldWorkflowDB = (IScenarioManager) oldInput;
    final IScenarioManager newDB = (IScenarioManager) newInput;
    if( m_dbChangeListerner == null )
    {
      m_dbChangeListerner = new ScenarioChangeListener( viewer );
    }
    if( oldWorkflowDB != null )
    {
      oldWorkflowDB.removeScenarioManagerListener( m_dbChangeListerner );
    }
    if( newDB != null )
    {
      newDB.addScenarioManagerListener( m_dbChangeListerner );
    }
    logger.info( "DB changed" );
  }
}