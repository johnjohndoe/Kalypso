package org.kalypso.kalypso1d2d.pjt;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Platform;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IPerspectiveDescriptor;
import org.eclipse.ui.IPerspectiveListener;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWindowListener;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.contexts.IContextService;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.afgui.db.IWorkflowDB;
import org.kalypso.afgui.model.IWorkflowSystem;
import org.kalypso.kalypso1d2d.pjt.actions.ProjectChangeListener;
import org.kalypso.kalypso1d2d.pjt.perspective.Perspective;
import org.kalypso.kalypso1d2d.pjt.views.ISzenarioDataProvider;
import org.kalypso.kalypso1d2d.pjt.views.SzenarioDataProvider;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.MapModellContextSwitcher;
import org.kalypso.scenarios.Scenario;
import org.kalypso.ui.editor.featureeditor.FeatureTemplateView;
import org.kalypso.ui.views.map.MapView;
import org.kalypso.workflow.Workflow;

//TODO move to workflow system problem with project??

/**
 * Represents the work context for a user. A workkontext is made of:
 * <ul>
 * <li/>The actuel project the user is working on <li/>The Workflow system <li/>The data basis system
 * </ul>
 * 
 * @author Patrice Congo
 */
public class ActiveWorkContext implements IWindowListener, IPartListener, IPerspectiveListener
{
  final static Logger logger = Logger.getLogger( ActiveWorkContext.class.getName() );

  private static final boolean log = Boolean.parseBoolean( Platform.getDebugOption( "org.kalypso.kalypso1d2d.pjt/debug" ) );

  static
  {
    if( !log )
      logger.setUseParentHandlers( false );
  }

  private static final String BASIS_SCENARIO = "http://www.tu-harburg.de/wb/kalypso/kb/workflow/test/Basis";

  private Scenario m_activeScenario;

  private IWorkflowDB workflowDB;

  private IWorkflowSystem workflowSystem;

  private final SzenarioDataProvider m_dataProvider = new SzenarioDataProvider();

  private IProject m_activeProject;

  private List<IActiveContextChangeListener> activeProjectChangeListener = new ArrayList<IActiveContextChangeListener>();

  /**
   * list of registries where we are registered as listeners <br>
   * used for clean dispose
   */
  private final List<Object> m_registries = new ArrayList<Object>();

  private final SzenarioSourceProvider m_simModelProvider = new SzenarioSourceProvider( this );

  private final ProjectChangeListener m_projectChangeListener = new ProjectChangeListener();

  private MapModellContextSwitcher m_contextSwitcher = new MapModellContextSwitcher();

  public ActiveWorkContext( )
  {
    final IWorkbench workbench = PlatformUI.getWorkbench();
    windowOpened( workbench.getActiveWorkbenchWindow() );
  }

  public void dispose( )
  {
    // remove all listeners
    for( Iterator iter = m_registries.iterator(); iter.hasNext(); )
    {
      final Object registry = iter.next();
      if( registry instanceof IWorkbench )
        ((IWorkbench) registry).removeWindowListener( this );
      if( registry instanceof IWorkbenchWindow )
        ((IWorkbenchWindow) registry).removePerspectiveListener( this );
      if( registry instanceof IWorkbenchPage )
        ((IWorkbenchPage) registry).removePartListener( this );
      if( registry instanceof IHandlerService )
        ((IHandlerService) registry).removeSourceProvider( m_simModelProvider );
      if( registry instanceof ActiveWorkContext )
        ((ActiveWorkContext) registry).removeActiveContextChangeListener( m_projectChangeListener );
    }
  }

  synchronized public void setActiveProject( final IProject activeProject )
  {
    if( this.m_activeProject == activeProject )
    {
      return;
    }
    logger.info( "New Project to Set:" + activeProject );
    final IWorkflowDB oldWorkflowDB = getWorkflowDB();
    try
    {
      if( oldWorkflowDB != null )
      {
        oldWorkflowDB.persist();
      }
      if( Kalypso1D2DProjectNature.isOfThisNature( activeProject ) )
      {
        final Kalypso1D2DProjectNature nature = Kalypso1D2DProjectNature.toThisNature( activeProject );
        this.m_activeProject = activeProject;
        this.workflowDB = nature.getWorkflowDB();
        this.workflowSystem = nature.getWorkflowSystem();
        logger.info( "WorkflowDB=" + workflowDB );
        logger.info( "WorkflowSystem:" + workflowSystem );
      }
      else
      {
        this.m_activeProject = null;
        this.workflowDB = null;
        logger.warning( "Project to set is not of 1d2d nature" );
      }
    }
    catch( final CoreException e )
    {
      logger.log( Level.SEVERE, "Error setting current project", e );
      return;
    }
    finally
    {
      fireActiveProjectChanged( activeProject, workflowDB.getScenario( BASIS_SCENARIO ) );
    }
  }

  synchronized public IProject getActiveProject( )
  {
    return m_activeProject;
  }

  synchronized public Scenario getActiveScenario( )
  {
    return m_activeScenario;
  }

  synchronized public IWorkflowDB getWorkflowDB( )
  {
    return workflowDB;
  }

  synchronized public IWorkflowSystem getWorkflowSystem( )
  {
    return workflowSystem;
  }

  public Workflow getCurrentWorkflow( )
  {
    if( m_activeProject == null )
    {
      return null;
    }
    if( workflowSystem == null )
    {
      return null;
    }
    else
    {
      return workflowSystem.getCurrentWorkFlow();
    }
  }

  synchronized public void addActiveContextChangeListener( IActiveContextChangeListener l )
  {
    logger.info( "Registering Active context change listener:" + l );
    if( l == null )
    {
      return;
    }
    else
    {
      if( activeProjectChangeListener.contains( l ) )
      {
        return;
      }
      else
      {
        activeProjectChangeListener.add( l );
      }
    }
  }

  synchronized public void removeActiveContextChangeListener( IActiveContextChangeListener l )
  {
    if( l == null )
    {
      return;
    }
    else
    {
      if( activeProjectChangeListener.contains( l ) )
      {
        activeProjectChangeListener.add( l );
      }
      else
      {
        // empty
      }
    }
  }

  synchronized public void removeAllActiveContextChangeListener( )
  {
    activeProjectChangeListener.clear();
  }

  final private void fireActiveProjectChanged( final IProject newProject, final Scenario scenario )
  {
    for( final IActiveContextChangeListener l : activeProjectChangeListener )
    {
      l.activeContextChanged( newProject, scenario );
    }
  }

  public ISzenarioDataProvider getSzenarioDataProvider( )
  {
    return m_dataProvider;
  }

  public void setCurrentSzenario( final Scenario scenario )
  {
    if( m_activeScenario == null ? scenario != null : m_activeScenario.getURI().equals( scenario == null ? null : scenario.getURI() ) )
    {
      final IProject activeProject = getActiveProject();
      m_activeScenario = scenario;
      m_dataProvider.setCurrent( activeProject, scenario );
      fireActiveProjectChanged( activeProject, scenario );
    }
  }

  // **** LISTENERS ****

  /**
   * @see org.eclipse.ui.IWindowListener#windowActivated(org.eclipse.ui.IWorkbenchWindow)
   */
  public void windowActivated( final IWorkbenchWindow window )
  {
    // nothing
  }

  /**
   * @see org.eclipse.ui.IWindowListener#windowDeactivated(org.eclipse.ui.IWorkbenchWindow)
   */
  public void windowDeactivated( final IWorkbenchWindow window )
  {
    // nothing
  }

  /**
   * @see org.eclipse.ui.IPageListener#pageActivated(org.eclipse.ui.IWorkbenchPage)
   */
  public void pageActivated( @SuppressWarnings("unused")
  final IWorkbenchPage page )
  {
    // nothing
  }

  /**
   * @see org.eclipse.ui.IWindowListener#windowOpened(org.eclipse.ui.IWorkbenchWindow)
   */
  public void windowOpened( final IWorkbenchWindow window )
  {
    window.addPerspectiveListener( this );
    m_registries.add( window );
    final IWorkbenchPage activePage = window.getActivePage();
    if( activePage != null )
    {
      perspectiveActivated( activePage, activePage.getPerspective() );
    }
  }

  /**
   * @see org.eclipse.ui.IWindowListener#windowClosed(org.eclipse.ui.IWorkbenchWindow)
   */
  public void windowClosed( final IWorkbenchWindow window )
  {
    window.removePerspectiveListener( this );
    m_registries.remove( window );
  }

  /**
   * @see org.eclipse.ui.IPerspectiveListener#perspectiveActivated(org.eclipse.ui.IWorkbenchPage,
   *      org.eclipse.ui.IPerspectiveDescriptor)
   */
  public void perspectiveActivated( final IWorkbenchPage page, final IPerspectiveDescriptor perspective )
  {
    final IHandlerService handlerService = (IHandlerService) page.getWorkbenchWindow().getService( IHandlerService.class );
    if( perspective.getId().equals( Perspective.ID ) )
    {
      handlerService.addSourceProvider( m_simModelProvider );
      addActiveContextChangeListener( m_projectChangeListener );
      page.addPartListener( this );
      m_registries.add( page );
      m_registries.add( handlerService );
      m_registries.add( this );
    }
    else
    {
      handlerService.removeSourceProvider( m_simModelProvider );
      removeActiveContextChangeListener( m_projectChangeListener );
      page.removePartListener( this );
      m_registries.remove( page );
      m_registries.remove( handlerService );
      m_registries.remove( this );
    }
  }

  /**
   * @see org.eclipse.ui.IPerspectiveListener#perspectiveChanged(org.eclipse.ui.IWorkbenchPage,
   *      org.eclipse.ui.IPerspectiveDescriptor, java.lang.String)
   */
  public void perspectiveChanged( final IWorkbenchPage page, final IPerspectiveDescriptor perspective, final String changeId )
  {
  }

  /**
   * @see org.eclipse.ui.IPartListener#partActivated(org.eclipse.ui.IWorkbenchPart)
   */
  public void partActivated( final IWorkbenchPart part )
  {
    if( part instanceof MapView || part instanceof FeatureTemplateView )
    {
      final IWorkbenchPage page = part.getSite().getPage();
      final IViewPart[] viewStack = page.getViewStack( (IViewPart) part );
      for( final IViewPart otherPart : viewStack )
      {
        if( otherPart == part )
        {
          continue;
        }
        else
        {
          page.hideView( otherPart );
        }
      }
    }
  }

  /**
   * @see org.eclipse.ui.IPartListener#partDeactivated(org.eclipse.ui.IWorkbenchPart)
   */
  public void partDeactivated( final IWorkbenchPart part )
  {
    // nothing
  }

  /**
   * @see org.eclipse.ui.IPartListener#partBroughtToTop(org.eclipse.ui.IWorkbenchPart)
   */
  public void partBroughtToTop( final IWorkbenchPart part )
  {
    // nothing
  }

  /**
   * @see org.eclipse.ui.IPartListener#partClosed(org.eclipse.ui.IWorkbenchPart)
   */
  public void partClosed( final IWorkbenchPart part )
  {
    if( part instanceof MapView )
    {
      final IContextService contextService = (IContextService) part.getSite().getService( IContextService.class );
      final MapPanel mapPanel = (MapPanel) part.getAdapter( MapPanel.class );
      mapPanel.removeModellListener( m_contextSwitcher );
      m_contextSwitcher.removeContextService( contextService );
    }
  }

  /**
   * @see org.eclipse.ui.IPartListener#partOpened(org.eclipse.ui.IWorkbenchPart)
   */
  public void partOpened( final IWorkbenchPart part )
  {
    if( part instanceof MapView )
    {
      final IContextService contextService = (IContextService) part.getSite().getService( IContextService.class );
      final MapPanel mapPanel = (MapPanel) part.getAdapter( MapPanel.class );
      mapPanel.addModellListener( m_contextSwitcher );
      m_contextSwitcher.addContextService( contextService );
    }
  }

}
