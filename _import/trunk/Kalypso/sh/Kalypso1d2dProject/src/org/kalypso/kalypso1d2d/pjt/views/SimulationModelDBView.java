/**
 * 
 */
package org.kalypso.kalypso1d2d.pjt.views;

import java.util.logging.Logger;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITreeSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreePath;
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.afgui.scenarios.IScenarioManager;
import org.kalypso.afgui.scenarios.Scenario;
import org.kalypso.kalypso1d2d.pjt.ActiveWorkContext;
import org.kalypso.kalypso1d2d.pjt.IActiveContextChangeListener;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;
import org.kalypso.kalypso1d2d.pjt.views.contentprov.SimModelBasedContentProvider;
import org.kalypso.kalypso1d2d.pjt.views.contentprov.WorkflowDataLabelProvider;

/**
 * @author Patrice Congo, Stefan Kurzbach
 */
public class SimulationModelDBView extends ViewPart
{
  final static Logger logger = Logger.getLogger( SimulationModelDBView.class.getName() );

  private static final boolean log = Boolean.parseBoolean( Platform.getDebugOption( "org.kalypso.kalypso1d2d.pjt/debug" ) );

  static
  {
    if( !log )
      logger.setUseParentHandlers( false );
  }

  static public final String ID = "org.kalypso.kalypso1d2d.pjt.views.SimulationModelDBView";

  private static final String MEMENTO_SCENARIO = "scenario";

  private static final String MEMENTO_PROJECT = "project";

  protected TreeViewer m_tv;

  protected ActiveWorkContext m_activeWorkContext;

  private IActiveContextChangeListener m_contextChangeListener = new IActiveContextChangeListener()
  {
    private IProject m_oldProject;

    @SuppressWarnings("synthetic-access")
    public void activeContextChanged( final IProject newProject, final Scenario scenario )
    {
      Scenario scenarioToSet = scenario;
      if( m_oldProject != newProject )
      {
        m_oldProject = newProject;
        final IScenarioManager scenarioManager = m_activeWorkContext.getScenarioManager();

        if( !m_tv.getControl().isDisposed() )
          m_tv.setInput( scenarioManager );

        if( m_scenarioFromMemento != null )
        {
          scenarioToSet = scenarioManager.getScenario( m_scenarioFromMemento );
          m_scenarioFromMemento = null;
        }
      }

      if( scenarioToSet != null && !m_tv.getControl().isDisposed() )
      {
        final ITreeSelection selection = new TreeSelection( constructTreePath( scenarioToSet ) );
        if( !m_tv.getSelection().equals( selection ) )
        {
          m_tv.setSelection( selection, true );
          getSite().getPage().activate( SimulationModelDBView.this );
        }
      }
      // TODO: this is for debugging purposes, remove later? Looks good to me (stefan)
      if( newProject == null )
        setContentDescription( "<kein aktives Projekt>" );
      else
        setContentDescription( "Projekt: " + newProject.getName() );
    }

    private TreePath constructTreePath( final Scenario scenario )
    {
      if( scenario.getParentScenario() != null )
        return constructTreePath( scenario.getParentScenario() ).createChildPath( scenario );
      else
        return new TreePath( new Object[] { scenario } );
    }
  };

  private String m_scenarioFromMemento;

  private String m_projectFromMemento;

  /**
   * @see org.eclipse.ui.part.ViewPart#init(org.eclipse.ui.IViewSite, org.eclipse.ui.IMemento)
   */
  @Override
  public void init( final IViewSite site, final IMemento memento ) throws PartInitException
  {
    super.init( site, memento );
    m_activeWorkContext = Kalypso1d2dProjectPlugin.getActiveWorkContext();
    m_activeWorkContext.addActiveContextChangeListener( m_contextChangeListener );
    if( memento != null )
    {
      m_scenarioFromMemento = memento.getString( MEMENTO_SCENARIO );
      m_projectFromMemento = memento.getString( MEMENTO_PROJECT );
    }
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createPartControl( final Composite parent )
  {
    final Composite top = new Composite( parent, SWT.FILL );
    top.setLayout( new FillLayout() );

    m_tv = new TreeViewer( top, SWT.FILL );
    m_tv.setContentProvider( new SimModelBasedContentProvider() );
    getSite().setSelectionProvider( m_tv );
    m_tv.setLabelProvider( new WorkflowDataLabelProvider() );
    m_tv.addSelectionChangedListener( new ISelectionChangedListener()
    {
      /**
       * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
       */
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final ITreeSelection selection = (ITreeSelection) event.getSelection();
        if( !selection.isEmpty() )
        {
          final Scenario scenario = (Scenario) selection.getPaths()[0].getLastSegment();
          m_activeWorkContext.setCurrentSzenario( scenario );
        }
      }
    } );
    if( m_projectFromMemento != null )
    {
      final IPath projectPath = Path.fromPortableString( m_projectFromMemento );
      final IResource resource = ResourcesPlugin.getWorkspace().getRoot().findMember( projectPath );
      if( resource != null && resource.getType() == IResource.PROJECT )
      {
        m_activeWorkContext.setActiveProject( (IProject) resource );
      }
      m_projectFromMemento = null;
    }
    m_tv.setInput( m_activeWorkContext.getScenarioManager() );
  }

  /*
   * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
   */
  @Override
  public void setFocus( )
  {
    if( m_tv != null )
    {
      m_tv.getControl().setFocus();
    }
  }

  /**
   * @see org.eclipse.ui.part.ViewPart#saveState(org.eclipse.ui.IMemento)
   */
  @Override
  public void saveState( final IMemento memento )
  {
    final IStructuredSelection selection = (IStructuredSelection) m_tv.getSelection();
    final Object firstElement = selection.isEmpty() ? null : selection.getFirstElement();
    if( firstElement != null && firstElement instanceof Scenario )
    {
      final Scenario data = (Scenario) firstElement;
      memento.putString( MEMENTO_SCENARIO, data.getURI() );
    }
    final IProject activeProject = m_activeWorkContext.getCurrentProject();
    if( activeProject != null )
    {
      final String projectPath = activeProject.getName();
      memento.putString( MEMENTO_PROJECT, projectPath );
    }
  }
}
