/**
 * 
 */
package org.kalypso.kalypso1d2d.pjt.views;

import java.util.logging.Logger;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
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
import org.kalypso.afgui.db.EWorkflowProperty;
import org.kalypso.afgui.db.IWorkflowDB;
import org.kalypso.afgui.db.IWorkflowDBChangeListerner;
import org.kalypso.afgui.model.IWorkflowData;
import org.kalypso.afgui.model.IWorkflowSystem;
import org.kalypso.kalypso1d2d.pjt.ActiveWorkContext;
import org.kalypso.kalypso1d2d.pjt.IActiveContextChangeListener;
import org.kalypso.kalypso1d2d.pjt.views.contentprov.SimModelBasedContentProvider;
import org.kalypso.kalypso1d2d.pjt.views.contentprov.WorkflowDataLabelProvider;

/**
 * @author Patrice Congo
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

  TreeViewer tv;

  private SimModelBasedContentProvider simModelBasedCP;

  ActiveWorkContext activeWorkContext = ActiveWorkContext.getInstance();

  private WorkflowDataLabelProvider labelProvider = new WorkflowDataLabelProvider();

  private IActiveContextChangeListener activeProjectChangeListener = new IActiveContextChangeListener()
  {

    @SuppressWarnings("synthetic-access")
    public void activeProjectChanged( IProject newProject, IProject olProject, IWorkflowDB oldWorkflowDB, IWorkflowSystem oldWorkflowSystem )
    {
      // oldWorkflowDB.removeWorkflowDBChangeListener(dbChangeListerner);
      if( oldWorkflowDB != null )
      {
        oldWorkflowDB.removeWorkflowDBChangeListener( dbChangeListerner );
      }
      IWorkflowDB newDB = activeWorkContext.getWorkflowDB();
      if( newDB != null )
      {
        newDB.addWorkflowDBChangeListener( dbChangeListerner );
        // top.setVisible(true);
      }
      else
      {
        logger.warning( "New Project DB is nul" );
        // top.setVisible(false);
      }
      tv.setInput( activeWorkContext );
      if( m_scenarioFromMemento != null )
      {
        final IWorkflowData scenario = activeWorkContext.getWorkflowDB().getWorkflowDataById( m_scenarioFromMemento );
        final IStructuredSelection selection = new StructuredSelection( scenario );
        tv.setSelection( selection, true );
        activeWorkContext.selectScenario( m_scenarioFromMemento );
        m_scenarioFromMemento = null;
      }
      // TODO: this is for debugging purposes, remove later? Looks good to me (stefan)
      setContentDescription( newProject == null ? "" : newProject.getName() );
    }

  };

  private Composite top;

  IWorkflowDBChangeListerner dbChangeListerner = new IWorkflowDBChangeListerner()
  {
    public void workflowDBChanged( )
    {
      tv.setInput( activeWorkContext );
      logger.info( "DB changed " );
    }
  };

  private String m_scenarioFromMemento;

  /**
   * @see org.eclipse.ui.part.ViewPart#init(org.eclipse.ui.IViewSite, org.eclipse.ui.IMemento)
   */
  @Override
  public void init( final IViewSite site, final IMemento memento ) throws PartInitException
  {
    super.init( site, memento );

    if( memento != null )
    {
      final String scenarioFromMemento = memento.getString( MEMENTO_SCENARIO );
      if( scenarioFromMemento != null )
      {
        m_scenarioFromMemento = scenarioFromMemento;
      }
    }
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createPartControl( Composite parent )
  {
    top = new Composite( parent, SWT.FILL );
    top.setLayout( new FillLayout() );

    tv = new TreeViewer( top, SWT.FILL );
    simModelBasedCP = new SimModelBasedContentProvider();
    tv.setContentProvider( simModelBasedCP );
    tv.setInput( activeWorkContext );
    activeWorkContext.addActiveContextChangeListener( activeProjectChangeListener );
    getSite().setSelectionProvider( tv );
    tv.setLabelProvider( labelProvider );
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
   */
  @Override
  public void setFocus( )
  {

  }

  /**
   * @see org.eclipse.ui.part.ViewPart#saveState(org.eclipse.ui.IMemento)
   */
  @Override
  public void saveState( final IMemento memento )
  {
    final IStructuredSelection selection = (IStructuredSelection) tv.getSelection();
    final Object firstElement = selection.isEmpty() ? null : selection.getFirstElement();
    if( firstElement != null && firstElement instanceof IWorkflowData )
    {
      final IWorkflowData data = (IWorkflowData) firstElement;
      memento.putString( MEMENTO_SCENARIO, data.getURI() );
    }
  }

  public void updateTreeView( @SuppressWarnings("unused")
  IWorkflowData selected )
  {
    tv.refresh();
    // IStructuredSelection selection= new StructuredSelection(selected);
  }
}
