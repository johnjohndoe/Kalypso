/**
 * 
 */
package org.kalypso.ui.wizards.imports.roughness;

import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCollection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygonCollection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelSimulationBaseConsts;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.mapmodel.IKalypsoThemePredicate;
import org.kalypso.ogc.gml.mapmodel.IKalypsoThemeVisitor;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.visitor.KalypsoThemeVisitor;
import org.kalypso.ui.views.map.MapView;
import org.kalypso.ui.wizards.imports.Messages;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;

import de.renew.workflow.connector.cases.ICaseDataProvider;
import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class ImportWizard extends Wizard implements INewWizard
{
  protected DataContainer m_data; // the data model

  protected PageMain m_pageMain;

  protected PageSecond m_pageSecond;

  protected ICoreRunnableWithProgress m_operation;

  private IProject m_project;

  private IFolder m_szenarioFolder;

  // workbench selection when the wizard was started
  protected IStructuredSelection selection;

  // flag indicated whether the wizard can be completed or not
  protected boolean wizardCompleted = false;

  public ImportWizard( )
  {
    super();
    m_data = new DataContainer();
    setNeedsProgressMonitor( true );
  }

  /**
   * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench,
   *      org.eclipse.jface.viewers.IStructuredSelection)
   */
  public void init( IWorkbench workbench, IStructuredSelection iSelection )
  {
    setWindowTitle( Messages.getString( "org.kalypso.ui.wizards.imports.roughness.PageMain.Title" ) );//$NON-NLS-1$
    selection = iSelection;
    final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
    final IEvaluationContext context = handlerService.getCurrentState();
    final ICaseDataProvider<IFeatureWrapper2> szenarioDataProvider = (ICaseDataProvider<IFeatureWrapper2>) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );
    final IWorkbenchWindow workbenchWindow = (IWorkbenchWindow) context.getVariable( ISources.ACTIVE_WORKBENCH_WINDOW_NAME );
    final MapView mapView = (MapView) workbenchWindow.getActivePage().findView( MapView.ID );
//    if( mapView == null )
//      throw new ExecutionException( "No map available, somethings wrong." );
    
    m_operation = new Transformer( m_data, mapView ); //$NON-NLS-1$
    
    ITerrainModel model;
    try
    {
      model = szenarioDataProvider.getModel( ITerrainModel.class );
      m_data.setModel(model);
    }
    catch( CoreException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    // RoughnessPolygonCollection feature = (RoughnessPolygonCollection) context.get( "IRoughnessPolygonCollection" );
    // if( feature.getRoughnessPolygons().size() > 0 )
    // throw new ExceptionInInitializerError("Roughness data allready loaded!");
    // else
    m_szenarioFolder = (IFolder) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_FOLDER_NAME );
    m_project = m_szenarioFolder.getProject();
    // m_project = (IProject) context.get( "Project" );
    // m_szenarioFolder = (IFolder) context.get( "SzenarioPath" );
    m_data.setProjectBaseFolder( m_szenarioFolder.getFullPath().segment( 0 ) );
    m_data.loadUserSelection( "/.metadata/roughnessUserSelection.dat" );
    try
    {
      m_data.setRoughnessDatabaseLocation( "/.metadata/roughness.gml", szenarioDataProvider.getModel( IRoughnessClsCollection.class ) );
    }
    catch( Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
  }

  @Override
  public void addPages( )
  {
    m_pageMain = new PageMain( m_data );
    addPage( m_pageMain );
    try
    {
      m_pageSecond = new PageSecond( m_data );
      addPage( m_pageSecond );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }

  @Override
  public boolean canFinish( )
  {
    return m_pageSecond.isPageComplete();
  }

  @Override
  public boolean performCancel( )
  {
    ((Transformer) m_operation).unprepare();

    // m_data.getRoughnessPolygonCollection().clear(); THIS MAKES PROBLEM IN GUI!
    m_data.getRoughnessShapeStaticRelationMap().clear();
    m_data.getRoughnessStaticCollectionMap().clear();
    try
    {
      m_project.refreshLocal( IResource.DEPTH_INFINITE, null );
      // ResourcesPlugin.getWorkspace().getRoot().getProject().refreshLocal( IResource.DEPTH_INFINITE, null );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
    return true;
  }

  @Override
  public boolean performFinish( )
  {
    IStatus status = null;
    try
    {
      m_pageMain.saveDataToModel();
      m_pageSecond.saveDataToModel();
      status = RunnableContextHelper.execute( getContainer(), true, true, m_operation );
      ErrorDialog.openError( getShell(), getWindowTitle(), "", status );
      m_data.getRoughnessShapeStaticRelationMap().clear();
      m_data.getRoughnessStaticCollectionMap().clear();
      m_data.saveUserSelection();
      m_szenarioFolder.refreshLocal( IResource.DEPTH_INFINITE, null );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }

    return status.isOK();
  }

  private IKalypsoTheme[] findRoughnessThemes( final IMapModell mapModell )
  {
    final IKalypsoThemePredicate predicate = new IKalypsoThemePredicate()
    {

      public boolean decide( final IKalypsoTheme theme )
      {
        return theme instanceof IKalypsoFeatureTheme && ((IKalypsoFeatureTheme) theme).getFeatureType().getQName().equals( KalypsoModelSimulationBaseConsts.SIM_BASE_F_ROUGHNESS_POLYGON );
      }
    };
    final KalypsoThemeVisitor visitor = new KalypsoThemeVisitor( predicate );
    mapModell.accept( visitor, IKalypsoThemeVisitor.DEPTH_INFINITE );
    return visitor.getFoundThemes();
  }


}
