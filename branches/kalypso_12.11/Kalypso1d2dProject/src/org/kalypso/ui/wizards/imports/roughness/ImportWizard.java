/**
 *
 */
package org.kalypso.ui.wizards.imports.roughness;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.afgui.scenarios.ScenarioHelper;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCollection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygonCollection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.RoughnessPolygon;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeaturesChangedModellEvent;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

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
  protected IStructuredSelection m_selection;

  // flag indicated whether the wizard can be completed or not
  protected boolean wizardCompleted = false;

  public ImportWizard( )
  {
    super();
    m_data = new DataContainer();
    setNeedsProgressMonitor( true );
  }

  @Override
  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
    setWindowTitle( Messages.getString( "org.kalypso.ui.wizards.imports.roughness.PageMain.Title" ) );//$NON-NLS-1$
    m_selection = selection;

    final IScenarioDataProvider szenarioDataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();

    m_operation = new Transformer( m_data ); //$NON-NLS-1$

    ITerrainModel model;
    try
    {
      model = szenarioDataProvider.getModel( ITerrainModel.class.getName() );
      m_data.setModel( model );
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
    }
    m_szenarioFolder = ScenarioHelper.getScenarioFolder();
    m_project = m_szenarioFolder.getProject();
    m_data.setProjectBaseFolder( m_szenarioFolder.getFullPath().segment( 0 ) );
    m_data.loadUserSelection( "/.metadata/roughnessUserSelection.dat" ); //$NON-NLS-1$
    try
    {
      m_data.setRoughnessDatabaseLocation( RoughnessPolygon.DATA_LOCATION, (IRoughnessClsCollection)szenarioDataProvider.getModel( IRoughnessClsCollection.class.getName() ) ); //$NON-NLS-1$
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  @Override
  public void addPages( )
  {
    m_pageMain = new PageMain( m_data );
    addPage( m_pageMain );

    m_pageSecond = new PageSecond( m_data );
    addPage( m_pageSecond );
  }

  @Override
  public boolean canFinish( )
  {
    return m_pageSecond.isPageComplete();
  }

  @Override
  public boolean performCancel( )
  {
    ((Transformer)m_operation).unprepare();
    m_data.getRoughnessShapeStaticRelationMap().clear();
    m_data.getRoughnessStaticCollectionMap().clear();
    try
    {
      m_project.refreshLocal( IResource.DEPTH_INFINITE, null );
    }
    catch( final Exception e )
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
      ErrorDialog.openError( getShell(), getWindowTitle(), "", status ); //$NON-NLS-1$
      m_data.getRoughnessShapeStaticRelationMap().clear();
      m_data.getRoughnessStaticCollectionMap().clear();
      m_data.saveUserSelection();
      m_szenarioFolder.refreshLocal( IResource.DEPTH_INFINITE, null );

      final List<Feature> changedFeatures = new ArrayList<>();
      final List<IRoughnessPolygonCollection> roughnessPolygonCollections = m_data.getModel().getRoughnessPolygonCollections();
      for( final IRoughnessPolygonCollection collection : roughnessPolygonCollections )
      {
        final List<IRoughnessPolygon> polygons = collection.getRoughnessPolygons();
        for( final IRoughnessPolygon polygon : polygons )
        {
          changedFeatures.add( polygon );
        }
      }
      final GMLWorkspace workspace = m_data.getModel().getWorkspace();
      workspace.fireModellEvent( new FeaturesChangedModellEvent( workspace, changedFeatures.toArray( new Feature[changedFeatures.size()] ) ) );

    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    return status.isOK();
  }

}
