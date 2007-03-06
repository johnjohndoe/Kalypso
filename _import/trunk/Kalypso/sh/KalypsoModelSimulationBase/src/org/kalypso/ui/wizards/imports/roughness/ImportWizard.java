/**
 * 
 */
package org.kalypso.ui.wizards.imports.roughness;

import java.io.File;
import java.io.FileWriter;
import java.util.HashMap;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IWorkbench;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.RoughnessPolygonCollection;
import org.kalypso.ogc.gml.GisTemplateHelper;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.template.gismapview.Gismapview.Layers;
import org.kalypso.template.types.StyledLayerType;
import org.kalypso.template.types.StyledLayerType.Style;
import org.kalypso.ui.wizards.imports.INewWizardKalypsoImport;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class ImportWizard extends Wizard implements INewWizardKalypsoImport
{
  protected DataContainer m_data; // the data model

  protected PageMain m_pageMain;

  protected PageSecond m_pageSecond;

  protected final ICoreRunnableWithProgress m_operation;

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
    m_operation = new Transformer( m_data ); //$NON-NLS-1$
  }

  /**
   * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench,
   *      org.eclipse.jface.viewers.IStructuredSelection)
   */
  public void init( IWorkbench iWorkbench, IStructuredSelection iSelection )
  {
    setWindowTitle( "Shape import" );
    selection = iSelection;
  }

  /**
   * @see org.kalypso.ui.wizards.imports.INewWizardKalypsoImport#initModelProperties(java.util.HashMap)
   */
  public void initModelProperties( HashMap<String, Object> map )
  {
    RoughnessPolygonCollection feature = (RoughnessPolygonCollection) map.get( "IRoughnessPolygonCollection" );
    // if( feature.getRoughnessPolygons().size() > 0 )
    // throw new ExceptionInInitializerError("Roughness data allready loaded!");
    // else
    m_project = (IProject) map.get( "Project" );
    m_szenarioFolder = (IFolder) map.get( "SzenarioPath" );   
    m_data.setRoughnessPolygonCollection( feature );
    m_data.setRoughnessDatabaseLocation( (String) map.get( "RoughnessDatabaseLocation" ) );
    m_data.setProjectBaseFolder( (String) map.get( "ProjectBaseFolder" ) );
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
    // m_data.getRoughnessPolygonCollection().clear(); THIS MAKES PROBLEM IN GUI!
    m_data.getRoughnessShapeStaticRelationMap().clear();
    m_data.getRoughnessStaticCollectionMap().clear();
    try
    {
      IResource resource = (IResource) selection.getFirstElement();
      resource.getProject().refreshLocal( IResource.DEPTH_INFINITE, null );
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
      m_project.refreshLocal( IResource.DEPTH_INFINITE, null );
      ErrorDialog.openError( getShell(), getWindowTitle(), "", status );
      m_data.getRoughnessPolygonCollection().clear();
      m_data.getRoughnessShapeStaticRelationMap().clear();
      m_data.getRoughnessStaticCollectionMap().clear();
      // m_project.refreshLocal( IResource.DEPTH_INFINITE, null );
      IFile ifile = m_szenarioFolder.getFile( "maps/roughness.gmt" );
      Gismapview gismapview = GisTemplateHelper.loadGisMapView( ifile );
      Layers layers = gismapview.getLayers();
      StyledLayerType layer = new StyledLayerType();
      final Style style = new Style();

      layer.setName( "Roughness" ); //$NON-NLS-1$
      layer.setVisible( true );
      layer.setFeaturePath( "#fid#RoughnessLayerPolygonCollection11709431308431/roughnessLayerMember[RoughnessPolygon]" ); //$NON-NLS-1$
      layer.setHref( "project:/szenario/models/terrain.gml" ); //$NON-NLS-1$ //$NON-NLS-2$
      layer.setType( "simple" ); //$NON-NLS-1$
      layer.setLinktype( "gml" ); //$NON-NLS-1$
      layer.setActuate( "onRequest" ); //$NON-NLS-1$
      layer.setId( "ID_" + (layers.getLayer().size() + 2) ); //$NON-NLS-1$

      style.setLinktype( "sld" ); //$NON-NLS-1$
      style.setStyle( "Roughness style" ); //$NON-NLS-1$
      style.setActuate( "onRequest" ); //$NON-NLS-1$
      style.setHref( "project:/.metadata/roughness.sld" ); //$NON-NLS-1$
      style.setType( "simple" ); //$NON-NLS-1$
      layer.getStyle().add( style );
      layers.getLayer().add( 0, layer );
      gismapview.setLayers( layers );
      GisTemplateHelper.saveGisMapView( gismapview, new FileWriter( new File( ifile.getLocationURI() ) ), "UTF-8" );
      m_project.refreshLocal( IResource.DEPTH_INFINITE, null );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }

    return status.isOK();
  }

}
