/**
 * 
 */
package org.kalypso.ui.wizards.imports.roughness;

import java.util.HashMap;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IWorkbench;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.RoughnessPolygonCollection;
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

  // workbench selection when the wizard was started
  protected IStructuredSelection selection;

  // flag indicated whether the wizard can be completed or not
  protected boolean wizardCompleted = false;

  public ImportWizard( )
  {
    super();
    m_data = new DataContainer();
    setNeedsProgressMonitor( true );
    m_operation = new TransformerShapeToIRoughnessCollection( m_data ); //$NON-NLS-1$
  }

  /**
   * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench,
   *      org.eclipse.jface.viewers.IStructuredSelection)
   */
  public void init( IWorkbench iWorkbench, IStructuredSelection iSelection )
  {
    setWindowTitle( "Shape import" );
  }

  /**
   * @see org.kalypso.ui.wizards.imports.INewWizardKalypsoImport#initModelProperties(java.util.HashMap)
   */
  public void initModelProperties( HashMap<String, Object> map )
  {
    RoughnessPolygonCollection feature = (RoughnessPolygonCollection) map.get( "IRoughnessPolygonCollection" );
//    if( feature.getRoughnessPolygons().size() > 0 )
//      throw new ExceptionInInitializerError("Roughness data allready loaded!");
//    else
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
    m_data.getRoughnessPolygonCollection().clear();
    m_data.getRoughnessShapeStaticRelationMap().clear();
    m_data.getRoughnessStaticCollectionMap().clear();
    return true;
  }

  @Override
  public boolean performFinish( )
  {
    m_pageMain.saveDataToModel();
    m_pageSecond.saveDataToModel();
    final IStatus status = RunnableContextHelper.execute( getContainer(), true, true, m_operation );
    ErrorDialog.openError( getShell(), getWindowTitle(), "", status );
    m_data.getRoughnessPolygonCollection().clear();
    m_data.getRoughnessShapeStaticRelationMap().clear();
    m_data.getRoughnessStaticCollectionMap().clear();
    
//    try
//    {
//      MapUtils mapUtils = new MapUtils();
//      mapUtils.createMap( true );
//    }
//    catch( IOException e1 )
//    {
//      e1.printStackTrace();
//    }
//    catch( JAXBException e1 )
//    {
//      e1.printStackTrace();
//    }
    
    try
    {
      ResourcesPlugin.getWorkspace().getRoot().getProject().refreshLocal( IResource.DEPTH_INFINITE, null );
    }
    catch( CoreException e )
    {
      e.printStackTrace();
    }
    
    return status.isOK();
  }

}
