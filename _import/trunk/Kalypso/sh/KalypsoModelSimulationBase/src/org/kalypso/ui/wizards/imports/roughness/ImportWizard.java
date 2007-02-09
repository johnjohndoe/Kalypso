/**
 * 
 */
package org.kalypso.ui.wizards.imports.roughness;

import java.lang.reflect.InvocationTargetException;
import java.net.MalformedURLException;
import java.net.URL;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelSimulationBaseConsts;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class ImportWizard extends Wizard implements INewWizard
{
  protected DataContainer m_data; // the data model

  protected PageMain m_pageMain;
  
  protected PageSecond m_pageSecond;

  protected final ICoreRunnableWithProgress m_operation;

  // workbench selection when the wizard was started
  protected IStructuredSelection selection;

  // flag indicated whether the wizard can be completed or not
  protected boolean wizardCompleted = false;

  public ImportWizard()
  {
    super();
    m_data = new DataContainer();
    setNeedsProgressMonitor( true );
    final QName m_RootFeatureQName = KalypsoModelSimulationBaseConsts.SIM_BASE_F_ROUGHNESS_POLYGON_COLLECTION;
    URL outputFileURL;
    GMLWorkspace workspace;
    try
    {
      outputFileURL = new URL( "file:D:/Eclipse/TESTS_RESULTS/rauheitstest.gml" );
      workspace = FeatureFactory.createGMLWorkspace( m_RootFeatureQName, outputFileURL, GmlSerializer.DEFAULT_FACTORY );
      m_data.setWorkspace( workspace );
    }
    catch( MalformedURLException e )
    {
      e.printStackTrace();
    }
    catch( InvocationTargetException e )
    {
      e.printStackTrace();
    }
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

  public void setWorkspace( GMLWorkspace workspace )
  {
    m_data.setWorkspace( workspace );
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
  public boolean performFinish( )
  {
    m_pageMain.saveDataToModel();
    m_pageSecond.saveDataToModel();
    final IStatus status = RunnableContextHelper.execute( getContainer(), true, true, m_operation );
    ErrorDialog.openError( getShell(), getWindowTitle(), "", status );
    return status.isOK();
  }

}
