package org.kalypso.ui.wizard.calccase;

import java.io.Writer;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.GMLWorkspace;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.eclipse.core.resources.IProjectProvider;
import org.kalypso.eclipse.util.SetContentThread;
import org.kalypso.java.lang.CatchRunnable;
import org.kalypso.ogc.gml.command.ChangeFeaturesCommand;
import org.kalypso.ogc.gml.featureview.FeatureChange;
import org.kalypso.ogc.gml.featureview.FeatureComposite;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.nature.ModelNature;

/**
 * Wizard-Page zur Eingabe der Steuerparameter
 * 
 * @author belger
 */
public class SteuerparameterWizardPage extends WizardPage
{
  private final IProjectProvider m_projectProvider;

  private FeatureComposite m_featureComposite;

  private boolean m_overrideCanFlipToNextPage;

  public SteuerparameterWizardPage( final IProjectProvider pp, final boolean overrideCanFlipToNextPage )
  {
    super( "EditCalcCaseControlPage", "Steurparameter", null );

    m_projectProvider = pp;
    m_overrideCanFlipToNextPage = overrideCanFlipToNextPage;
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    try
    {
      final IProject project = m_projectProvider.getProject();
      final ModelNature nature = (ModelNature)m_projectProvider.getProject().getNature(
          ModelNature.ID );
      final GMLWorkspace workspace = nature.loadDefaultControl();
      
      // Vorlage auslesen
      final URL viewURL = new URL( "platform:/resource/" + project.getName() + "/"
          + ModelNature.CONTROL_VIEW_PATH );
      m_featureComposite = new FeatureComposite( workspace.getRootFeature(), new URL[]
      { viewURL } );
      setControl( m_featureComposite.createControl( parent, SWT.NONE ) );
    }
    catch( final MalformedURLException e )
    {
      // ERROR handling
      e.printStackTrace();
    }
    catch( final CoreException e )
    {
      // ERROR handling
      e.printStackTrace();
    }
  }

  public void setCalcCase( final IFolder folder ) throws CoreException
  {
    // gibts schon nen GML?
    final ModelNature nature = (ModelNature)m_projectProvider.getProject().getNature(
        ModelNature.ID );
    final GMLWorkspace gml = nature.loadOrCreateControl( folder );
    m_featureComposite.setFeature( gml.getRootFeature() );
    m_featureComposite.updateControl();
  }

  public void saveChanges( final IFolder folder, final IProgressMonitor monitor ) throws CoreException
  {
    monitor.beginTask( "Steuerparameter speichern" ,  2000 );
    
    // COMMITTEN
    final Collection changes = new ArrayList();

    final FeatureComposite fc = m_featureComposite;

    getControl().getDisplay().syncExec( new CatchRunnable()
    {
      public void runIntern() throws Exception
      {
        fc.collectChanges( changes );

        // Änderungen committen
        new ChangeFeaturesCommand( null, (FeatureChange[])changes
            .toArray( new FeatureChange[changes.size()] ) ).process();
      }
    } );
    
    monitor.worked( 1000 );

    // SPEICHERN
    final Feature rootFeature = m_featureComposite.getFeature();

    final IFile controlFile = folder.getFile( ModelNature.CONTROL_NAME );
    
    final SetContentThread thread = new SetContentThread( controlFile, !controlFile.exists(), false, false, new NullProgressMonitor() )
    {
      public void write( final Writer w ) throws Throwable
      {
        GmlSerializer.serializeFeature( w, rootFeature, new NullProgressMonitor() );
      }
    };
    thread.start();
    try
    {
      thread.join();
    }
    catch( final InterruptedException e )
    {
      throw new CoreException( Status.CANCEL_STATUS );
    }
    finally
    {
      monitor.done();
    }
    
    final CoreException fileException = thread.getFileException();
    if( fileException != null )
      throw fileException;
    
    final Throwable throwable = thread.getThrown();
    if( throwable != null )
      throw new CoreException( new Status( IStatus.ERROR, KalypsoGisPlugin.getId(), 0, "Fehler beim Speichern der Steuerdaten.\n" + throwable.getLocalizedMessage(), throwable ) );
  }
  
  /**
   * @see org.eclipse.jface.wizard.WizardPage#canFlipToNextPage()
   */
  public boolean canFlipToNextPage()
  {
    if( m_overrideCanFlipToNextPage )
      return isPageComplete();
    
    return super.canFlipToNextPage();
  }
  
}