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
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
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

  private boolean m_update;

  private Button m_checkUpdate;

  private GMLWorkspace m_workspace;

  private IFolder m_currentCalcCase;

  public SteuerparameterWizardPage( final IProjectProvider pp,
      final boolean overrideCanFlipToNextPage, final ImageDescriptor image )
  {
    super( "EditCalcCaseControlPage", "Steuerparameter", image );

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

      // gleich mal den Workspace auf das default setzen
      final ModelNature nature = (ModelNature)project.getNature( ModelNature.ID );
      m_workspace = nature.loadOrCreateControl( m_currentCalcCase );

      // Vorlage auslesen
      final URL viewURL = new URL( "platform:/resource/" + project.getName() + "/"
          + ModelNature.CONTROL_VIEW_PATH );

      final Feature f = m_workspace.getRootFeature();

      m_featureComposite = new FeatureComposite( f, new URL[]
      { viewURL } );

      final Composite panel = new Composite( parent, SWT.NONE );
      panel.setLayout( new GridLayout() );
      panel.setLayoutData( new GridData( GridData.FILL_BOTH ) );

      final Control featureControl = m_featureComposite.createControl( panel, SWT.NONE );
      featureControl.setLayoutData( new GridData( GridData.FILL_BOTH ) );

      final Button checkUpdate = new Button( panel, SWT.CHECK );
      checkUpdate.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
      checkUpdate.setText( "Zeitreihen aktualisieren" );
      checkUpdate
          .setToolTipText( "falls aktiv, werden die Zeitreihen im nächsten Schritt aktualisiert" );
      checkUpdate.setSelection( m_update );
      m_checkUpdate = checkUpdate;

      checkUpdate.addSelectionListener( new SelectionAdapter()
      {
        /**
         * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
         */
        public void widgetSelected( final SelectionEvent e )
        {
            setUpdate( checkUpdate.getSelection() );
        }
      } );

      setControl( panel );
    }
    catch( final MalformedURLException e )
    {
      // ERROR handling
      e.printStackTrace();
    }
    catch( CoreException e )
    {
      e.printStackTrace();
    }
  }

  public void saveChanges( final IFolder folder, final IProgressMonitor monitor )
      throws CoreException
  {
    monitor.beginTask( "Steuerparameter speichern", 2000 );

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
    final IFile controlFile = folder.getFile( ModelNature.CONTROL_NAME );

    final GMLWorkspace workspace = m_workspace;
    final SetContentThread thread = new SetContentThread( controlFile, !controlFile.exists(),
        false, false, new NullProgressMonitor() )
    {
      public void write( final Writer w ) throws Throwable
      {
        GmlSerializer.serializeWorkspace( w, workspace);
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
      throw new CoreException( new Status( IStatus.ERROR, KalypsoGisPlugin.getId(), 0,
          "Fehler beim Speichern der Steuerdaten.\n" + throwable.getLocalizedMessage(), throwable ) );
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

  public boolean isUpdate()
  {
    return m_update;
  }

  public void setUpdate( final boolean update )
  {
    if( m_update != update )
    {
      m_update = update;

      final Button checkUpdate = m_checkUpdate;
      if( checkUpdate != null && !checkUpdate.isDisposed() )
        checkUpdate.getDisplay().asyncExec( new Runnable()
        {
          public void run()
          {
            checkUpdate.setSelection( update );
          }
        } );
    }

  }

  /**
   * Setzt dne aktuellen Rechenfall, ist dort schon eine .calculation vorhanden,
   * wird diese geladen, sonst die default.
   * 
   * @throws CoreException
   */
  public void setFolder( final IFolder currentCalcCase ) throws CoreException
  {
    m_currentCalcCase = currentCalcCase;
    
    final ModelNature nature = (ModelNature)m_projectProvider.getProject().getNature(
        ModelNature.ID );

    GMLWorkspace workspace = nature.loadOrCreateControl( currentCalcCase );
    setWorkspace( workspace );
  }

  public void setWorkspace( final GMLWorkspace workspace )
  {
    m_workspace = workspace;

    final FeatureComposite featureComposite = m_featureComposite;
    if( featureComposite != null )
    {
      featureComposite.setFeature( workspace.getRootFeature() );
      
      final Control control = m_featureComposite.getControl();
      if( control != null && !control.isDisposed() )
      {
        control.getDisplay().asyncExec( new Runnable( ) {
          public void run()
          {
            featureComposite.updateControl();
          }} );
      }
    }
  }

}