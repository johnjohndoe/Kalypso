package org.kalypso.ui.editor.featureeditor;

import java.lang.reflect.InvocationTargetException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Iterator;
import java.util.List;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.event.ModellEvent;
import org.deegree.model.feature.event.ModellEventListener;
import org.eclipse.core.resources.IEncodedStorage;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IStorage;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.IStorageEditorInput;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.eclipse.ui.part.EditorPart;
import org.eclipse.ui.progress.IProgressService;
import org.kalypso.eclipse.core.resources.ResourceUtilities;
import org.kalypso.ogc.gml.command.ChangeFeaturesCommand;
import org.kalypso.ogc.gml.featureview.FeatureChange;
import org.kalypso.ogc.gml.featureview.FeatureComposite;
import org.kalypso.ogc.gml.featureview.IFeatureChangeListener;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.template.featureview.Featuretemplate;
import org.kalypso.template.featureview.FeatureviewType;
import org.kalypso.template.featureview.ObjectFactory;
import org.kalypso.template.featureview.FeaturetemplateType.LayerType;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.command.DefaultCommandManager;
import org.kalypso.util.command.JobExclusiveCommandTarget;
import org.kalypso.util.pool.IPoolListener;
import org.kalypso.util.pool.IPoolableObjectType;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.util.pool.ResourcePool;
import org.xml.sax.InputSource;

/**
 * <p>
 * Eclipse-Editor zum Editieren von Features
 * </p>
 * 
 * @author belger
 */
public class FeatureEditor extends EditorPart implements IPoolListener, ModellEventListener
{
  private final ObjectFactory m_templateFactory = new ObjectFactory();

  protected final Marshaller m_marshaller;

  private final Unmarshaller m_unmarshaller;

  private final ResourcePool m_pool = KalypsoGisPlugin.getDefault().getPool();

  protected Composite m_panel;

  private IPoolableObjectType m_key;

  private CommandableWorkspace m_workspace;

  private String m_featurePath;

  private FeatureComposite m_featureComposite = new FeatureComposite( null, new URL[] {} );

  private Label m_label;

  private final Runnable m_dirtyRunnable = new Runnable()
  {
    public void run()
    {
      if( m_panel != null )
      {
        m_panel.getDisplay().asyncExec( new Runnable( ) {
          public void run()
          {
            fireDirtyChange(  );
          }} );
      }
    }
  };

  protected final JobExclusiveCommandTarget m_commandTarget = new JobExclusiveCommandTarget(
      new DefaultCommandManager(), m_dirtyRunnable );

  private final IFeatureChangeListener m_changeListener = new IFeatureChangeListener()
  {
    public void featureChanged( final FeatureChange change )
    {
      onFeatureChanged( change );
    }
  };

  public FeatureEditor()
  {
    try
    {
      m_marshaller = m_templateFactory.createMarshaller();
      m_marshaller.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );
      
      m_unmarshaller = m_templateFactory.createUnmarshaller();
    }
    catch( final JAXBException e )
    {
      // sollte nie passieren
      e.printStackTrace();

      throw new RuntimeException( e );
    }
  }

  /**
   * @see org.kalypso.ui.editor.AbstractEditorPart#dispose()
   */
  public void dispose()
  {
    setWorkspace( null );

    m_commandTarget.dispose();

    m_featureComposite.dispose();

    m_pool.removePoolListener( this );

    super.dispose();
  }

  /**
   * @see org.eclipse.ui.ISaveablePart#isSaveAsAllowed()
   */
  public boolean isSaveAsAllowed()
  {
    return false;
  }

  /**
   * @see org.eclipse.ui.part.EditorPart#doSaveAs()
   */
  public void doSaveAs()
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.eclipse.ui.part.EditorPart#init(org.eclipse.ui.IEditorSite,
   *      org.eclipse.ui.IEditorInput)
   */
  public void init( final IEditorSite site, final IEditorInput input ) throws PartInitException
  {
    if( !( input instanceof IStorageEditorInput ) )
      throw new PartInitException( "Can only use IStorageEditorInput" );

    setSite( site );

    setInput( input );
  }

  /**
   * @see org.eclipse.ui.part.EditorPart#setInput(org.eclipse.ui.IEditorInput)
   */
  protected final void setInput( final IEditorInput input )
  {
    if( !( input instanceof IStorageEditorInput ) )
      throw new IllegalArgumentException( "Only IStorageEditorInput supported" );

    super.setInput( input );

    load( (IStorageEditorInput)input );
  }

  /**
   * @see org.eclipse.ui.part.EditorPart#doSave(org.eclipse.core.runtime.IProgressMonitor)
   */
  public void doSave( final IProgressMonitor monitor )
  {
    try
    {
      m_pool.saveObject( m_workspace, monitor );

      m_commandTarget.resetDirty();
      fireDirtyChange();
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      final IStatus status = KalypsoGisPlugin.createErrorStatus( "", e );
      ErrorDialog.openError( getSite().getShell(), "Speichern", "Fehler beim Speichern der Daten",
          status );
    }
  }

  /**
   * @see org.eclipse.ui.part.EditorPart#isDirty()
   */
  public boolean isDirty()
  {
    return m_commandTarget.isDirty();
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#setFocus()
   */
  public void setFocus()
  {
  // nix
  }

  //  private final void saveTemplate( final IFile file, final IProgressMonitor
  // monitor )
  //  {
  //    if( m_featureComposite == null || m_workspace == null )
  //      return;
  //
  //    try
  //    {
  //      final Feature feature = m_featureComposite.getFeature();
  //      final FeatureType featureType = feature.getFeatureType();
  //      final String featurePath = m_workspace.getFeaturepathForFeature( feature );
  //
  //      final FeatureviewType featureview = m_featureComposite.getFeatureview(
  // featureType );
  //
  //      final LayerType layer =
  // m_templateFactory.createFeaturetemplateTypeLayerType();
  //      layer.setHref( m_key.getSourceAsString() );
  //      layer.setLinktype( m_key.getType() );
  //      layer.setFeaturePath( featurePath );
  //
  //      final Featuretemplate featuretemplate =
  // m_templateFactory.createFeaturetemplate();
  //      featuretemplate.setLayer( layer );
  //      featuretemplate.setView( featureview );
  //
  //      final SetContentThread thread = new SetContentThread( file, !file.exists(),
  // false, true,
  //          monitor )
  //      {
  //        protected void write( Writer writer ) throws Throwable
  //        {
  //          m_marshaller.marshal( featuretemplate, writer );
  //        }
  //      };
  //
  //      thread.start();
  //      try
  //      {
  //        thread.join();
  //      }
  //      catch( InterruptedException e1 )
  //      {
  //        e1.printStackTrace();
  //      }
  //
  //      final CoreException fileException = thread.getFileException();
  //      if( fileException != null )
  //        throw fileException;
  //
  //      final Throwable thrown = thread.getThrown();
  //      if( thrown != null )
  //        throw thrown;
  //    }
  //    catch( final Throwable e )
  //    {
  //      // todo: error handling!
  //      e.printStackTrace();
  //    }
  //  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  public void createPartControl( final Composite parent )
  {
    m_panel = new Composite( parent, SWT.NONE );
    m_panel.setLayout( new GridLayout() );

    final IActionBars actionBars = getEditorSite().getActionBars();
    actionBars.setGlobalActionHandler( ActionFactory.UNDO.getId(), m_commandTarget.undoAction );
    actionBars.setGlobalActionHandler( ActionFactory.REDO.getId(), m_commandTarget.redoAction );
    actionBars.updateActionBars();

    createControls( null );

    setWorkspace( m_workspace );
  }

  protected final void load( final IStorageEditorInput input )
  {
    final WorkspaceModifyOperation op = new WorkspaceModifyOperation()
    {
      protected void execute( final IProgressMonitor monitor ) throws CoreException
      {
        loadInput( input, monitor );
      }
    };

    final IProgressService progressService = PlatformUI.getWorkbench().getProgressService();
    try
    {
      progressService.busyCursorWhile( op );
    }
    catch( final InvocationTargetException e )
    {
      e.printStackTrace();

      final CoreException ce = (CoreException)e.getTargetException();
      ErrorDialog.openError( getEditorSite().getShell(), "Fehler", "Fehler beim Laden der Ansicht",
          ce.getStatus() );
    }
    catch( final InterruptedException e )
    {
      e.printStackTrace();
    }
  }

  protected final void loadInput( final IStorageEditorInput input, final IProgressMonitor monitor )
      throws CoreException
  {
    monitor.beginTask( "Ansicht laden", 1000 );

    try
    {
      final IStorage storage = input.getStorage();
      final InputSource is = new InputSource( storage.getContents() );
      
      if( storage instanceof IEncodedStorage )
        is.setEncoding( ((IEncodedStorage)storage).getCharset() );

      final Featuretemplate m_template = (Featuretemplate)m_unmarshaller.unmarshal( is );

      final List views = m_template.getView();
      for( Iterator iter = views.iterator(); iter.hasNext(); )
        m_featureComposite.addView( (FeatureviewType)iter.next() );

      final LayerType layer = m_template.getLayer();

      m_featurePath = layer.getFeaturePath();

      final String href = layer.getHref();
      final String linktype = layer.getLinktype();
      
      final IFile file = ResourcesPlugin.getWorkspace().getRoot().getFile( storage.getFullPath() );
      final URL context = ResourceUtilities.createURL( file );

      m_key = new PoolableObjectType( linktype, href, context );
      m_pool.addPoolListener( this, m_key );
    }
    catch( final MalformedURLException e )
    {
      e.printStackTrace();

      throw new CoreException( KalypsoGisPlugin.createErrorStatus(
          "Fehler beim Parsen der Context-URL", e ) );
    }
    catch( final JAXBException e )
    {
      e.printStackTrace();

      throw new CoreException( KalypsoGisPlugin.createErrorStatus( "Fehler beim Lesen von XML", e ) );
    }
    finally
    {
      monitor.done();
    }
  }

  private void setWorkspace( final CommandableWorkspace workspace )
  {
    if( m_workspace != null )
      m_workspace.removeModellListener( this );

    m_workspace = workspace;

    if( m_workspace != null )
      m_workspace.addModellListener( this );

    m_commandTarget.setCommandManager( workspace );

    if( m_panel == null || m_panel.isDisposed() )
      return;

    m_panel.getDisplay().asyncExec( new Runnable()
    {
      public void run()
      {
        createControls( workspace );
      }
    } );
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#objectLoaded(org.kalypso.util.pool.IPoolableObjectType,
   *      java.lang.Object, org.eclipse.core.runtime.IStatus)
   */
  public void objectLoaded( final IPoolableObjectType key, final Object newValue,
      final IStatus status )
  {
    // Daten sind jetzt da!
    if( m_pool.equalsKeys( m_key, key ) )
      setWorkspace( (CommandableWorkspace)newValue );
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#objectInvalid(org.kalypso.util.pool.IPoolableObjectType,
   *      java.lang.Object)
   */
  public void objectInvalid( final IPoolableObjectType key, final Object oldValue )
  {
    if( m_pool.equalsKeys( key, m_key ) )
      setWorkspace( null );
  }

  protected void createControls( final CommandableWorkspace workspace )
  {
    // erstmal alles reseten
    if( m_label != null && !m_label.isDisposed() )
      m_label.dispose();

    m_featureComposite.setFeature( null );
    m_featureComposite.disposeControl();

    m_featureComposite.setFeature( null );
    m_featureComposite.updateControl();

    if( workspace == null )
    {
      if( m_panel != null )
      {
        m_label = new Label( m_panel, SWT.CENTER );
        m_label.setText( "laden..." );
        m_label.setLayoutData( new GridData( GridData.FILL_BOTH ) );
      }

      return;
    }

    final Object featureFromPath = workspace.getFeatureFromPath( m_featurePath );
    if( featureFromPath instanceof Feature )
    {
      final Feature feature = (Feature)featureFromPath;

      m_featureComposite.setFeature( feature );
      m_featureComposite.createControl( m_panel, SWT.NONE, feature.getFeatureType() );
      m_featureComposite.setFeature( feature );
      m_featureComposite.updateControl();

      m_panel.layout();

      m_featureComposite.addChangeListener( m_changeListener );

      m_commandTarget.resetDirty();
      fireDirtyChange();
    }
    else
    {
      // todo Fehlermeldung anzeigen
    }
  }

  protected void onFeatureChanged( final FeatureChange change )
  {
    m_commandTarget.postCommand( new ChangeFeaturesCommand( m_workspace, new FeatureChange[]
    { change } ), m_dirtyRunnable );
  }

  protected void fireDirtyChange()
  {
     firePropertyChange( PROP_DIRTY );
  }

  /**
   * @see org.deegree.model.feature.event.ModellEventListener#onModellChange(org.deegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    final FeatureComposite featureComposite = m_featureComposite;
    if( m_panel != null )
    {
      m_panel.getDisplay().asyncExec( new Runnable()
      {
        public void run()
        {
          featureComposite.updateControl();
        }
      } );
    }
  }
}