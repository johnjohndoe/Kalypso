package org.kalypso.ui.editor.featureeditor;

import java.io.Writer;
import java.net.URL;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureType;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.IFileEditorInput;
import org.kalypso.eclipse.core.resources.ResourceUtilities;
import org.kalypso.eclipse.util.SetContentThread;
import org.kalypso.ogc.gml.featureview.FeatureComposite;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.template.featureview.Featuretemplate;
import org.kalypso.template.featureview.FeatureviewType;
import org.kalypso.template.featureview.ObjectFactory;
import org.kalypso.template.featureview.FeaturetemplateType.LayerType;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.editor.AbstractEditorPart;
import org.kalypso.util.command.ICommandTarget;
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
public class FeatureEditor extends AbstractEditorPart implements ISelectionProvider,
    ICommandTarget, IPoolListener
{
  private final ObjectFactory m_templateFactory = new ObjectFactory();

  protected final Marshaller m_marshaller;

  private final Unmarshaller m_unmarshaller;

  private final ResourcePool m_pool = KalypsoGisPlugin.getDefault().getPool();

  private Composite m_panel;

  private IPoolableObjectType m_key;

  private CommandableWorkspace m_workspace;

  private String m_featurePath;

  private FeatureComposite m_featureComposite = new FeatureComposite( null, new URL[] {} );

  private Label m_label;

  public FeatureEditor()
  {
    try
    {
      m_marshaller = m_templateFactory.createMarshaller();
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
    m_featureComposite.dispose();
    
    m_pool.removePoolListener( this );

    super.dispose();
  }

  /** File must exist! */
  protected void doSaveInternal( final IProgressMonitor monitor, final IFileEditorInput input )
  {
  // TODO implement it
  // die Daten speichern!
  }

  public final void saveTemplate( final IFile file, final IProgressMonitor monitor )
  {
    if( m_featureComposite == null || m_workspace == null )
      return;

    try
    {
      final Feature feature = m_featureComposite.getFeature();
      final FeatureType featureType = feature.getFeatureType();
      final String featurePath = m_workspace.getFeaturepathForFeature( feature );

      final FeatureviewType featureview = m_featureComposite.getFeatureview( featureType );

      final LayerType layer = m_templateFactory.createFeaturetemplateTypeLayerType();
      layer.setHref( m_key.getSourceAsString() );
      layer.setLinktype( m_key.getType() );
      layer.setFeaturePath( featurePath );

      final Featuretemplate featuretemplate = m_templateFactory.createFeaturetemplate();
      featuretemplate.setLayer( layer );
      featuretemplate.setView( featureview );

      final SetContentThread thread = new SetContentThread( file, !file.exists(), false, true,
          monitor )
      {
        protected void write( Writer writer ) throws Throwable
        {
          m_marshaller.marshal( featuretemplate, writer );
        }
      };

      thread.start();
      try
      {
        thread.join();
      }
      catch( InterruptedException e1 )
      {
        e1.printStackTrace();
      }

      final CoreException fileException = thread.getFileException();
      if( fileException != null )
        throw fileException;

      final Throwable thrown = thread.getThrown();
      if( thrown != null )
        throw thrown;
    }
    catch( final Throwable e )
    {
      // TODO: error handling!
      e.printStackTrace();
    }
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  public void createPartControl( final Composite parent )
  {
    super.createPartControl( parent );

    m_panel = new Composite( parent, SWT.NONE );
    m_panel.setLayout( new GridLayout() );

    createControls();
  }

  protected final void loadInternal( final IProgressMonitor monitor, final IFileEditorInput input )
      throws Exception
  {
    monitor.beginTask( "Ansicht laden", 1000 );

    final IFile file = input.getFile();
    final InputSource is = new InputSource( file.getContents() );
    is.setEncoding( file.getCharset() );

    final Featuretemplate m_template = (Featuretemplate)m_unmarshaller.unmarshal( is );

    m_featureComposite.addView( m_template.getView() );

    final LayerType layer = m_template.getLayer();

    m_featurePath = layer.getFeaturePath();

    final String href = layer.getHref();
    final String linktype = layer.getLinktype();
    final URL context = ResourceUtilities.createURL( file );

    m_key = new PoolableObjectType( linktype, href, context );
    m_pool.addPoolListener( this, m_key );

    monitor.worked( 1000 );
  }

  private void setWorkspace( final CommandableWorkspace workspace )
  {
    m_workspace = workspace;

    if( m_panel == null || m_panel.isDisposed() )
      return;

    m_panel.getDisplay().asyncExec( new Runnable()
    {
      public void run()
      {
        createControls();
      }
    } );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#addSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
   */
  public void addSelectionChangedListener( ISelectionChangedListener listener )
  {
  // nix tun
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#getSelection()
   */
  public ISelection getSelection()
  {
    return null;
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#removeSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
   */
  public void removeSelectionChangedListener( ISelectionChangedListener listener )
  {
  // nix tun
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#setSelection(org.eclipse.jface.viewers.ISelection)
   */
  public void setSelection( final ISelection selection )
  {
  // nix tun
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
    {
      m_featureComposite.setFeature( null );
      m_featureComposite.disposeControl();
      m_featureComposite.updateControl();
    }
  }

  protected void createControls()
  {
    // erstmal alles reseten
    if( m_label != null && !m_label.isDisposed() )
      m_label.dispose();

    m_featureComposite.setFeature( null );
    m_featureComposite.disposeControl();

    m_featureComposite.setFeature( null );
    m_featureComposite.updateControl();

    if( m_workspace == null )
    {
      if( m_panel != null )
      {
        m_label = new Label( m_panel, SWT.CENTER );
        m_label.setText( "laden..." );
        m_label.setLayoutData( new GridData( GridData.FILL_BOTH ) );
      }

      return;
    }

    final Object featureFromPath = m_workspace.getFeatureFromPath( m_featurePath );
    if( featureFromPath instanceof Feature )
    {
      final Feature feature = (Feature)featureFromPath;

      m_featureComposite.setFeature( feature );
      m_featureComposite.createControl( m_panel, SWT.NONE, feature.getFeatureType() );
      m_featureComposite.setFeature( feature );
      m_featureComposite.updateControl();
      
      m_panel.layout();
    }
    else
    {
      // TODO Fehlermeldung anzeigen
    }
  }
}