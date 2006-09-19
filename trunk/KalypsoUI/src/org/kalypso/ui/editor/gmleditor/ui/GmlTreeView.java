package org.kalypso.ui.editor.gmleditor.ui;

import java.io.Reader;
import java.io.Writer;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.SafeRunner;
import org.eclipse.jface.util.SafeRunnable;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.views.navigator.LocalSelectionTransfer;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.viewers.ArrayTreeContentProvider;
import org.kalypso.contribs.eclipse.jface.viewers.ConstantLabelProvider;
import org.kalypso.contribs.java.util.Arrays;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.jwsdp.JaxbUtilities;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.AbstractFeatureSelection;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.FeatureSelectionHelper;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypso.ogc.gml.selection.IFeatureSelectionListener;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.template.gistreeview.Gistreeview;
import org.kalypso.template.gistreeview.ObjectFactory;
import org.kalypso.template.types.LayerType;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.ImageProvider.DESCRIPTORS;
import org.kalypso.util.pool.IPoolListener;
import org.kalypso.util.pool.IPoolableObjectType;
import org.kalypso.util.pool.KeyComparator;
import org.kalypso.util.pool.KeyInfo;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.util.pool.ResourcePool;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.feature.event.FeaturesChangedModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventListener;
import org.kalypsodeegree.model.feature.event.ModellEventProvider;
import org.kalypsodeegree.model.feature.event.ModellEventProviderAdapter;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;
import org.kalypsodeegree_impl.model.feature.visitors.CollectorVisitor;
import org.xml.sax.InputSource;

/**
 * Is among others a {@link org.eclipse.jface.viewers.IPostSelectionProvider}. The returned selection consists of
 * {@link org.kalypsodeegree.model.feature.Feature}and
 * {@link org.kalypsodeegree.model.feature.FeatureAssociationTypeProperty}objects.
 * 
 * @author <verschiedene>
 */
public class GmlTreeView implements ISelectionProvider, IPoolListener, ModellEventProvider, ModellEventListener
{
  private final static JAXBContext JC = JaxbUtilities.createQuiet( ObjectFactory.class );

  private final IFeatureSelectionListener m_globalSelectionChangedListener = new IFeatureSelectionListener()
  {
    public void selectionChanged( final IFeatureSelection selection )
    {
      handleGlobalSelectionChanged( selection );
    }
  };

  private final ISelectionChangedListener m_treeSelectionChangedListener = new ISelectionChangedListener()
  {
    public void selectionChanged( final SelectionChangedEvent event )
    {
      handleTreeSelectionChanged( event );
    }
  };

  private final IDoubleClickListener m_doubleClickListener = new IDoubleClickListener()
  {
    public void doubleClick( final DoubleClickEvent event )
    {
      if( event.getSelection() instanceof IStructuredSelection )
      {
        final TreeViewer treeViewer = getTreeViewer();

        final IStructuredSelection selection = (IStructuredSelection) event.getSelection();
        final Object obj = selection.getFirstElement();
        if( obj instanceof LinkedFeatureElement2 )
        {
          // Jump to linked element
          final Feature feature = ((LinkedFeatureElement2) obj).getDecoratedFeature();
          final StructuredSelection ss = new StructuredSelection( feature );
          treeViewer.setSelection( ss, true );
          treeViewer.expandToLevel( feature, 1 );
          treeViewer.reveal( feature );
        }
        else
        {
          // Toggle expansion state
          final boolean expandedState = treeViewer.getExpandedState( obj );
          treeViewer.setExpandedState( obj, !expandedState );
        }
      }
    }
  };

  private final List<ISelectionChangedListener> m_selectionListeners = new ArrayList<ISelectionChangedListener>( 5 );

  private final ResourcePool m_pool = KalypsoGisPlugin.getDefault().getPool();

  private final GMLEditorLabelProvider2 m_labelProvider = new GMLEditorLabelProvider2();

  private final GMLEditorContentProvider2 m_contentProvider = new GMLEditorContentProvider2();

  private final ModellEventProviderAdapter m_eventProvider = new ModellEventProviderAdapter();

  protected CommandableWorkspace m_workspace = null;

  protected Composite m_composite = null;

  private PoolableObjectType m_key = null;

  private TreeViewer m_treeViewer = null;

  protected final IFeatureSelectionManager m_selectionManager;

  private Gistreeview m_gisTreeview;

  private boolean m_disposed = false;

  public GmlTreeView( final Composite composite, final IFeatureSelectionManager selectionManager )
  {
    m_composite = composite;
    m_selectionManager = selectionManager;

    m_selectionManager.addSelectionListener( m_globalSelectionChangedListener );

    createViewerPart();

    m_treeViewer.addDoubleClickListener( m_doubleClickListener );

    // als post selection listener anmelden,
    // dann passiert das ganze gedöns nicht immer sofort
    m_treeViewer.addPostSelectionChangedListener( m_treeSelectionChangedListener );
  }

  protected void handleGlobalSelectionChanged( final IFeatureSelection selection )
  {
    final TreeViewer treeViewer = m_treeViewer;
    // must be sync, if not we get a racing condition with handleModelChange
    final Control control = treeViewer.getControl();
    if( control == null || control.isDisposed() )
      return;

    final GMLEditorContentProvider2 contentProvider = m_contentProvider;
    control.getDisplay().syncExec( new Runnable()
    {
      public void run( )
      {
        if( !control.isDisposed() )
        {
          final Feature[] globalFeatures = FeatureSelectionHelper.getFeatures( selection, getWorkspace() );
          final Feature[] selectedFeatures = filterSelectedFeatures( treeViewer.getSelection() );
          final boolean isEqual = Arrays.equalsUnordered( globalFeatures, selectedFeatures );
          if( !isEqual )
          {
            for( int i = 0; i < globalFeatures.length; i++ )
              contentProvider.expandElement( contentProvider.getParent( globalFeatures[i] ) );
            treeViewer.setSelection( selection, true );
          }
        }
      }
    } );
  }

  protected void handleTreeSelectionChanged( final SelectionChangedEvent event )
  {
    final ISelection selection = event.getSelection();
    final Feature[] features = filterSelectedFeatures( selection );

    final Object input = m_treeViewer.getInput();
    if( input instanceof CommandableWorkspace )
    {
      final CommandableWorkspace workspace = (CommandableWorkspace) input;

      // remove all feature of my workspace from the selection manager
      final CollectorVisitor visitor = new CollectorVisitor();
      workspace.accept( visitor, workspace.getRootFeature(), FeatureVisitor.DEPTH_INFINITE );
      final Feature[] toRemove = visitor.getResults( true );

      final EasyFeatureWrapper[] toAdd = new EasyFeatureWrapper[features.length];
      for( int i = 0; i < toAdd.length; i++ )
      {
        final Feature feature = features[i];
        final Feature parent = feature.getParent();
        final IRelationType parentProperty = m_contentProvider.getParentFeatureProperty( feature );

        toAdd[i] = new EasyFeatureWrapper( workspace, feature, parent, parentProperty );
      }

      m_selectionManager.changeSelection( toRemove, toAdd );

      fireSelectionChanged();
    }
  }

  protected Feature[] filterSelectedFeatures( final ISelection selection )
  {
    final Object[] selectedTreeItems = ((IStructuredSelection) selection).toArray();
    final List<Feature> selectedFeatures = new ArrayList<Feature>();
    for( int i = 0; i < selectedTreeItems.length; i++ )
    {
      final Object treeElement = selectedTreeItems[i];
      Feature feature = null;
      if( treeElement instanceof LinkedFeatureElement2 )
        feature = ((LinkedFeatureElement2) treeElement).getDecoratedFeature();
      else if( treeElement instanceof Feature )
        feature = (Feature) treeElement;
      if( feature != null && !selectedFeatures.contains( feature ) )
        selectedFeatures.add( feature );
    }
    return selectedFeatures.toArray( new Feature[selectedFeatures.size()] );
  }

  private void createViewerPart( )
  {
    final GridLayout layout = new GridLayout();
    layout.numColumns = 1;
    layout.verticalSpacing = 2;
    layout.marginWidth = 0;
    layout.marginHeight = 2;

    // DIRTY! should be set from outside
    m_composite.setLayout( layout );

    m_treeViewer = new TreeViewer( m_composite );
    m_treeViewer.setUseHashlookup( true );

    // add drag and drop support
    int ops = DND.DROP_COPY | DND.DROP_MOVE | DND.DROP_LINK;
    final Transfer[] transfers = new Transfer[] { LocalSelectionTransfer.getInstance() };
    m_treeViewer.addDragSupport( ops, transfers, new GmlTreeDragListener( this ) );
    m_treeViewer.addDropSupport( ops, transfers, new GmlTreeDropAdapter( this ) );

    final GridData layoutData = new GridData();
    layoutData.grabExcessHorizontalSpace = true;
    layoutData.grabExcessVerticalSpace = true;
    layoutData.horizontalAlignment = GridData.FILL;
    layoutData.verticalAlignment = GridData.FILL;
    m_treeViewer.getControl().setLayoutData( layoutData );
  }

  public void dispose( )
  {
    m_disposed = true;
    m_composite.dispose();
    m_pool.removePoolListener( this );

    m_selectionManager.removeSelectionListener( m_globalSelectionChangedListener );
    m_treeViewer.removePostSelectionChangedListener( m_treeSelectionChangedListener );
    m_treeViewer.removeDoubleClickListener( m_doubleClickListener );
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventListener#onModellChange(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  @SuppressWarnings("unchecked")
  public void onModellChange( final ModellEvent modellEvent )
  {
    if( modellEvent instanceof FeatureStructureChangeModellEvent )
    {
      final FeatureStructureChangeModellEvent structureEvent = (FeatureStructureChangeModellEvent) modellEvent;
      final Feature[] parentFeature = structureEvent.getParentFeatures();

      if( !m_composite.isDisposed() )
      {
        final TreeViewer treeViewer = m_treeViewer;
        // must be sync, if not we get a racing condition with handleGlobalSelection
        m_composite.getDisplay().syncExec( new Runnable()
        {
          public void run( )
          {
            // This does not work nicely. In order to refresh the tree in a nice way,
            // the modell event should also provide which festures where added/removed
            final Object[] expandedElements = treeViewer.getExpandedElements();

            if( parentFeature == null )
              treeViewer.refresh();
            else
            {
              for( int i = 0; i < parentFeature.length; i++ )
              {
                final Feature feature = parentFeature[i];
                treeViewer.refresh( feature );
              }
            }

            treeViewer.setExpandedElements( expandedElements );
          }
        } );
      }
    }
    else if( modellEvent instanceof FeaturesChangedModellEvent )
    {
      final FeaturesChangedModellEvent fcme = (FeaturesChangedModellEvent) modellEvent;
      final List featureList = fcme.getFeatures();
      final Feature[] features = (Feature[]) featureList.toArray( new Feature[featureList.size()] );
      final Control treeControl = m_treeViewer.getControl();
      final TreeViewer treeViewer = m_treeViewer;
      if( treeControl != null && !treeControl.isDisposed() )
      {
        treeControl.getDisplay().asyncExec( new Runnable()
        {
          public void run( )
          {
            if( !treeControl.isDisposed() )
            {
              for( final Feature feature : features )
                treeViewer.refresh( feature, true );
            }
          }
        } );
      }
    }

    fireModellEvent( modellEvent );
  }

  public void saveData( final IProgressMonitor monitor ) throws CoreException
  {
    try
    {
      if( m_workspace != null )
      {
        KalypsoGisPlugin.getDefault().getPool().saveObject( m_workspace, monitor );
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new CoreException( StatusUtilities.statusFromThrowable( e, "Fehler beim Speichern" ) );
    }
  }

  public CommandableWorkspace getWorkspace( )
  {
    return m_workspace;
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventProvider#addModellListener(org.kalypsodeegree.model.feature.event.ModellEventListener)
   */
  public void addModellListener( ModellEventListener listener )
  {
    m_eventProvider.addModellListener( listener );
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventProvider#removeModellListener(org.kalypsodeegree.model.feature.event.ModellEventListener)
   */
  public void removeModellListener( ModellEventListener listener )
  {
    m_eventProvider.removeModellListener( listener );
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventProvider#fireModellEvent(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  public void fireModellEvent( ModellEvent event )
  {
    m_eventProvider.fireModellEvent( event );
  }

  public TreeViewer getTreeViewer( )
  {
    return m_treeViewer;
  }

  public ISelection getSelection( )
  {
    if( m_treeViewer.getContentProvider() != m_contentProvider )
      return m_treeViewer.getSelection();

    return new TreeFeatureSelection( (IStructuredSelection) m_treeViewer.getSelection() );
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#objectLoaded(org.kalypso.util.pool.IPoolableObjectType, java.lang.Object,
   *      org.eclipse.core.runtime.IStatus)
   */
  public void objectLoaded( final IPoolableObjectType key, final Object newValue, final IStatus status )
  {
    if( KeyComparator.getInstance().compare( key, m_key ) == 0 )
    {
      if( m_workspace != null )
      {
        m_workspace.removeModellListener( this );
        m_workspace = null;
      }

      m_workspace = ((CommandableWorkspace) newValue);
      if( m_workspace != null )
        m_workspace.addModellListener( this );

      final String rootPathString = m_gisTreeview.getInput().getFeatureXPath();

      final TreeViewer treeViewer = m_treeViewer;
      final Control control = treeViewer.getControl();
      if( control == null || control.isDisposed() )
        return;

      final GMLEditorContentProvider2 contentProvider = m_contentProvider;
      final GMLEditorLabelProvider2 labelProvider = m_labelProvider;
      final Display display = control.getDisplay();
      display.asyncExec( new Runnable()
      {
        public void run( )
        {
          if( !control.isDisposed() )
          {
            if( m_workspace == null )
            {
              final Image failImg = KalypsoGisPlugin.getImageProvider().getImage( DESCRIPTORS.FAILED_LOADING_OBJ );
              treeViewer.setLabelProvider( new ConstantLabelProvider( status.getMessage(), failImg ) );
              treeViewer.setContentProvider( new ArrayTreeContentProvider() );
              treeViewer.getTree().setLinesVisible( false );
              treeViewer.setInput( new Object[] { "" } );
            }
            else
            {
              treeViewer.setLabelProvider( labelProvider );
              treeViewer.setContentProvider( contentProvider );
              treeViewer.getTree().setLinesVisible( false );
              treeViewer.setInput( m_workspace );

              final GMLXPath rootPath = new GMLXPath( rootPathString );
              contentProvider.setRootPath( rootPath );
            }
          }
        }
      } );

      if( m_workspace != null )
        display.asyncExec( new Runnable()
        {
          public void run( )
          {
            if( !control.isDisposed() )
            {
              final Object[] elements = contentProvider.getElements( m_workspace );
              if( elements != null && elements.length > 0 )
                treeViewer.setSelection( new StructuredSelection( elements[0] ) );
            }
          }
        } );
    }
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#objectInvalid(org.kalypso.util.pool.IPoolableObjectType, java.lang.Object)
   */
  public void objectInvalid( final IPoolableObjectType key, final Object oldValue )
  {
    final Image failImg = KalypsoGisPlugin.getImageProvider().getImage( DESCRIPTORS.FAILED_LOADING_OBJ );
    m_treeViewer.setLabelProvider( new ConstantLabelProvider( "no data...", failImg ) );
    m_treeViewer.setContentProvider( new ArrayTreeContentProvider() );
    m_treeViewer.getTree().setLinesVisible( false );
    m_treeViewer.setInput( new Object[] { "" } );
  }

  protected void loadInput( final Reader r, final URL context, final IProgressMonitor monitor ) throws CoreException
  {
    monitor.beginTask( "Ansicht laden", 1000 );
    try
    {
      final Unmarshaller unmarshaller = JC.createUnmarshaller();

      final InputSource is = new InputSource( r );

      m_gisTreeview = (Gistreeview) unmarshaller.unmarshal( is );

      final LayerType input = m_gisTreeview.getInput();

      // set the rootPath twice (here and after object loaded), in order
      // to suppress the dirty flag after load
      final String rootPathString = input.getFeatureXPath();
      final GMLXPath rootPath = new GMLXPath( rootPathString );
      m_contentProvider.setRootPath( rootPath );

      final Image waitImg = KalypsoGisPlugin.getImageProvider().getImage( DESCRIPTORS.WAIT_LOADING_OBJ );
      m_treeViewer.setLabelProvider( new ConstantLabelProvider( "Loading " + context + "...", waitImg ) );
      m_treeViewer.setContentProvider( new ArrayTreeContentProvider() );
      m_treeViewer.getTree().setLinesVisible( false );
      m_treeViewer.setInput( new Object[] { "" } );

      final String href = input.getHref();
      final String linktype = input.getLinktype();
      m_key = new PoolableObjectType( linktype, href, context );
      m_pool.addPoolListener( this, m_key );
    }
    catch( final JAXBException e )
    {
      e.printStackTrace();
      throw new CoreException( StatusUtilities.statusFromThrowable( e, "Fehler beim Lesen der Vorlage" ) );
    }
    finally
    {
      monitor.done();
    }
  }

  protected void saveInput( final Writer writer, final IProgressMonitor monitor ) throws CoreException
  {
    monitor.beginTask( "Baumansicht speichern", 1000 );
    try
    {
      final GMLXPath rootPath = m_contentProvider.getRootPath();
      m_gisTreeview.getInput().setFeatureXPath( rootPath.toString() );

      final Marshaller marshaller = JaxbUtilities.createMarshaller( JC, true );
      marshaller.marshal( m_gisTreeview, writer );
    }
    catch( final JAXBException e )
    {
      e.printStackTrace();
      throw new CoreException( StatusUtilities.statusFromThrowable( e, "Fehler beim Speichern der Vorlage" ) );
    }
    finally
    {
      monitor.done();
    }
  }

  public IFeatureSelectionManager getSelectionManager( )
  {
    return m_selectionManager;
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#addSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
   */
  public void addSelectionChangedListener( final ISelectionChangedListener listener )
  {
    m_selectionListeners.add( listener );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#removeSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
   */
  public void removeSelectionChangedListener( final ISelectionChangedListener listener )
  {
    m_selectionListeners.remove( listener );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#setSelection(org.eclipse.jface.viewers.ISelection)
   */
  public void setSelection( final ISelection selection )
  {
    // should never happen
    m_treeViewer.setSelection( selection );
  }

  private final void fireSelectionChanged( )
  {
    final ISelectionChangedListener[] listenersArray = m_selectionListeners.toArray( new ISelectionChangedListener[m_selectionListeners.size()] );

    final SelectionChangedEvent e = new SelectionChangedEvent( this, getSelection() );
    for( int i = 0; i < listenersArray.length; i++ )
    {
      final ISelectionChangedListener l = listenersArray[i];
      final SafeRunnable safeRunnable = new SafeRunnable()
      {
        public void run( )
        {
          l.selectionChanged( e );
        }
      };

      SafeRunner.run( safeRunnable );
    }
  }

  private final class TreeFeatureSelection extends AbstractFeatureSelection
  {
    public TreeFeatureSelection( final IStructuredSelection selection )
    {
      super( selection );
    }

    public CommandableWorkspace getWorkspace( final Feature feature )
    {
      return m_workspace;
    }

    public Feature getParentFeature( final Feature feature )
    {
      return feature.getParent();
    }

    public IRelationType getParentFeatureProperty( final Feature feature )
    {
      final GMLEditorContentProvider2 contentProvider = (GMLEditorContentProvider2) getTreeViewer().getContentProvider();
      return contentProvider.getParentFeatureProperty( feature );
    }

    /**
     * @see org.kalypso.ogc.gml.selection.IFeatureSelection#getAllFeatures()
     */
    public EasyFeatureWrapper[] getAllFeatures( )
    {
      return FeatureSelectionHelper.createEasyWrappers( this );
    }

    /**
     * @see org.kalypso.ogc.gml.selection.IFeatureSelection#getSelectionManager()
     */
    public IFeatureSelectionManager getSelectionManager( )
    {
      return m_selectionManager;
    }

    /**
     * @see org.kalypso.ogc.gml.selection.IFeatureSelection#getFocusedFeature()
     */
    public Feature getFocusedFeature( )
    {
      // the tree doesn't support focused features
      return null;
    }

    /**
     * @see org.kalypso.ogc.gml.selection.IFeatureSelection#getFocusedProperty()
     */
    public IPropertyType getFocusedProperty( )
    {
      // the tree doesn't support focused features
      return null;
    }
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#isDisposed()
   */
  public boolean isDisposed( )
  {
    return m_disposed;
  }

  /** Returns true, if this viewer is dirty. This is the case, if the template (not the data) has changed. */
  public boolean isDirty( )
  {
    if( m_gisTreeview == null )
      return false;

    final GMLXPath currentRootPath = m_contentProvider.getRootPath();
    final String rootPathString = m_gisTreeview.getInput().getFeatureXPath();
    final GMLXPath rootPath = new GMLXPath( rootPathString );

    return !rootPath.equals( currentRootPath );
  }

  public boolean isDataDirty( )
  {
    if( m_workspace != null )
    {
      final KeyInfo info = m_pool.getInfo( m_workspace );
      return info != null && info.isDirty();
    }

    return false;
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#dirtyChanged(org.kalypso.util.pool.IPoolableObjectType, boolean)
   */
  public void dirtyChanged( final IPoolableObjectType key, final boolean isDirty )
  {
    // TODO propagate dirty change to GmlEditor if present
  }

}