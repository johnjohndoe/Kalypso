package org.kalypso.ui.editor.gmleditor.ui;

import java.io.Reader;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
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
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.part.PluginTransfer;
import org.eclipse.ui.views.navigator.LocalSelectionTransfer;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
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
import org.kalypso.util.pool.IPoolListener;
import org.kalypso.util.pool.IPoolableObjectType;
import org.kalypso.util.pool.KeyComparator;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.util.pool.ResourcePool;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventListener;
import org.kalypsodeegree.model.feature.event.ModellEventProvider;
import org.kalypsodeegree.model.feature.event.ModellEventProviderAdapter;
import org.kalypsodeegree_impl.model.feature.visitors.CollectorVisitor;
import org.xml.sax.InputSource;

/**
 * Is among others a {@link org.eclipse.jface.viewers.IPostSelectionProvider}. The returned selection consists of
 * {@link org.kalypsodeegree.model.feature.Feature}and
 * {@link org.kalypsodeegree.model.feature.FeatureAssociationTypeProperty}objects. Changing the selection from outside
 * is not supported (todo).
 * 
 * @author <verschiedene>
 */
public class GmlTreeView implements ISelectionProvider, IPoolListener, ModellEventProvider, ModellEventListener
{
  protected GMLEditorLabelProvider2 m_labelProvider = new GMLEditorLabelProvider2();

  protected GMLEditorContentProvider2 m_contentProvider = new GMLEditorContentProvider2();

  protected CommandableWorkspace m_workspace = null;

  protected final Composite m_composite;

  private final ModellEventProviderAdapter m_eventProvider = new ModellEventProviderAdapter();

  private PoolableObjectType m_key = null;

  private final ResourcePool m_pool = KalypsoGisPlugin.getDefault().getPool();

  private final ObjectFactory m_factory = new ObjectFactory();

  private final List m_selectionListeners = new ArrayList( 5 );

  private TreeViewer m_treeViewer = null;

  private final IFeatureSelectionListener m_globalSelectionChangedListener = new IFeatureSelectionListener()
  {
    public void selectionChanged( final IFeatureSelection selection )
    {
      handleGlobalSelectionChanged( selection );
    }
  };

  final ISelectionChangedListener m_treeSelectionChangedListener = new ISelectionChangedListener()
  {
    public void selectionChanged( final SelectionChangedEvent event )
    {
      handleTreeSelectionChanged( event );
    }
  };
  
  public static final int DEFAULT_EXPATIONA_LEVEL = 3;

  private GmlTreeDropAdapter m_dropAdapter;

  protected final IFeatureSelectionManager m_selectionManager;

  public GmlTreeView( final Composite composite, final IFeatureSelectionManager selectionManager )
  {
    m_composite = composite;
    m_selectionManager = selectionManager;

    m_selectionManager.addSelectionListener( m_globalSelectionChangedListener );

    createViewerPart();

    hookListeners( m_treeViewer );

    // als post selection listener anmelden,
    // dann passiert das ganze gedöns nicht immer sofort
    m_treeViewer.addPostSelectionChangedListener( m_treeSelectionChangedListener );
  }

  protected void handleGlobalSelectionChanged( final IFeatureSelection selection )
  {
    final Feature[] globalFeatures = FeatureSelectionHelper.getFeatures( selection, getWorkspace() );
    final Feature[] selectedFeatures = filterSelectedFeatures( m_treeViewer.getSelection() );
    final boolean isEqual = FeatureSelectionHelper.compare( globalFeatures, selectedFeatures );
    if( !isEqual )
    {
      final TreeViewer treeViewer = m_treeViewer;
      treeViewer.getControl().getDisplay().syncExec( new Runnable()
      {
        public void run()
        {
//          final Object[] currentExpansion = treeViewer.getExpandedElements();
          treeViewer.setSelection( selection, true );
          for( int i = 0; i < globalFeatures.length; i++ )
            m_contentProvider.expandElement( m_contentProvider.getParent( globalFeatures[i] ) );
          
//          final ArrayList list = new ArrayList(  );
//          Arrays.addAll(list, currentExpansion );
//          Arrays.addAll(list, globalFeatures );
//
//          treeViewer.setExpandedElements( list.toArray() );
        }
      } );
    }
  }

  protected void handleTreeSelectionChanged( final SelectionChangedEvent event )
  {
    final ISelection selection = event.getSelection();
    final Feature[] features = filterSelectedFeatures( selection );

    final Object input = m_treeViewer.getInput();
    if( input instanceof CommandableWorkspace )
    {
      final CommandableWorkspace workspace = (CommandableWorkspace)input;

      // remove all feature of my workspace from the selection manager
      final CollectorVisitor visitor = new CollectorVisitor();
      workspace.accept( visitor, workspace.getRootFeature(), FeatureVisitor.DEPTH_INFINITE );
      final Feature[] toRemove = visitor.getResults( true );

      final EasyFeatureWrapper[] toAdd = new EasyFeatureWrapper[features.length];
      for( int i = 0; i < toAdd.length; i++ )
      {
        final Feature feature = features[i];
        final Feature parent = m_contentProvider.getParentFeature( feature );
        final String parentProperty = m_contentProvider.getParentFeatureProperty( feature );

        toAdd[i] = new EasyFeatureWrapper( workspace, feature, parent, parentProperty );
      }

      m_selectionManager.changeSelection( toRemove, toAdd );

      fireSelectionChanged();
    }
  }

  private Feature[] filterSelectedFeatures( final ISelection selection )
  {
    final Object[] selectedTreeItems = ( (IStructuredSelection)selection ).toArray();
    final List selectedFeatures = new ArrayList();
    for( int i = 0; i < selectedTreeItems.length; i++ )
    {
      final Object treeElement = selectedTreeItems[i];
      Object feature = null;
      if( treeElement instanceof LinkedFeatureElement2 )
        feature = ( (LinkedFeatureElement2)treeElement ).getDecoratedFeature();
      else if( treeElement instanceof Feature )
        feature = treeElement;
      if( feature != null && !selectedFeatures.contains( feature ) )
        selectedFeatures.add( feature );
    }
    final Feature[] features = (Feature[])selectedFeatures.toArray( new Feature[selectedFeatures.size()] );
    return features;
  }

  private void createViewerPart()
  {
    final GridLayout layout = new GridLayout();
    layout.numColumns = 1;
    layout.verticalSpacing = 2;
    layout.marginWidth = 0;
    layout.marginHeight = 2;

    // DIRTY! should be set from outside
    m_composite.setLayout( layout );

    m_treeViewer = new TreeViewer( m_composite );
    m_treeViewer.setContentProvider( m_contentProvider );
    m_treeViewer.setLabelProvider( m_labelProvider );
    m_treeViewer.setUseHashlookup( true );

    //add drag and drop support
    int ops = DND.DROP_COPY | DND.DROP_MOVE | DND.DROP_LINK;
    Transfer[] transfers = new Transfer[]
    {
        LocalSelectionTransfer.getInstance(),
        PluginTransfer.getInstance() };
    m_treeViewer.addDragSupport( ops, transfers, new GmlTreeDragListener( this ) );
    transfers = new Transfer[]
    { LocalSelectionTransfer.getInstance() };
    m_dropAdapter = new GmlTreeDropAdapter( this );
    m_treeViewer.addDropSupport( ops, transfers, m_dropAdapter );

    final GridData layoutData = new GridData();
    layoutData.grabExcessHorizontalSpace = true;
    layoutData.grabExcessVerticalSpace = true;
    layoutData.horizontalAlignment = GridData.FILL;
    layoutData.verticalAlignment = GridData.FILL;
    m_treeViewer.getControl().setLayoutData( layoutData );
  }

  protected void hookListeners( final TreeViewer treeViewer )
  {
    treeViewer.addDoubleClickListener( new IDoubleClickListener()
    {
      public void doubleClick( final DoubleClickEvent event )
      {
        if( event.getSelection() instanceof IStructuredSelection )
        {
          final IStructuredSelection selection = (IStructuredSelection)event.getSelection();
          final Object obj = selection.getFirstElement();
          if( obj instanceof LinkedFeatureElement2 )
          {
            final Feature feature = ( (LinkedFeatureElement2)obj ).getDecoratedFeature();
            final StructuredSelection ss = new StructuredSelection( feature );
            treeViewer.setSelection( ss, true );
            treeViewer.expandToLevel( feature, 1 );
            treeViewer.reveal( feature );
          }
        }
      }
    } );
  }

  public void dispose()
  {
    m_composite.dispose();
    m_pool.removePoolListener( this );

    m_selectionManager.removeSelectionListener( m_globalSelectionChangedListener );
    m_treeViewer.removePostSelectionChangedListener( m_treeSelectionChangedListener );
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventListener#onModellChange(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    if( modellEvent instanceof FeatureStructureChangeModellEvent )
    {
      final FeatureStructureChangeModellEvent structureEvent = (FeatureStructureChangeModellEvent)modellEvent;
      final Feature parentFeature = structureEvent.getParentFeature();
      
      if( !m_composite.isDisposed() )
      {
        final TreeViewer treeViewer = m_treeViewer;
        m_composite.getDisplay().asyncExec( new Runnable()
        {
          public void run()
          {
            if( parentFeature == null )
                treeViewer.refresh();
            else
              treeViewer.refresh( parentFeature );
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
        KalypsoGisPlugin.getDefault().getPool().saveObject( m_workspace, monitor );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new CoreException( StatusUtilities.statusFromThrowable( e, "Fehler beim Speichern" ) );
    }
  }

  public CommandableWorkspace getWorkspace()
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

  protected TreeViewer getTreeViewer()
  {
    return m_treeViewer;
  }

  public ISelection getSelection()
  {
    return new TreeFeatureSelection( (IStructuredSelection)m_treeViewer.getSelection() );
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

      m_workspace = ( (CommandableWorkspace)newValue );
      if( m_workspace != null )
        m_workspace.addModellListener( this );

      final TreeViewer treeViewer = m_treeViewer;
      treeViewer.getControl().getDisplay().asyncExec( new Runnable()
      {
        public void run()
        {
          treeViewer.setInput( m_workspace );
        }
      } );
    }
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#objectInvalid(org.kalypso.util.pool.IPoolableObjectType, java.lang.Object)
   */
  public void objectInvalid( final IPoolableObjectType key, final Object oldValue )
  {
  // TODO: release workspace
  }

  protected void loadInput( final Reader r, final URL context, final IProgressMonitor monitor ) throws CoreException
  {
    monitor.beginTask( "Ansicht laden", 1000 );
    try
    {
      final Unmarshaller unmarshaller = m_factory.createUnmarshaller();

      final InputSource is = new InputSource( r );

      final Gistreeview gisTreeview = (Gistreeview)unmarshaller.unmarshal( is );

      final LayerType input = gisTreeview.getInput();
      final String href = input.getHref();
      final String linktype = input.getLinktype();
      input.getFeaturePath();

      m_key = new PoolableObjectType( linktype, href, context );
      m_pool.addPoolListener( this, m_key );
    }
    catch( JAXBException e )
    {
      e.printStackTrace();
      throw new CoreException( StatusUtilities.statusFromThrowable( e, "Fehler beim Lesen der Vorlage" ) );
    }
    finally
    {
      monitor.done();
    }
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

  private final void fireSelectionChanged()
  {
    final ISelectionChangedListener[] listenersArray = (ISelectionChangedListener[])m_selectionListeners
        .toArray( new ISelectionChangedListener[m_selectionListeners.size()] );

    final SelectionChangedEvent e = new SelectionChangedEvent( this, getSelection() );
    for( int i = 0; i < listenersArray.length; i++ )
    {
      final ISelectionChangedListener l = listenersArray[i];
      final SafeRunnable safeRunnable = new SafeRunnable()
      {
        public void run()
        {
          l.selectionChanged( e );
        }
      };

      Platform.run( safeRunnable );
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
      return m_contentProvider.getParentFeature( feature );
    }

    public String getParentFeatureProperty( final Feature feature )
    {
      return m_contentProvider.getParentFeatureProperty( feature );
    }

    /**
     * @see org.kalypso.ogc.gml.selection.IFeatureSelection#getAllFeatures()
     */
    public EasyFeatureWrapper[] getAllFeatures()
    {
      return FeatureSelectionHelper.createEasyWrappers( this );
    }

    /**
     * @see org.kalypso.ogc.gml.selection.IFeatureSelection#getSelectionManager()
     */
    public IFeatureSelectionManager getSelectionManager()
    {
      return m_selectionManager;
    }

    /**
     * @see org.kalypso.ogc.gml.selection.IFeatureSelection#getFocusedFeature()
     */
    public Feature getFocusedFeature()
    {
      // the tree doesn't support focused features
      return null;
    }

    /**
     * @see org.kalypso.ogc.gml.selection.IFeatureSelection#getFocusedProperty()
     */
    public String getFocusedProperty()
    {
      // the tree doesn't support focused features
      return null;
    }
  }

}