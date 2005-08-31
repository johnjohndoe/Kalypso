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
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.IPostSelectionProvider;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
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
import org.kalypso.contribs.eclipse.jface.viewers.KalypsoSelectionChangedEvent;
import org.kalypso.contribs.eclipse.jface.viewers.SelectionProviderAdapter;
import org.kalypso.ogc.gml.command.SelectFeaturesCommand;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.CommandableFeatureSelection;
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
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventListener;
import org.kalypsodeegree.model.feature.event.ModellEventProvider;
import org.kalypsodeegree.model.feature.event.ModellEventProviderAdapter;
import org.kalypsodeegree_impl.model.feature.selection.IFeatureSelectionManager;
import org.xml.sax.InputSource;

/**
 * Is among others a {@link org.eclipse.jface.viewers.IPostSelectionProvider}. The returned selection consists of
 * {@link org.kalypsodeegree.model.feature.Feature}and
 * {@link org.kalypsodeegree.model.feature.FeatureAssociationTypeProperty}objects. Changing the selection from outside
 * is not supported (todo).
 * 
 * @author <verschiedene>
 */
public class GmlTreeView extends SelectionProviderAdapter implements IPostSelectionProvider, IPoolListener,
    ModellEventProvider, ModellEventListener
{
  protected GMLEditorLabelProvider2 m_labelProvider = new GMLEditorLabelProvider2();

  protected GMLEditorContentProvider2 m_contentProvider = new GMLEditorContentProvider2();

  protected CommandableWorkspace m_workspace = null;

  protected final Composite m_composite;

  private final ModellEventProviderAdapter m_eventProvider = new ModellEventProviderAdapter();

  private PoolableObjectType m_key = null;

  private final ResourcePool m_pool = KalypsoGisPlugin.getDefault().getPool();

  private final ObjectFactory m_factory = new ObjectFactory();

  private final Unmarshaller m_unmarshaller;

  private TreeViewer m_treeViewer = null;

  private final ISelectionChangedListener m_workspaceSelectionChangedListener;

  final ISelectionChangedListener m_treeSelectionChangedListener = new ISelectionChangedListener()
  {
    public void selectionChanged( final SelectionChangedEvent event )
    {
      handleTreeSelectionChanged( event );
    }
  };

  private GmlTreeDropAdapter m_dropAdapter;

  public GmlTreeView( final Composite composite )
  {
    m_composite = composite;
    try
    {
      m_unmarshaller = m_factory.createUnmarshaller();
    }
    catch( final JAXBException e )
    {
      e.printStackTrace();
      throw new RuntimeException( e );
    }

    createViewerPart();

    hookListeners( m_treeViewer );

    m_workspaceSelectionChangedListener = new ISelectionChangedListener()
    {
      public void selectionChanged( final SelectionChangedEvent event )
      {
        handleWorkspaceSelectionChanged( event );
      }
    };

    // als post selection listener anmelden,
    // dann passiert das ganze gedöns nicht immer sofort
    m_treeViewer.addPostSelectionChangedListener( m_treeSelectionChangedListener );
  }

  protected void handleWorkspaceSelectionChanged( final SelectionChangedEvent event )
  {
    final TreeViewer treeViewer = m_treeViewer;

    if( event instanceof KalypsoSelectionChangedEvent )
    {
      final KalypsoSelectionChangedEvent kalypsoEvent = (KalypsoSelectionChangedEvent)event;
      final Object selectionInvoker = kalypsoEvent.getSelectionInvoker();
      if( selectionInvoker != null && !selectionInvoker.equals( GmlTreeView.this ) )
      {
        treeViewer.getControl().getDisplay().syncExec( new Runnable()
        {
          public void run()
          {
            treeViewer.setSelection( event.getSelection(), true );
          }
        } );
      }
    }

    // Inform selection-listeners of the GmlTreeView
    setSelection( event.getSelection() );
  }

  protected void handleTreeSelectionChanged( final SelectionChangedEvent event )
  {
    ISelection selection = event.getSelection();
    if( selection instanceof IStructuredSelection )
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
      final Object input = m_treeViewer.getInput();
      if( input instanceof CommandableWorkspace )
      {
        final CommandableWorkspace workspace = (CommandableWorkspace)input;
        final Feature[] features = (Feature[])selectedFeatures.toArray( new Feature[selectedFeatures.size()] );
        final SelectFeaturesCommand command = new SelectFeaturesCommand( workspace, features, workspace
            .getSelectionManager(), GmlTreeView.this );
        try
        {
          workspace.postCommand( command );
        }
        catch( final Exception ex )
        {
          ex.printStackTrace();
        }
      }
    }
  }

  private void createViewerPart()
  {
    final GridLayout layout = new GridLayout();
    layout.numColumns = 1;
    layout.verticalSpacing = 2;
    layout.marginWidth = 0;
    layout.marginHeight = 2;
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
    if( m_workspace != null )
      m_workspace.getSelectionManager().removeSelectionChangedListener( m_workspaceSelectionChangedListener );
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventListener#onModellChange(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
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

  public TreeViewer getTreeViewer()
  {
    return m_treeViewer;
  }

  public ISelection getSelection()
  {
    if( m_workspace == null )
      return super.getSelection();
    final IFeatureSelectionManager selectionManager = m_workspace.getSelectionManager();
    final IStructuredSelection ss = selectionManager.getStructuredSelection();
    return new CommandableFeatureSelection( m_workspace, this, ss, null, null );
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
        m_workspace.getSelectionManager().removeSelectionChangedListener( m_workspaceSelectionChangedListener );
        m_workspace = null;
      }
      m_workspace = ( (CommandableWorkspace)newValue );
      m_workspace.addModellListener( this );
      m_workspace.getSelectionManager().addSelectionChangedListener( m_workspaceSelectionChangedListener );

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
      final InputSource is = new InputSource( r );

      final Gistreeview gisTreeview = (Gistreeview)m_unmarshaller.unmarshal( is );

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
}