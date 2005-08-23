package org.kalypso.ui.editor.gmleditor.ui;

import java.io.Reader;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.IPostSelectionProvider;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.viewers.SelectionProviderAdapter;
import org.kalypso.ogc.gml.command.SelectFeaturesCommand;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.CommandableFeatureSelection;
import org.kalypso.template.gistreeview.Gistreeview;
import org.kalypso.template.gistreeview.ObjectFactory;
import org.kalypso.template.types.LayerType;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.editor.gmleditor.util.Clipboard;
import org.kalypso.ui.editor.gmleditor.util.GMLReader;
import org.kalypso.ui.editor.gmleditor.util.actions.AddFeatureAction;
import org.kalypso.ui.editor.gmleditor.util.actions.AddLinkAction;
import org.kalypso.ui.editor.gmleditor.util.actions.CopyFeatureAction;
import org.kalypso.ui.editor.gmleditor.util.actions.EditFeatureAction;
import org.kalypso.ui.editor.gmleditor.util.actions.PasteFeatureAction;
import org.kalypso.ui.editor.gmleditor.util.command.MoveFeatureCommand;
import org.kalypso.ui.editor.gmleditor.util.model.FeatureElement;
import org.kalypso.ui.editor.gmleditor.util.model.GMLDocumentEvent;
import org.kalypso.ui.editor.gmleditor.util.model.IGMLDocumentListener;
import org.kalypso.ui.editor.gmleditor.util.model.IModel;
import org.kalypso.ui.editor.gmleditor.util.model.LinkedFeatureElement;
import org.kalypso.ui.editor.gmleditor.util.model.Model;
import org.kalypso.ui.editor.gmleditor.util.model.PropertyElement;
import org.kalypso.ui.editor.gmleditor.util.model.visitors.FindDataElementsVisitor;
import org.kalypso.util.pool.IPoolListener;
import org.kalypso.util.pool.IPoolableObjectType;
import org.kalypso.util.pool.KeyComparator;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.util.pool.ResourcePool;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureSelectionChangedModellEvent;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
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
public class GMLEditorTreeView extends SelectionProviderAdapter implements IGMLDocumentListener, // 
    IPostSelectionProvider, IPoolListener, ModellEventProvider, ModellEventListener//, ISelectionChangedListener
{
  protected GMLEditorLabelProvider2 m_labelProvider = new GMLEditorLabelProvider2();

  protected GMLEditorContentProvider2 m_contentProvider = new GMLEditorContentProvider2();

  protected FeatureElement m_root;

  protected CommandableWorkspace m_workspace = null;

  protected final Composite m_composite;

  protected Action deleteFeatureAction = null;

  protected GMLReader m_reader = null;

  protected Action moveFeatureUpAction = null;

  protected Action moveFeatureDownAction = null;

  protected Action addFeatureActions[] = null;

  protected Action addLinkActions[] = null;

  protected Action copyFeatureAction = null;

  protected Action editFeatureAction = null;

  protected Action pasteFeatureAction = null;

  protected Clipboard clipboard = new Clipboard();

  private final ModellEventProviderAdapter m_eventProvider = new ModellEventProviderAdapter();

  private PoolableObjectType m_key;

  private final ResourcePool m_pool = KalypsoGisPlugin.getDefault().getPool();

  private final ObjectFactory m_factory = new ObjectFactory();

  private final Marshaller m_marshaller;

  private final Unmarshaller m_unmarshaller;

  public final static String INTERNAL = "INTERNAL";

  public final static String EXTERNAL = "EXTERNAL";

  private String m_selectionSource = null;

  private IFeatureSelectionManager m_workspaceSelectionManager;

  private TreeViewer m_treeViewer;

  public GMLEditorTreeView( final Composite composite )
  {
    m_composite = composite;
    try
    {
      m_marshaller = m_factory.createMarshaller();
      m_unmarshaller = m_factory.createUnmarshaller();
      m_marshaller.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );

    }
    catch( JAXBException e )
    {
      e.printStackTrace();
      throw new RuntimeException( e );
    }

    setUpTreeViewer();

    hookListeners();
    //    createActions();
    //    createMenu( m_treeViewer.getControl() );
  }

  private void setUpTreeViewer()
  {
    final GridLayout layout = new GridLayout();
    layout.numColumns = 1;
    layout.verticalSpacing = 2;
    layout.marginWidth = 0;
    layout.marginHeight = 2;
    m_composite.setLayout( layout );

    m_treeViewer = new TreeViewer( m_composite );
    m_treeViewer.setContentProvider( m_contentProvider );
    //    setContentProvider( m_contentProvider );
    m_treeViewer.setLabelProvider( m_labelProvider );
    //    setLabelProvider( m_labelProvider );
    m_treeViewer.setUseHashlookup( true );
    //    setUseHashlookup( true );

    final GridData layoutData = new GridData();
    layoutData.grabExcessHorizontalSpace = true;
    layoutData.grabExcessVerticalSpace = true;
    layoutData.horizontalAlignment = GridData.FILL;
    layoutData.verticalAlignment = GridData.FILL;
    m_treeViewer.getControl().setLayoutData( layoutData );
    //    getControl().setLayoutData( layoutData );
  }

  protected void createActions()
  {
    //    deleteFeatureAction = new Action( "L�schen" )
    //    {
    //      public void run()
    //      {
    //        if( m_treeViewer.getSelection().isEmpty() )
    //          return;
    //        IStructuredSelection selection = (IStructuredSelection)m_treeViewer.getSelection();
    //        for( Iterator iterator = selection.iterator(); iterator.hasNext(); )
    //        {
    //          Model model = (Model)iterator.next();
    //
    //          if( model instanceof FeatureElement || model instanceof LinkedFeatureElement )
    //          {
    //            Object childItem = null;
    //            Feature parentFeature = null;
    //
    //            if( model instanceof FeatureElement )
    //            {
    //              childItem = ( (FeatureElement)model ).getFeature();
    //              parentFeature = ( (FeatureElement)model.getParent().getParent() ).getFeature();
    //            }
    //            else
    //            {
    //              childItem = ( (LinkedFeatureElement)model ).getName();
    //              parentFeature = ( (FeatureElement)model.getParent().getParent() ).getFeature();
    //            }
    //            DeleteFeatureCommand command = new DeleteFeatureCommand( m_workspace, parentFeature,
    //                ( (PropertyElement)model.getParent() ).getProperty().getName(), childItem );
    //            try
    //            {
    //              m_workspace.postCommand( command );
    //              //m_gmlEditor.postCommand(command, null);
    //              return;
    //            }
    //            catch( Exception e )
    //            {
    //              e.printStackTrace();
    //            }
    //          }
    //        }
    //      }
    //    };

    moveFeatureUpAction = new Action( "Nach oben verschieben" )
    {
      public void run()
      {
        if( getSelection().isEmpty() )
          return;
        IStructuredSelection selection = (IStructuredSelection)getSelection();
        for( Iterator iterator = selection.iterator(); iterator.hasNext(); )
        {
          Model model = (Model)iterator.next();

          if( model instanceof FeatureElement || model instanceof LinkedFeatureElement )
          {
            Object childItem = null;
            Feature parentFeature = null;

            if( model instanceof FeatureElement )
            {
              childItem = ( (FeatureElement)model ).getFeature();
              parentFeature = ( (FeatureElement)model.getParent().getParent() ).getFeature();
            }
            else
            {
              childItem = ( (LinkedFeatureElement)model ).getName();
              parentFeature = ( (FeatureElement)model.getParent().getParent() ).getFeature();
            }

            MoveFeatureCommand command = new MoveFeatureCommand( m_workspace, parentFeature, ( (PropertyElement)model
                .getParent() ).getProperty().getName(), childItem, MoveFeatureCommand.UP );
            try
            {
              m_workspace.postCommand( command );
              return;
            }
            catch( Exception e )
            {
              e.printStackTrace();
            }
          }
        }
      }
    };

    moveFeatureDownAction = new Action( "Nach unten verschieben" )
    {
      public void run()
      {
        if( getSelection().isEmpty() )
          return;
        IStructuredSelection selection = (IStructuredSelection)getSelection();
        for( Iterator iterator = selection.iterator(); iterator.hasNext(); )
        {
          Model model = (Model)iterator.next();

          if( model instanceof FeatureElement || model instanceof LinkedFeatureElement )
          {
            Object childItem = null;
            Feature parentFeature = null;

            if( model instanceof FeatureElement )
            {
              childItem = ( (FeatureElement)model ).getFeature();
              parentFeature = ( (FeatureElement)model.getParent().getParent() ).getFeature();
            }
            else
            {
              childItem = ( (LinkedFeatureElement)model ).getName();
              parentFeature = ( (FeatureElement)model.getParent().getParent() ).getFeature();
            }

            MoveFeatureCommand command = new MoveFeatureCommand( m_workspace, parentFeature, ( (PropertyElement)model
                .getParent() ).getProperty().getName(), childItem, MoveFeatureCommand.DOWN );

            try
            {
              m_workspace.postCommand( command );
              return;
            }
            catch( Exception e )
            {
              e.printStackTrace();
            }
          }
        }
      }
    };
  }

  protected void createMenu( final Control control )
  {
    final MenuManager rootMenuManager = new MenuManager( "#PopUp" );
    rootMenuManager.setRemoveAllWhenShown( true );
    rootMenuManager.addMenuListener( new IMenuListener()
    {
      public void menuAboutToShow( IMenuManager mgr )
      {
        mgr.add( new Separator( IWorkbenchActionConstants.MB_ADDITIONS ) );
        if( editFeatureAction != null )
          mgr.add( editFeatureAction );
        mgr.add( deleteFeatureAction );
        mgr.add( moveFeatureUpAction );
        mgr.add( moveFeatureDownAction );
        if( addFeatureActions != null )
        {
          for( int i = 0; i < addFeatureActions.length; i++ )
            mgr.add( addFeatureActions[i] );
        }
        if( addLinkActions != null )
        {
          for( int i = 0; i < addLinkActions.length; i++ )
            mgr.add( addLinkActions[i] );
        }
        if( copyFeatureAction != null )
          mgr.add( copyFeatureAction );
        if( pasteFeatureAction != null )
          mgr.add( pasteFeatureAction );
      }
    } );
    final Menu menu = rootMenuManager.createContextMenu( control );
    control.setMenu( menu );
  }

  protected void hookListeners()
  {
    m_treeViewer.addDoubleClickListener( new IDoubleClickListener() //m_treeViewer
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
                m_treeViewer.setSelection( ss, true );
                //            setSelection( ss, true );
                m_treeViewer.expandToLevel( feature, 1 );
                //            expandToLevel( feature, 1 );
                m_treeViewer.reveal( feature );
                //            reveal( feature );
              }
            }
          }
        } );

    m_treeViewer.addSelectionChangedListener( new ISelectionChangedListener() //m_treeViewer.
        {

          public void selectionChanged( final SelectionChangedEvent event )
          {
            //        onTreeSelectionChanged( event );
            if( m_selectionSource == null )
              m_selectionSource = INTERNAL;
            else if( m_selectionSource.equals( EXTERNAL ) )
            {
              m_selectionSource = INTERNAL;
              return;
            }
            ISelection selection = event.getSelection();

            if( selection instanceof IStructuredSelection )
            {
              Object input = m_treeViewer.getInput();
              //          Object input = getInput();
              Object[] selectedTreeItems = ( (IStructuredSelection)selection ).toArray();
              ArrayList selectedFeatures = new ArrayList();
              for( int i = 0; i < selectedTreeItems.length; i++ )
              {
                Object treeElement = selectedTreeItems[i];
                //                if( treeElement instanceof FeatureAssociationTypeElement )
                //                  selectedFeatures.add( ( (FeatureAssociationTypeElement)treeElement ).getAssociationTypeProperty() );
                if( treeElement instanceof LinkedFeatureElement2 )
                  selectedFeatures.add( ( (LinkedFeatureElement2)treeElement ).getDecoratedFeature() );
                else if( treeElement instanceof GMLWorkspace )
                  selectedFeatures.add( ( (GMLWorkspace)treeElement ).getRootFeature() );
                else if( treeElement instanceof Feature )
                  selectedFeatures.add( treeElement );
              }
              if( input instanceof GMLWorkspace )
              {
                final IFeatureSelectionManager selectionManager = ( (GMLWorkspace)input ).getSelectionManager();
                final CommandableWorkspace workspace = (CommandableWorkspace)input;
                final Feature[] features = (Feature[])selectedFeatures.toArray( new Feature[selectedFeatures.size()] );
                final SelectFeaturesCommand command = new SelectFeaturesCommand( workspace, features, selectionManager,
                    ModellEvent.TREE_SELECTION_CHANGED );
                try
                {
                  m_selectionSource = INTERNAL;
                  workspace.postCommand( command );
                }
                catch( Exception e )
                {
                  e.printStackTrace();
                }
              }
            }
          }
        } );

    //    m_treeViewer.addPostSelectionChangedListener( new ISelectionChangedListener()
    //    {
    //      public void selectionChanged( final SelectionChangedEvent event )
    //      {
    //        firePostSelectionChanged();
    //      }
    //    } );

  }

  protected void onTreeSelectionChanged( final SelectionChangedEvent event )
  {
    // first inform my listeners
    final IStructuredSelection selection = (IStructuredSelection)event.getSelection();
    final ISelection unwrappedSelection = new CommandableFeatureSelection( getWorkspace(), new StructuredSelection(
        getDataFromElements( selection.toArray() ) ), null, null );
    setSelection( unwrappedSelection );
    // now update actions

    final Object obj = selection.getFirstElement();
    if( obj instanceof PropertyElement )
    {
      deleteFeatureAction.setEnabled( false );
      moveFeatureUpAction.setEnabled( false );
      moveFeatureDownAction.setEnabled( false );

      FeatureType types[] = ( (PropertyElement)obj ).getProperty().getAssociationFeatureTypes();
      Feature parentFeature = ( (FeatureElement)( (PropertyElement)obj ).getParent() ).getFeature();

      addFeatureActions = new Action[types.length];
      addLinkActions = new Action[types.length];
      for( int i = 0; i < types.length; i++ )
      {
        addFeatureActions[i] = new AddFeatureAction( types[i], m_workspace, parentFeature, ( (PropertyElement)obj )
            .getProperty().getName(), 0, m_composite.getShell() );
        addLinkActions[i] = new AddLinkAction( types[i], m_workspace, parentFeature, ( (PropertyElement)obj )
            .getProperty().getName(), 0, m_composite.getShell() );
      }

      pasteFeatureAction = new PasteFeatureAction( m_workspace, parentFeature, ( (PropertyElement)obj ).getProperty()
          .getName(), clipboard );

      if( clipboard.getClipboardFeature() != null )
        pasteFeatureAction.setEnabled( true );
      else
        pasteFeatureAction.setEnabled( false );

      if( copyFeatureAction != null )
        copyFeatureAction.setEnabled( false );
      if( editFeatureAction != null )
        editFeatureAction.setEnabled( false );
    }
    else
    {
      deleteFeatureAction.setEnabled( true );
      moveFeatureUpAction.setEnabled( true );
      moveFeatureDownAction.setEnabled( true );
      addFeatureActions = null;
      addLinkActions = null;
      if( copyFeatureAction != null )
        copyFeatureAction.setEnabled( false );
      if( editFeatureAction != null )
        editFeatureAction.setEnabled( false );
      if( obj instanceof FeatureElement )
      {
        editFeatureAction = new EditFeatureAction( m_workspace, m_reader, m_composite.getShell() );
        editFeatureAction.setEnabled( true );
        copyFeatureAction = new CopyFeatureAction( ( (FeatureElement)obj ).getFeature(), m_workspace, clipboard );
        copyFeatureAction.setEnabled( true );
      }
      if( pasteFeatureAction != null )
        pasteFeatureAction.setEnabled( false );
    }
  }

  protected FeatureElement findFeatureElement( final LinkedFeatureElement lfe )
  {
    return findFeatureElement( ( (FeatureElement)m_root.getChildren()[0] ), lfe.getName() );
  }

  private FeatureElement findFeatureElement( final FeatureElement root, final String searchString )
  {
    if( root.getName().equals( searchString ) )
      return root;

    Object[] pe = root.getChildren(); // PropertyElement[]
    for( int i = 0; i < pe.length; i++ )
    {
      Object[] model = ( (PropertyElement)pe[i] ).getChildren();
      for( int j = 0; j < model.length; j++ )
      {
        if( model[j] instanceof FeatureElement )
        {
          FeatureElement fe = findFeatureElement( (FeatureElement)model[j], searchString );
          if( fe != null )
            return fe;
        }
      }
    }
    return null;
  }

  /**
   * @see org.kalypso.ui.editor.gmleditor.util.model.IGMLDocumentListener#onChange(org.kalypso.ui.editor.gmleditor.util.model.GMLDocumentEvent)
   */
  public void onChange( GMLDocumentEvent event )
  {
  //    m_root = event.getGmlDocument();
  //    if( m_workspace != null )
  //      m_workspace.removeModellListener( this );
  //    m_workspace = event.getWorkspace();
  //    if( m_workspace == null )
  //      return;
  //
  //    m_workspace.addModellListener( this );
  //
  //    if( m_composite == null || m_composite.isDisposed() )
  //      return;
  //
  //    m_composite.getDisplay().asyncExec( new Runnable()
  //    {
  //      public void run()
  //      {
  //        m_treeViewer.getTree().setVisible( false );
  //        m_treeViewer.setInput( m_root );
  //        // need to expand in order to load all elements into the treeviewers
  //        // cache
  //        m_treeViewer.expandAll();
  //        m_treeViewer.collapseAll();
  //
  //        m_treeViewer.expandToLevel( DEFAULT_EXPANSION_LEVEL );
  //        m_treeViewer.getTree().setVisible( true );
  //
  //        fireModellEvent( new ModellEvent( GMLEditorTreeView.this, ModellEvent.FULL_CHANGE ) );
  //      }
  //    } );

  }

  public void dispose()
  {
    m_composite.dispose();
    m_pool.removePoolListener( this );
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventListener#onModellChange(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    if( modellEvent instanceof FeatureSelectionChangedModellEvent )
    {
      FeatureSelectionChangedModellEvent event = (FeatureSelectionChangedModellEvent)modellEvent;
      if( event.isType( ModellEvent.TREE_SELECTION_CHANGED ) )
        return;
      if( event.isType( ModellEvent.SELECTION_CHANGED ) )
        m_treeViewer.getControl().getDisplay().asyncExec( new Runnable()
        {

          public void run()
          {
            //TODO dies ist nur ein hack um eine endlos schleife mit der selectionChanged Methode zu
            //vermeiden evtuel mit TreeSelection zu ersezten
            m_selectionSource = EXTERNAL;
            final CommandableFeatureSelection selection = new CommandableFeatureSelection( m_workspace, m_workspace
                .getSelectionManager().getStructuredSelection(), null, null );
            m_treeViewer.setSelection( selection, true );
          }
        } );
    }

    if( modellEvent instanceof FeatureStructureChangeModellEvent )
    {
      Feature parentFeature = ( (FeatureStructureChangeModellEvent)modellEvent ).getParentFeature();
      m_treeViewer.update( parentFeature, null );
      m_treeViewer.refresh();
    }
    //    if( modellEvent.getEventSource() instanceof CommandableWorkspace )
    //    {
    //      if( !m_composite.isDisposed() )
    //        m_composite.getDisplay().asyncExec( new Runnable()
    //        {
    //          public void run()
    //          {
    // das selektierte Feature merken und dann wieder anzeigen
    //            final Object[] expandedElements = m_treeViewer.getExpandedElements();
    //            final Object[] expandedData = getDataFromElements( expandedElements );
    //
    //            final IStructuredSelection selection = (IStructuredSelection)m_treeViewer.getSelection();
    //            final Object[] selecteddata = getDataFromElements( selection.toArray() );
    //
    //            final FeatureElement root = GMLReader.getGMLDocument( (CommandableWorkspace)modellEvent.getEventSource()
    // );
    //            if( root != m_root )
    //            {
    //              m_root = root;
    //              m_treeViewer.setInput( root );
    //
    //              m_treeViewer.setExpandedElements( findDataElements( expandedData ) );
    //              m_treeViewer.setSelection( new StructuredSelection( findDataElements( selecteddata ) ) );
    //            }
    //
    //            fireModellEvent( modellEvent );
    //          }
    //        } );
    //    }
  }

  protected static Object[] getDataFromElements( final Object[] elements )
  {
    final Collection data = new LinkedList();
    for( int i = 0; i < elements.length; i++ )
    {
      final Object object = elements[i];
      if( object instanceof FeatureElement )
        data.add( ( (FeatureElement)object ).getFeature() );
      else if( object instanceof PropertyElement )
        data.add( ( (PropertyElement)object ).getProperty() );
    }

    return data.toArray();
  }

  protected IModel[] findDataElements( final Object[] data )
  {
    final FindDataElementsVisitor visitor = new FindDataElementsVisitor( data );
    m_root.accept( visitor );
    return visitor.getResults();
  }

  public void saveData( final IProgressMonitor monitor ) throws CoreException
  {
    m_reader.saveFeatures( monitor );
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
    return new CommandableFeatureSelection( m_workspace, ss, null, null );
  }

  /**
   * @param workspace
   */
  //  public void setInput( GMLWorkspace workspace )
  //  {
  //    m_treeViewer.setInput( workspace );
  //  }
  /**
   * @see org.kalypso.util.pool.IPoolListener#objectLoaded(org.kalypso.util.pool.IPoolableObjectType, java.lang.Object,
   *      org.eclipse.core.runtime.IStatus)
   */
  public void objectLoaded( IPoolableObjectType key, Object newValue, IStatus status )
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
      {
        m_workspace.addModellListener( this );
        m_treeViewer.getControl().getDisplay().asyncExec( new Runnable()
        {
          public void run()
          {
            m_treeViewer.setInput( m_workspace );
          }
        } );
      }
    }
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#objectInvalid(org.kalypso.util.pool.IPoolableObjectType, java.lang.Object)
   */
  public void objectInvalid( IPoolableObjectType key, Object oldValue )
  {
  // TODO Auto-generated method stub

  }

  /**
   * @param r
   * @param context
   * @param monitor
   * @param properties
   * @throws CoreException
   */
  protected void loadInput( Reader r, URL context, IProgressMonitor monitor ) throws CoreException
  {

    monitor.beginTask( "Ansicht laden", 1000 );
    try
    {
      final InputSource is = new InputSource( r );

      Gistreeview gisTreeview = (Gistreeview)m_unmarshaller.unmarshal( is );

      LayerType input = gisTreeview.getInput();
      String href = input.getHref();
      String linktype = input.getLinktype();
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

  //  /**
  //   * @see
  // org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
  //   */
  //  public void selectionChanged( SelectionChangedEvent event )
  //  {
  //    // onTreeSelectionChanged( event );
  //    if( m_selectionSource == null )
  //      m_selectionSource = INTERNAL;
  //    else if( m_selectionSource.equals( EXTERNAL ) )
  //    {
  //      m_selectionSource = INTERNAL;
  //      return;
  //    }
  //    ISelection selection = event.getSelection();
  //
  //    if( selection instanceof IStructuredSelection )
  //    {
  //// Object input = m_treeViewer.getInput();
  //      Object input = getInput();
  //      Object[] selectedTreeItems = ( (IStructuredSelection)selection ).toArray();
  //      ArrayList selectedFeatures = new ArrayList();
  //      for( int i = 0; i < selectedTreeItems.length; i++ )
  //      {
  //        Object treeElement = selectedTreeItems[i];
  //        if( treeElement instanceof FeatureAssociationTypeElement )
  //          selectedFeatures.add( ( (FeatureAssociationTypeElement)treeElement ).getParentFeature() );
  //        else if( treeElement instanceof LinkedFeatureElement2 )
  //          selectedFeatures.add( ( (LinkedFeatureElement2)treeElement ).getDecoratedFeature() );
  //        else if( treeElement instanceof GMLWorkspace )
  //          selectedFeatures.add( ( (GMLWorkspace)treeElement ).getRootFeature() );
  //        else if( treeElement instanceof Feature )
  //          selectedFeatures.add( treeElement );
  //      }
  //      if( input instanceof GMLWorkspace )
  //      {
  //        final IFeatureSelectionManager selectionManager = ( (GMLWorkspace)input ).getSelectionManager();
  //        final CommandableWorkspace workspace = (CommandableWorkspace)input;
  //        final Feature[] features = (Feature[])selectedFeatures.toArray( new Feature[selectedFeatures.size()] );
  //        final SelectFeaturesCommand command = new SelectFeaturesCommand( workspace, features, selectionManager,
  //            ModellEvent.TREE_SELECTION_CHANGED );
  //        try
  //        {
  //          m_selectionSource = INTERNAL;
  //          workspace.postCommand( command );
  //        }
  //        catch( Exception e )
  //        {
  //          e.printStackTrace();
  //        }
  //      }
  //    }
  //    
  //  }

}