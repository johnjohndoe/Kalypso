package org.kalypso.ui.repository.view;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.logging.Logger;

import org.eclipse.compare.Splitter;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.views.properties.IPropertySheetPage;
import org.eclipse.ui.views.properties.PropertySheetEntry;
import org.eclipse.ui.views.properties.PropertySheetPage;
import org.kalypso.eclipse.ui.MementoUtils;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.view.propertySource.ObservationPropertySourceProvider;
import org.kalypso.repository.DefaultRepositoryContainer;
import org.kalypso.repository.IRepository;
import org.kalypso.repository.IRepositoryContainer;
import org.kalypso.repository.IRepositoryContainerListener;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.repository.conf.RepositoryConfigItem;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.repository.actions.AddRepositoryAction;
import org.kalypso.ui.repository.actions.CollapseAllAction;
import org.kalypso.ui.repository.actions.ConfigurePreviewAction;
import org.kalypso.ui.repository.actions.CopyLinkAction;
import org.kalypso.ui.repository.actions.ExpandAllAction;
import org.kalypso.ui.repository.actions.ExportAsFileAction;
import org.kalypso.ui.repository.actions.ReloadAction;
import org.kalypso.ui.repository.actions.RemoveRepositoryAction;
import org.kalypso.util.adapter.IAdaptable;

/**
 * Wird als ZeitreihenBrowser benutzt.
 * 
 * @author schlienger
 */
public class RepositoryExplorerPart extends ViewPart implements IRepositoryContainerListener,
    ISelectionProvider, ISelectionChangedListener
{
  private TreeViewer m_repViewer = null;

  private final DefaultRepositoryContainer m_repContainer;

  private RemoveRepositoryAction m_removeAction = null;

  private ConfigurePreviewAction m_confAction = null;

  private ReloadAction m_reloadAction = null;

  private PropertySheetPage m_propsPage = null;

  private CopyLinkAction m_copyLinkAction = null;

  private IMemento m_memento = null;

  // persistence flags for memento
  private static final String TAG_REPOSITORY = "repository"; //$NON-NLS-1$
  
  private static final String TAG_REPOSITORY_PROPS = "repositoryProperties"; //$NON-NLS-1$

  private static final String TAG_EXPANDED = "expanded"; //$NON-NLS-1$

  private static final String TAG_ELEMENT = "element"; //$NON-NLS-1$

  private static final String TAG_IDENFITIER = "identifier"; //$NON-NLS-1$

  private static final String TAG_REPOSITORIES = "repositories"; //$NON-NLS-1$

  private ExportAsFileAction m_exportAsFileAction;

  /**
   * @see org.eclipse.core.runtime.IAdaptable#getAdapter(java.lang.Class)
   */
  public Object getAdapter( final Class adapter )
  {
    if( adapter == IPropertySheetPage.class )
    {
      // lazy loading
      if( m_propsPage == null || m_propsPage.getControl().isDisposed() )
      {
        // TODO check if this is ok to dispose it when not null
        if( m_propsPage != null )
          m_propsPage.dispose();
        
        // PropertySheetPage erzeugen. Sie wird in das standard PropertySheet
        // von Eclipse dargestellt
        m_propsPage = new PropertySheetPage();

        // eigenes entry mit source provider
        final PropertySheetEntry entry = new PropertySheetEntry();
        entry.setPropertySourceProvider( new ObservationPropertySourceProvider() );

        m_propsPage.setRootEntry( entry );

        m_propsPage.selectionChanged( this, getSelection() );
      }

      return m_propsPage;
    }

    return null;
  }

  public RepositoryExplorerPart()
  {
    m_repContainer = KalypsoGisPlugin.getDefault().getRepositoryContainer();

    m_repContainer.addRepositoryContainerListener( this );
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#dispose()
   */
  public void dispose()
  {
    m_repContainer.removeRepositoryContainerListener( this );

    if( m_removeAction != null )
      m_removeAction.dispose();

    if( m_confAction != null )
      m_confAction.dispose();

    if( m_reloadAction != null )
      m_reloadAction.dispose();
    
    if( m_exportAsFileAction != null )
      m_exportAsFileAction.dispose();

    if( m_repViewer != null )
      removeSelectionChangedListener( this );

    super.dispose();
  }

  public TreeViewer getViewer()
  {
    return m_repViewer;
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  public void createPartControl( final Composite parent )
  {
    final Splitter split = new Splitter( parent, SWT.VERTICAL | SWT.H_SCROLL | SWT.V_SCROLL );

    m_repViewer = new TreeViewer( split, SWT.H_SCROLL | SWT.V_SCROLL );
    m_repViewer.setContentProvider( new RepositoryTreeContentProvider() );
    m_repViewer.setLabelProvider( new LabelProvider() );
    m_repViewer.setInput( m_repContainer );

    initContextMenu();

    final IToolBarManager toolBarManager = getViewSite().getActionBars().getToolBarManager();

    toolBarManager.add( new AddRepositoryAction( this ) );

    toolBarManager.add( new Separator() );

    m_removeAction = new RemoveRepositoryAction( this );
    toolBarManager.add( m_removeAction );

    m_confAction = new ConfigurePreviewAction( this );
    toolBarManager.add( m_confAction );

    m_reloadAction = new ReloadAction( this );
    toolBarManager.add( m_reloadAction );

    toolBarManager.add( new Separator() );

    toolBarManager.add( new CollapseAllAction( this ) );
    toolBarManager.add( new ExpandAllAction( this ) );
    
    toolBarManager.add( new Separator() );
    
    m_exportAsFileAction = new ExportAsFileAction( this );
    toolBarManager.add( m_exportAsFileAction );

    getViewSite().getActionBars().updateActionBars();

    addSelectionChangedListener( this );

    if( m_memento != null )
      restoreState( m_memento );
    m_memento = null;
  }

  /**
   * @see org.eclipse.ui.part.ViewPart#init(org.eclipse.ui.IViewSite, org.eclipse.ui.IMemento)
   */
  public void init( final IViewSite site, final IMemento memento ) throws PartInitException
  {
    super.init( site, memento );
    
    m_memento = memento;
  }
  
  /**
   * Initializes and registers the context menu.
   */
  private void initContextMenu()
  {
    final MenuManager menuMgr = new MenuManager( "#PopupMenu" ); //$NON-NLS-1$
    menuMgr.setRemoveAllWhenShown( true );
    menuMgr.addMenuListener( new IMenuListener()
    {
      public void menuAboutToShow( final IMenuManager manager )
      {
        fillContextMenu( manager );
      }
    } );

    final Menu menu = menuMgr.createContextMenu( m_repViewer.getTree() );
    m_repViewer.getTree().setMenu( menu );
    getSite().registerContextMenu( menuMgr, m_repViewer );

    m_copyLinkAction = new CopyLinkAction( this );
  }

  /**
   * Called when the context menu is about to open.
   * @param menu
   */
  protected void fillContextMenu( final IMenuManager menu )
  {
    if( isObservationSelected( getViewer().getSelection() ) != null )
    {
      menu.add( m_copyLinkAction );
      menu.add( new Separator() );
      menu.add( new Separator( IWorkbenchActionConstants.MB_ADDITIONS ) );
      menu.add( new Separator( IWorkbenchActionConstants.MB_ADDITIONS + "-end" ) );
    }
  }

  /**
   * @param selection
   * @return checks if given selection is a <code>IRepository</code>. Returns a repository or null if no repository is selected.
   */
  public IRepository isRepository( final ISelection selection )
  {
    final IStructuredSelection sel = (IStructuredSelection)selection;
    if( sel.isEmpty() )
      return null;

    final Object element = sel.getFirstElement();
    if( !( element instanceof IRepository ) )
      return null;

    return (IRepository)element;
  }

  /**
   * @param selection
   * @return the IObservation object when the selection is an IAdaptable object
   * that get deliver an IObservation.
   */
  public IObservation isObservationSelected( final ISelection selection )
  {
    final IStructuredSelection sel = (IStructuredSelection)selection;
    if( sel.isEmpty() )
      return null;

    final Object element = sel.getFirstElement();
    if( element instanceof IAdaptable )
    {
      final IObservation obs = (IObservation)( (IAdaptable)element )
          .getAdapter( IObservation.class );

      return obs;
    }

    return null;
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#setFocus()
   */
  public void setFocus()
  {
  // noch nix
  }

  /**
   * @see org.kalypso.repository.IRepositoryContainerListener#onRepositoryContainerChanged()
   */
  public void onRepositoryContainerChanged()
  {
    final Runnable r = new Runnable()
    {
      public void run()
      {
        getViewer().refresh();
      }
    };

    getSite().getShell().getDisplay().asyncExec( r );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#addSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
   */
  public void addSelectionChangedListener( ISelectionChangedListener listener )
  {
    m_repViewer.addSelectionChangedListener( listener );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#getSelection()
   */
  public ISelection getSelection()
  {
    return m_repViewer.getSelection();
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#removeSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
   */
  public void removeSelectionChangedListener( ISelectionChangedListener listener )
  {
    m_repViewer.removeSelectionChangedListener( listener );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#setSelection(org.eclipse.jface.viewers.ISelection)
   */
  public void setSelection( final ISelection selection )
  {
    m_repViewer.setSelection( selection );
  }

  /**
   * Returns 
   * @return the repository container
   */
  public IRepositoryContainer getRepositoryContainer()
  {
    return m_repContainer;
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
   */
  public void selectionChanged( final SelectionChangedEvent event )
  {
    if( m_propsPage != null && !m_propsPage.getControl().isDisposed() )
      m_propsPage.selectionChanged( this, event.getSelection() );
  }

  /**
   * @see org.eclipse.ui.part.ViewPart#saveState(org.eclipse.ui.IMemento)
   */
  public void saveState( final IMemento memento )
  {
    final TreeViewer viewer = getViewer();
    if( viewer == null )
    {
      if( m_memento != null ) //Keep the old state;
        memento.putMemento( m_memento );

      return;
    }

    // save list of repositories
    final IMemento repsMem = memento.createChild( TAG_REPOSITORIES );
    for( final Iterator it = m_repContainer.getRepositories().iterator(); it.hasNext(); )
    {
      final IRepository rep = (IRepository)it.next();

      final IMemento child = repsMem.createChild( TAG_REPOSITORY );
      child.putTextData( new RepositoryConfigItem( rep.getFactory() ).saveState() );
      
      // save properties for that repository
      final IMemento propsMem = child.createChild( TAG_REPOSITORY_PROPS );
      try
      {
        MementoUtils.saveProperties( propsMem, rep.getProperties() );
      }
      catch( IOException e )
      {
        e.printStackTrace();
      }
    }

    // save visible expanded elements
    final Object expandedElements[] = viewer.getVisibleExpandedElements();
    if( expandedElements.length > 0 )
    {
      final IMemento expandedMem = memento.createChild( TAG_EXPANDED );
      for( int i = 0; i < expandedElements.length; i++ )
      {
        if( expandedElements[i] instanceof IRepositoryItem )
        {
          final IMemento elementMem = expandedMem.createChild( TAG_ELEMENT );
          final String id = ( (IRepositoryItem)expandedElements[i] )
          .getIdentifier();
          elementMem.putString( TAG_IDENFITIER, id );
        }
      }
    }
  }

  /**
   * Restores the state of the receiver to the state described in the specified
   * memento.
   * 
   * @param memento
   *          the memento
   */
  private void restoreState( final IMemento memento )
  {
    final TreeViewer viewer = getViewer();

    final IMemento repsMem = memento.getChild( TAG_REPOSITORIES );
    if( repsMem != null )
    {
      final IMemento[] repMem = repsMem.getChildren( TAG_REPOSITORY );
      for( int i = 0; i < repMem.length; i++ )
      {
        if( repMem[i] == null )
          continue;
          
        try
        {
          final RepositoryConfigItem item = RepositoryConfigItem.restore( repMem[i].getTextData() );
          if( item != null )
          {
	          final IRepository rep = item.createFactory( getClass().getClassLoader() ).createRepository();
	          
	          final IMemento propsMem = repMem[i].getChild( TAG_REPOSITORY_PROPS );
	          if( propsMem == null )
	            rep.setProperties( KalypsoGisPlugin.getDefault().getDefaultRepositoryProperties() );
	          else
	            MementoUtils.loadProperties( propsMem, rep.getProperties() );
	          
	          m_repContainer.addRepository( rep );
          }
        }
        catch( Exception e ) // generic exception caught for simplicity
        {
          // ignored
          e.printStackTrace();
        }
      }
    }

    final IMemento childMem = memento.getChild( TAG_EXPANDED );
    if( childMem != null )
    {
      final ArrayList elements = new ArrayList();
      final IMemento[] elementMem = childMem.getChildren( TAG_ELEMENT );
      for( int i = 0; i < elementMem.length; i++ )
      {
        try
        {
          final String id = elementMem[i].getString( TAG_IDENFITIER );
          final Object element = m_repContainer.findItem( id );
          
          if( element != null )
            elements.add( element );
          else
            Logger.getLogger( getClass().getName() ).warning( "Restoring GUI State for observation explorer part: could not find item " + id );
        }
        catch( NoSuchElementException e )
        {
          // ignored
        }
      }
      
      viewer.setExpandedElements( elements.toArray() );
    }
  }
}