package org.kalypso.ui.repository.view;

import org.eclipse.compare.Splitter;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.views.properties.IPropertySheetPage;
import org.eclipse.ui.views.properties.PropertySheetEntry;
import org.eclipse.ui.views.properties.PropertySheetPage;
import org.kalypso.ogc.sensor.view.propertySource.ObservationPropertySourceProvider;
import org.kalypso.repository.DefaultRepositoryContainer;
import org.kalypso.repository.IRepository;
import org.kalypso.repository.IRepositoryContainer;
import org.kalypso.repository.IRepositoryContainerListener;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.repository.actions.AddRepositoryAction;
import org.kalypso.ui.repository.actions.ConfigurePreviewAction;
import org.kalypso.ui.repository.actions.ReloadAction;
import org.kalypso.ui.repository.actions.RemoveRepositoryAction;

/**
 * Wird als ZeitreihenBrowser benutzt.
 * 
 * @author schlienger
 */
public class RepositoryExplorerPart extends ViewPart implements IRepositoryContainerListener,
    ISelectionProvider, ISelectionChangedListener
{
  protected TreeViewer m_repViewer = null;

  private final DefaultRepositoryContainer m_repContainer;

  private RemoveRepositoryAction m_removeAction = null;
  private ConfigurePreviewAction m_confAction = null;
  private ReloadAction m_reloadAction = null;

  private PropertySheetPage m_propsPage = null;



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
        // PropertySheetPage erzeugen. Sie wird in das standard PropertySheet von Eclipse dargestellt
        m_propsPage  = new PropertySheetPage();

        // eigenes entry mit source provider
        final PropertySheetEntry entry = new PropertySheetEntry();
        entry.setPropertySourceProvider( new ObservationPropertySourceProvider() );

        m_propsPage.setRootEntry( entry );
        
        m_propsPage.selectionChanged(this, getSelection() );
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

    if( m_repViewer != null )
      removeSelectionChangedListener( this );
    
    super.dispose();
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  public void createPartControl( Composite parent )
  {
    final Splitter split = new Splitter( parent, SWT.VERTICAL | SWT.H_SCROLL | SWT.V_SCROLL );

    m_repViewer = new TreeViewer( split, SWT.H_SCROLL | SWT.V_SCROLL );
    m_repViewer.setContentProvider( new RepositoryTreeContentProvider() );
    m_repViewer.setLabelProvider( new LabelProvider() );
    m_repViewer.setInput( m_repContainer );

    final Shell shell = getSite().getShell();

    final IToolBarManager toolBarManager = getViewSite().getActionBars().getToolBarManager();
    
    toolBarManager.add( new AddRepositoryAction( shell, m_repContainer ) );
    
    m_removeAction = new RemoveRepositoryAction( shell, this );
    toolBarManager.add( m_removeAction );
    
    m_confAction = new ConfigurePreviewAction( shell, this );
    toolBarManager.add( m_confAction );
    
    m_reloadAction = new ReloadAction( shell, this );
    toolBarManager.add( m_reloadAction );

    getViewSite().getActionBars().updateActionBars();

    addSelectionChangedListener( this );
  }

  /**
   * Utility method that checks if given selection is a <code>IRepository</code>.
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
        m_repViewer.refresh();
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
  public void saveState( IMemento memento )
  {
    super.saveState( memento );
    
    // TODO: save gui state (See ResourceNavigator)
  }
}
