package org.kalypso.util.repository.view;

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
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.plugin.KalypsoGisPlugin;
import org.kalypso.util.repository.DefaultRepositoryContainer;
import org.kalypso.util.repository.IRepositoryContainer;
import org.kalypso.util.repository.IRepositoryContainerListener;
import org.kalypso.util.repository.action.AddRepositoryAction;
import org.kalypso.util.repository.action.RemoveRepositoryAction;

/**
 * 
 * 
 * @author schlienger
 */
public class RepositoryExplorerPart extends ViewPart implements IRepositoryContainerListener,
    ISelectionProvider, ISelectionChangedListener
{
  private TreeViewer m_repViewer = null;

  private Label m_metaViewer = null;

  private final DefaultRepositoryContainer m_repContainer;

  private RemoveRepositoryAction m_removeAction;

  public RepositoryExplorerPart()
  {
    super();

    m_repContainer = KalypsoGisPlugin.getDefault().getRepositoryContainer();

    m_repContainer.addRepositoryContainerListener( this );
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#dispose()
   */
  public void dispose()
  {
    super.dispose();

    m_repContainer.removeRepositoryContainerListener( this );

    if( m_removeAction != null )
      m_removeAction.dispose();

    if( m_repViewer != null )
      removeSelectionChangedListener( this );
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  public void createPartControl( Composite parent )
  {
    Splitter split = new Splitter( parent, SWT.VERTICAL | SWT.H_SCROLL | SWT.V_SCROLL );

    m_repViewer = new TreeViewer( split, SWT.H_SCROLL | SWT.V_SCROLL );
    m_repViewer.setContentProvider( new RepositoryTreeContentProvider() );
    m_repViewer.setLabelProvider( new LabelProvider() );
    m_repViewer.setInput( m_repContainer );

    m_metaViewer = new Label( split, SWT.CENTER );
    m_metaViewer.setText( "Metadata..." );

    final Shell shell = getSite().getShell();
    m_removeAction = new RemoveRepositoryAction( shell, this );

    final IToolBarManager toolBarManager = getViewSite().getActionBars().getToolBarManager();
    toolBarManager.add( new AddRepositoryAction( shell, m_repContainer ) );
    toolBarManager.add( m_removeAction );

    getViewSite().getActionBars().updateActionBars();
    
    addSelectionChangedListener( this );
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#setFocus()
   */
  public void setFocus()
  {
  // TODO
  }

  /**
   * @see org.kalypso.util.repository.IRepositoryContainerListener#onRepositoryContainerChanged()
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
  public void setSelection( ISelection selection )
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
    final IStructuredSelection selection = (IStructuredSelection)event.getSelection();

    String text = "<Keim Element selektiert>";
    if( !selection.isEmpty() )
      text = selection.getFirstElement().toString();
    m_metaViewer.setText( text );
  }
}