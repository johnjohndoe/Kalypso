package org.kalypso.ogc.gml.outline;

import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.jface.viewers.TableTreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.TableTree;
import org.eclipse.swt.custom.TableTreeItem;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.kalypso.ogc.IMapModell;
import org.kalypso.ogc.IMapModellView;
import org.kalypso.ogc.command.EnableThemeCommand;
import org.kalypso.ogc.event.ModellEvent;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.util.command.ICommand;
import org.kalypso.util.command.ICommandTarget;

/**
 * @author belger
 */
public class GisMapOutlineViewer implements ISelectionProvider,
    IMapModellView, SelectionListener
{
  protected StructuredViewer m_viewer;

  private final MapModellTreeContentProvider m_contentProvider = new MapModellTreeContentProvider();

  private final MapModellLabelProvider m_labelProvider = new MapModellLabelProvider();

  private IMapModell m_mapModel;

  private final ICommandTarget m_commandTarget;

  public GisMapOutlineViewer( final ICommandTarget commandTarget, final IMapModell mapModel )
  {
    setMapModell( mapModel );
    m_commandTarget = commandTarget;
  }

  public void dispose()
  {
    m_contentProvider.dispose();
    m_labelProvider.dispose();

    // tabeltree already disposed! (probably released its listeners itself)
    // m_viewer.getTableTree().removeSelectionListener(this);
  }

  public void createControl( final Composite parent )
  {
    final TableTree tree = new TableTree( parent, SWT.SINGLE | SWT.CHECK );
    tree.addSelectionListener( this );

    m_viewer = new TableTreeViewer( tree );    
    m_viewer.setContentProvider( m_contentProvider );
    m_viewer.setLabelProvider( m_labelProvider );

    m_viewer.setInput( m_mapModel );
    m_viewer.refresh();
    // Refresh check state
    //onModellChange( null );
  }

  /**
   * @see org.eclipse.jface.viewers.Viewer#getControl()
   */
  public Control getControl()
  {
    return m_viewer.getControl();
  }

  /**
   * @see org.kalypso.ogc.IMapModellView#getMapModell()
   */
  public IMapModell getMapModell()
  {
    return m_mapModel;
  }

  /**
   * @see org.kalypso.ogc.IMapModellView#setMapModell(org.kalypso.ogc.IMapModell)
   */
  public void setMapModell( final IMapModell modell )
  {
    if( m_mapModel != null )
      m_mapModel.removeModellListener( this );

    m_mapModel = modell;

    if( m_mapModel != null )
      m_mapModel.addModellListener( this );

    if( m_viewer != null && m_viewer.getContentProvider() != null )
    {
      m_viewer.getControl().getDisplay().syncExec( new Runnable()
      {
        public void run()
        {
          m_viewer.setInput( modell );
        }
      } );
    }

    onModellChange( null );
  }

  /**
   * @see org.kalypso.ogc.event.ModellEventListener#onModellChange(org.kalypso.ogc.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    // den Checkstate setzen!
    if( m_viewer != null )
    {
      final IMapModell mm = getMapModell();
      if( mm == null )
        return;

      final StructuredViewer viewer = m_viewer;
      final TableTree tt = (TableTree)m_viewer.getControl();
      if( tt.isDisposed() )
        return;

      final TableTreeItem[] items = tt.getItems();

      tt.getDisplay().asyncExec( new Runnable()
      {
        public void run()
        {

          for( int i = 0; i < items.length; i++ )
          {
            final TableTreeItem item = items[i];

            if( !item.isDisposed() )
              item.setChecked( mm.isThemeEnabled( (IKalypsoTheme)item.getData() ) );
          }

          // und die ganze view refreshen!
          viewer.refresh();
        }

      } );
    }
  }

  /**
   * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
   */
  public void widgetSelected( final SelectionEvent e )
  {

      final TableTreeItem ti = (TableTreeItem)e.item;
      final Object data = ti.getData();
      if( data instanceof IKalypsoTheme )
      {
        if(m_mapModel.getActiveTheme()!=(IKalypsoTheme)data)
         m_mapModel.activateTheme((IKalypsoTheme)data);   
        // TODO 1. create command for this
        // TODO 2. create MultiCommand (eg. activate and enable) 
      }    
    
    if( ( e.detail & SWT.CHECK ) != 0 )
    {
      if( data instanceof IKalypsoTheme )
      {
        final ICommand command = new EnableThemeCommand( m_mapModel, (IKalypsoTheme)data, ti
            .getChecked() );
        m_commandTarget.postCommand( command, null );
      }
    }    
  }

  /**
   * @see org.eclipse.swt.events.SelectionListener#widgetDefaultSelected(org.eclipse.swt.events.SelectionEvent)
   */
  public void widgetDefaultSelected( SelectionEvent e )
  {
  //
  }

  /**
   * @see org.eclipse.jface.viewers.IInputProvider#getInput()
   */
  public Object getInput()
  {
    return null;
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#addSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
   */
  public void addSelectionChangedListener( ISelectionChangedListener listener )
  {
    m_viewer.addSelectionChangedListener( listener );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#getSelection()
   */
  public ISelection getSelection()
  {
    return m_viewer.getSelection();
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#removeSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
   */
  public void removeSelectionChangedListener( ISelectionChangedListener listener )
  {
    m_viewer.removeSelectionChangedListener( listener );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#setSelection(org.eclipse.jface.viewers.ISelection)
   */
  public void setSelection( ISelection selection )
  {
    m_viewer.setSelection( selection );
  }

  public IStructuredContentProvider getContentProvider()
  {
    return (IStructuredContentProvider)m_viewer.getContentProvider();
  }

  /**
   * Adds a listener for double-clicks in this viewer. Has no effect if an
   * identical listener is already registered.
   * 
   * @param listener
   *          a double-click listener
   */
  public void addDoubleClickListener( IDoubleClickListener listener )
  {
    m_viewer.addDoubleClickListener( listener );
  }

  /**
   * Removes the given double-click listener from this viewer. Has no affect if
   * an identical listener is not registered.
   * 
   * @param listener
   *          a double-click listener
   */
  public void removeDoubleClickListener( IDoubleClickListener listener )
  {
    m_viewer.removeDoubleClickListener( listener );
  }

}