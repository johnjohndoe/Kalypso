package org.kalypso.editor.mapeditor;

import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableTreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.TableTree;
import org.eclipse.swt.custom.TableTreeItem;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;
import org.kalypso.editor.mapeditor.actions.AddThemeAction;
import org.kalypso.editor.mapeditor.actions.MoveThemeDownAction;
import org.kalypso.editor.mapeditor.actions.MoveThemeUpAction;
import org.kalypso.editor.mapeditor.actions.OpenStyleDialogAction;
import org.kalypso.editor.mapeditor.actions.RemoveThemeAction;
import org.kalypso.ogc.IMapModellView;
import org.kalypso.ogc.MapModell;
import org.kalypso.ogc.command.EnableThemeCommand;
import org.kalypso.ogc.command.MoveThemeDownCommand;
import org.kalypso.ogc.command.MoveThemeUpCommand;
import org.kalypso.ogc.command.RemoveThemeCommand;
import org.kalypso.ogc.event.ModellEvent;
import org.kalypso.ogc.gml.KalypsoTheme;
import org.kalypso.plugin.ImageProvider;
import org.kalypso.util.command.ICommandManager;
import org.kalypso.util.command.RedoAction;
import org.kalypso.util.command.UndoAction;
import org.kalypso.util.list.IListManipulator;

/**
 * OutlinePage f?r das MapView-Template
 * 
 * @author gernot
 */
public class GisMapOutlinePage implements IContentOutlinePage, IDoubleClickListener,
    IMapModellView, SelectionListener, IListManipulator
{
  private MoveThemeDownAction m_moveOneDownAction = null;

  private MoveThemeUpAction m_moveOneUpAction = null;

  private RemoveThemeAction m_removeAction = null;

  private AddThemeAction m_addAction = null;

  private OpenStyleDialogAction m_openStyleDialogAction=null;
  
  private TableTreeViewer m_viewer;

  private final MapModellTreeContentProvider m_contentProvider = new MapModellTreeContentProvider();

  private final MapModellLabelProvider m_labelProvider = new MapModellLabelProvider();

  private MapModell m_mapModell;

  private final ICommandManager m_commandManager;

  public GisMapOutlinePage( final ICommandManager commandManager )
  {
    m_commandManager = commandManager;
  }

  /**
   * @see org.eclipse.ui.part.IPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    final TableTree tree = new TableTree( parent, SWT.SINGLE | SWT.CHECK );
    tree.addSelectionListener( this );

    m_viewer = new TableTreeViewer( tree );
    m_viewer.setContentProvider( m_contentProvider );
    m_viewer.setLabelProvider( m_labelProvider );

    m_viewer.addDoubleClickListener( this );

    m_viewer.setInput( m_mapModell );

    m_moveOneDownAction = new MoveThemeDownAction( "Thema nach unten verschieben",
        ImageProvider.IMAGE_MAPVIEW_OUTLINE_DOWN, "Thema eins nach unten verschieben", m_viewer, this );

    m_moveOneUpAction = new MoveThemeUpAction( "Thema nach oben verschieben",
        ImageProvider.IMAGE_MAPVIEW_OUTLINE_UP, "Thema eins nach oben verschieben", m_viewer,
        this );

    m_removeAction = new RemoveThemeAction( "Thema l?schen",
        ImageProvider.IMAGE_MAPVIEW_OUTLINE_REMOVE, "Thema l?schen", m_viewer, this );

    m_addAction = new AddThemeAction( "Thema hinzuf?gen", ImageProvider.IMAGE_MAPVIEW_OUTLINE_ADD,
        "Thema hinzuf?gen", m_viewer, this );
    
    m_openStyleDialogAction = new OpenStyleDialogAction( "Style ver?ndern", ImageProvider.IMAGE_MAPVIEW_OUTLINE_ADD,
            "Style ?ndern", m_viewer, this );
        
    onModellChange(null);
  }

  /**
   * @see org.eclipse.ui.part.IPage#dispose()
   */
  public void dispose()
  {
    if( m_mapModell != null )
      m_mapModell.removeModellListener( this );

    if( m_viewer != null )
    {
      m_viewer.removeDoubleClickListener( this );

      // tabeltree already disposed! (probably released its listeners itself)
      // m_viewer.getTableTree().removeSelectionListener(this);
    }

    m_contentProvider.dispose();
    m_labelProvider.dispose();

    if( m_moveOneDownAction != null )
      m_moveOneDownAction.dispose();
    if( m_moveOneUpAction != null )
      m_moveOneUpAction.dispose();
    if( m_removeAction != null )
      m_removeAction.dispose();
  }

  /**
   * @see org.eclipse.ui.part.IPage#getControl()
   */
  public Control getControl()
  {
    return m_viewer.getControl();
  }

  /**
   * @see org.eclipse.ui.part.IPage#setActionBars(org.eclipse.ui.IActionBars)
   */
  public void setActionBars( final IActionBars actionBars )
  {
    actionBars.setGlobalActionHandler( ActionFactory.UNDO.getId(),
        new UndoAction( m_commandManager ) );
    actionBars.setGlobalActionHandler( ActionFactory.REDO.getId(),
        new RedoAction( m_commandManager ) );

    final IToolBarManager toolBarManager = actionBars.getToolBarManager();
    toolBarManager.add( m_addAction );
    toolBarManager.add( m_moveOneDownAction );
    toolBarManager.add( m_moveOneUpAction );
    toolBarManager.add( m_moveOneDownAction );
    toolBarManager.add( m_removeAction );
    toolBarManager.add( m_openStyleDialogAction );
    actionBars.updateActionBars();
  }

  /**
   * @see org.eclipse.ui.part.IPage#setFocus()
   */
  public void setFocus()
  {
  // nichts tun
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#addSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
   */
  public void addSelectionChangedListener( final ISelectionChangedListener listener )
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

  /**
   * @see org.eclipse.jface.viewers.IDoubleClickListener#doubleClick(org.eclipse.jface.viewers.DoubleClickEvent)
   */
  public void doubleClick( final DoubleClickEvent event )
  {
  /*
   * final IStructuredSelection sel =
   * (IStructuredSelection)event.getSelection(); if( !sel.isEmpty() )
   * m_gisEditor.postCommand( new EditPropertiesCommand(
   * getControl().getShell(), (LayerType)sel.getFirstElement() ) );
   */
  }

  /**
   * @see org.kalypso.ogc.IMapModellView#getMapModell()
   */
  public MapModell getMapModell()
  {
    return m_mapModell;
  }

  /**
   * @see org.kalypso.ogc.IMapModellView#setMapModell(org.kalypso.ogc.MapModell)
   */
  public void setMapModell( MapModell modell )
  {
    if( m_mapModell != null )
      m_mapModell.removeModellListener( this );

    m_mapModell = modell;

    if( m_mapModell != null )
      m_mapModell.addModellListener( this );

    if( m_viewer != null )
      m_viewer.setInput( modell );

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
      final TableTree tt = (TableTree)m_viewer.getControl();
      final TableTreeItem[] items = tt.getItems();
      final MapModell mm = getMapModell();

      for( int i = 0; i < items.length; i++ )
      {
        final TableTreeItem item = items[i];

        tt.getDisplay().asyncExec( new Runnable()
        {
          public void run()
          {
            if( !item.isDisposed() )
              item.setChecked( mm.isThemeEnabled( (KalypsoTheme)item.getData() ) );
          }
        } );
      }
    }
  }

  /**
   * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
   */
  public void widgetSelected( final SelectionEvent e )
  {
    if( ( e.detail & SWT.CHECK ) != 0 )
    {
      final TableTreeItem ti = (TableTreeItem)e.item;
      final Object data = ti.getData();
      if( data instanceof KalypsoTheme )
        m_commandManager.postCommand( new EnableThemeCommand( m_mapModell, (KalypsoTheme)data, ti
            .getChecked() ), null );
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
   * 
   * @see org.kalypso.util.list.IListManipulator#moveElementDown(java.lang.Object)
   */
  public void moveElementDown( final Object element )
  {
    m_commandManager.postCommand( new MoveThemeUpCommand( m_mapModell, (KalypsoTheme)element ), new SelectThemeRunner( (KalypsoTheme)element ) );
  }

  /**
   * @see org.kalypso.util.list.IListManipulator#moveElementUp(java.lang.Object)
   */
  public void moveElementUp( final Object element )
  {
    m_commandManager.postCommand( new MoveThemeDownCommand( m_mapModell, (KalypsoTheme)element ), new SelectThemeRunner( (KalypsoTheme)element ) );
  }

  /**
   * @see org.kalypso.util.list.IListManipulator#removeElement(java.lang.Object)
   */
  public void removeElement( final Object element )
  {
    m_commandManager.postCommand( new RemoveThemeCommand( m_mapModell, (KalypsoTheme)element ), new SelectThemeRunner( (KalypsoTheme)element ) );
  }

  /**
   * @see org.kalypso.util.list.IListManipulator#addElement(java.lang.Object)
   */
  public void addElement( final Object elementBefore )
  {
  //m_commandManager.postCommand( new AddThemeCommand( m_mapModell,
  // (Theme)element ) );
  }
  
  private final class SelectThemeRunner implements Runnable
  {
    public final KalypsoTheme m_theme;

    public SelectThemeRunner( final KalypsoTheme theme )
    {
      m_theme = theme;
    }
    
    /**
     * @see java.lang.Runnable#run()
     */
    public void run()
    {
      getControl().getDisplay().asyncExec( new Runnable() {
        public void run()
        {
          setSelection( new StructuredSelection( m_theme ) );
        }} );
    }
  }

}