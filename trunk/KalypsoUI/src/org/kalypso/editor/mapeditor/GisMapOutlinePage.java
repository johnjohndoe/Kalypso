package org.kalypso.editor.mapeditor;

import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.internal.Workbench;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;
import org.kalypso.editor.mapeditor.views.StyleEditorViewPart;
import org.kalypso.ogc.IMapModell;
import org.kalypso.ogc.IMapModellView;
import org.kalypso.ogc.command.MoveThemeDownCommand;
import org.kalypso.ogc.command.MoveThemeUpCommand;
import org.kalypso.ogc.command.RemoveThemeCommand;
import org.kalypso.ogc.event.ModellEvent;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.outline.GisMapOutlineViewer;
import org.kalypso.ogc.gml.outline.MoveThemeDownAction;
import org.kalypso.ogc.gml.outline.MoveThemeUpAction;
import org.kalypso.ogc.gml.outline.OpenStyleDialogAction;
import org.kalypso.ogc.gml.outline.RemoveThemeAction;
import org.kalypso.ogc.gml.outline.SaveStyleAction;
import org.kalypso.plugin.ImageProvider;
import org.kalypso.util.command.JobExclusiveCommandTarget;
import org.kalypso.util.list.IListManipulator;

/**
 * OutlinePage f�r das MapView-Template
 * 
 * @author gernot
 */
public class GisMapOutlinePage implements IContentOutlinePage, IDoubleClickListener,
    IMapModellView, IListManipulator, ISelectionChangedListener
{
  protected MoveThemeDownAction m_moveOneDownAction = null;

  protected MoveThemeUpAction m_moveOneUpAction = null;

  protected RemoveThemeAction m_removeAction = null;

  protected OpenStyleDialogAction m_openStyleDialogAction = null;
  
  protected SaveStyleAction m_saveStyleAction = null;

  private final JobExclusiveCommandTarget m_commandTarget;

  private final GisMapOutlineViewer m_modellView;

  public GisMapOutlinePage( final JobExclusiveCommandTarget commandTarget )
  {
    m_commandTarget = commandTarget;
    m_modellView = new GisMapOutlineViewer( m_commandTarget, null );
  }

  /**
   * @see org.eclipse.ui.part.IPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    m_modellView.createControl( parent );
    
    m_modellView.addDoubleClickListener( this );
    
    m_moveOneDownAction = new MoveThemeDownAction( "Thema nach unten verschieben",
        ImageProvider.IMAGE_MAPVIEW_OUTLINE_DOWN, "Thema eins nach unten verschieben", m_modellView,
        this );

    m_moveOneUpAction = new MoveThemeUpAction( "Thema nach oben verschieben",
        ImageProvider.IMAGE_MAPVIEW_OUTLINE_UP, "Thema eins nach oben verschieben", m_modellView, this );

    m_removeAction = new RemoveThemeAction( "Thema l�schen",
        ImageProvider.IMAGE_MAPVIEW_OUTLINE_REMOVE, "Thema l�schen", m_modellView, this );

//    m_addAction = new AddThemeAction( "Thema hinzuf?gen", ImageProvider.IMAGE_MAPVIEW_OUTLINE_ADD,
//        "Thema hinzuf?gen", m_modellView, this );

    m_openStyleDialogAction = new OpenStyleDialogAction( "Style ver�ndern",
        ImageProvider.IMAGE_MAPVIEW_OUTLINE_ADD, "Style �ndern", m_modellView );
        
    m_saveStyleAction = new SaveStyleAction( "Style speichern",
    		ImageProvider.IMAGE_MAPVIEW_OUTLINE_ADD, "Style speichern", m_modellView);    

    onModellChange( null );
  }

  /**
   * @see org.eclipse.ui.part.IPage#dispose()
   */
  public void dispose()
  {
    if( m_modellView != null )
    {
      m_modellView.removeDoubleClickListener( this );
      
      m_modellView.dispose();
    }

    if( m_moveOneDownAction != null )
      m_moveOneDownAction.dispose();
    if( m_moveOneUpAction != null )
      m_moveOneUpAction.dispose();
    if( m_removeAction != null )
      m_removeAction.dispose();
    if(m_saveStyleAction !=null)
    	m_saveStyleAction.dispose();
  }

  /**
   * @see org.eclipse.ui.part.IPage#getControl()
   */
  public Control getControl()
  {
    return m_modellView.getControl();
  }

  /**
   * @see org.eclipse.ui.part.IPage#setActionBars(org.eclipse.ui.IActionBars)
   */
  public void setActionBars( final IActionBars actionBars )
  {
    actionBars.setGlobalActionHandler( ActionFactory.UNDO.getId(), m_commandTarget.undoAction );
    actionBars.setGlobalActionHandler( ActionFactory.REDO.getId(), m_commandTarget.redoAction );

    final IToolBarManager toolBarManager = actionBars.getToolBarManager();
    toolBarManager.add( m_moveOneDownAction );
    toolBarManager.add( m_moveOneUpAction );
    toolBarManager.add( m_moveOneDownAction );
    toolBarManager.add( m_removeAction );
    toolBarManager.add( m_openStyleDialogAction );
    actionBars.updateActionBars();

    final MenuManager menuMgr = new MenuManager( "#ThemeContextMenu" );
    menuMgr.setRemoveAllWhenShown( true );
    menuMgr.addMenuListener( new IMenuListener()
    {
      public void menuAboutToShow( IMenuManager manager )
      {
        manager.add( m_moveOneDownAction );
        manager.add( m_moveOneUpAction );
        manager.add( m_removeAction );
        manager.add( new Separator() );
        manager.add( new Separator( IWorkbenchActionConstants.MB_ADDITIONS ) );
        manager.add( m_openStyleDialogAction );
        manager.add( m_saveStyleAction);
      }
    } );
    final Menu menu = menuMgr.createContextMenu( m_modellView.getControl() );
    m_modellView.getControl().setMenu( menu );
  }

  /**
   * @see org.eclipse.ui.part.IPage#setFocus()
   */
  public void setFocus()
  {
    // bei jedem Focus, �berpr�fe ob outline beim StyleEditor registriert ist.
    IWorkbenchWindow window = Workbench.getInstance().getActiveWorkbenchWindow();
    StyleEditorViewPart part = (StyleEditorViewPart)window.getActivePage().findView(
        "org.kalypso.editor.mapeditor.views.styleeditor" );

    if( part != null )
      part.setSelectionChangedProvider( this );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#addSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
   */
  public void addSelectionChangedListener( final ISelectionChangedListener listener )
  {
    m_modellView.addSelectionChangedListener( listener );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#getSelection()
   */
  public ISelection getSelection()
  {
    return m_modellView.getSelection();
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#removeSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
   */
  public void removeSelectionChangedListener( ISelectionChangedListener listener )
  {
    m_modellView.removeSelectionChangedListener( listener );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#setSelection(org.eclipse.jface.viewers.ISelection)
   */
  public void setSelection( ISelection selection )
  {
    m_modellView.setSelection( selection );
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
  public IMapModell getMapModell()
  {
    return m_modellView.getMapModell();
  }

  /**
   * @see org.kalypso.ogc.IMapModellView#setMapModell(org.kalypso.ogc.MapModell)
   */
  public void setMapModell( final IMapModell modell )
  {
    m_modellView.setMapModell( modell );
  }

  /**
   * 
   * @see org.kalypso.util.list.IListManipulator#moveElementDown(java.lang.Object)
   */
  public void moveElementDown( final Object element )
  {
    final MoveThemeUpCommand moveThemeUpCommand = new MoveThemeUpCommand( getMapModell(),
        (IKalypsoTheme)element );
    m_commandTarget
        .postCommand( moveThemeUpCommand, new SelectThemeRunner( (IKalypsoTheme)element ) );
  }

  /**
   * @see org.kalypso.util.list.IListManipulator#moveElementUp(java.lang.Object)
   */
  public void moveElementUp( final Object element )
  {
    m_commandTarget.postCommand( new MoveThemeDownCommand( getMapModell(), (IKalypsoTheme)element ),
        new SelectThemeRunner( (IKalypsoTheme)element ) );
  }

  /**
   * @see org.kalypso.util.list.IListManipulator#removeElement(java.lang.Object)
   */
  public void removeElement( final Object element )
  {
    m_commandTarget.postCommand( new RemoveThemeCommand( getMapModell(), (IKalypsoTheme)element ),
        new SelectThemeRunner( (IKalypsoTheme)element ) );
  }

  /**
   * @see org.kalypso.util.list.IListManipulator#addElement(java.lang.Object)
   */
  public void addElement( final Object elementBefore )
  {
  // TODO
  }

  private final class SelectThemeRunner implements Runnable
  {
    public final IKalypsoTheme m_theme;

    public SelectThemeRunner( final IKalypsoTheme theme )
    {
      m_theme = theme;
    }

    /**
     * @see java.lang.Runnable#run()
     */
    public void run()
    {
      getControl().getDisplay().asyncExec( new Runnable()
      {
        public void run()
        {
          setSelection( new StructuredSelection( m_theme ) );
        }
      } );
    }
  }

  /**
   * @see org.kalypso.ogc.event.ModellEventListener#onModellChange(org.kalypso.ogc.event.ModellEvent)
   */
  public void onModellChange( ModellEvent modellEvent )
  {
    // nix tun
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
   */
  public void selectionChanged( SelectionChangedEvent event )
  {
    // TODO: setactivetheme
  }
}