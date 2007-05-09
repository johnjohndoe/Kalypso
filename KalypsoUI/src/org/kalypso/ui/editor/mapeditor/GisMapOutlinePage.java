/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ui.editor.mapeditor;

import java.util.List;

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
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;
import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.list.IListManipulator;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.command.MoveThemeDownCommand;
import org.kalypso.ogc.gml.command.MoveThemeUpCommand;
import org.kalypso.ogc.gml.command.RemoveThemeCommand;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.IMapModellView;
import org.kalypso.ogc.gml.outline.AbstractOutlineAction;
import org.kalypso.ogc.gml.outline.GisMapOutlineViewer;
import org.kalypso.ogc.gml.outline.PluginMapOutlineAction;
import org.kalypso.ui.editor.mapeditor.views.StyleEditorViewPart;
import org.kalypso.util.command.JobExclusiveCommandTarget;
import org.kalypsodeegree.model.feature.event.ModellEvent;

/**
 * OutlinePage für das MapView-Template
 * 
 * @author gernot
 */
public class GisMapOutlinePage implements IContentOutlinePage, IDoubleClickListener, IMapModellView, IListManipulator, ISelectionChangedListener
{
  private final JobExclusiveCommandTarget m_commandTarget;

  private final GisMapOutlineViewer m_modellView;

  private List<PluginMapOutlineAction> m_actionDelegates = null;

  public GisMapOutlineViewer getModellView( )
  {
    return m_modellView;
  }

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
    if( parent.isDisposed() )
    {
      System.out.println( "parent is disposed" );
    }
    m_modellView.createControl( parent );

    m_modellView.addDoubleClickListener( this );

    m_actionDelegates = GisMapOutlinePageExtension.getRegisteredMapOutlineActions( this );

    onModellChange( null );
  }

  /**
   * @see org.eclipse.ui.part.IPage#dispose()
   */
  public void dispose( )
  {
    if( m_modellView != null )
    {
      m_modellView.removeDoubleClickListener( this );      
      m_modellView.dispose();
    }

    for( final AbstractOutlineAction action : m_actionDelegates )
      action.dispose();
  }

  /**
   * @see org.eclipse.ui.part.IPage#getControl()
   */
  public Control getControl( )
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
    for( final AbstractOutlineAction action : m_actionDelegates )
    {
      toolBarManager.add( action );
    }

    actionBars.updateActionBars();

    final MenuManager menuMgr = new MenuManager( "#ThemeContextMenu" );
    menuMgr.setRemoveAllWhenShown( true );
    menuMgr.addMenuListener( new IMenuListener()
    {
      public void menuAboutToShow( IMenuManager manager )
      {
        manager.add( new Separator() );
        manager.add( new Separator( IWorkbenchActionConstants.MB_ADDITIONS ) );
        manager.add( new Separator( IWorkbenchActionConstants.MB_ADDITIONS ) );
      }
    } );

    final Menu menu = menuMgr.createContextMenu( m_modellView.getControl() );

    final IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
    // final IViewPart outlineView = page.findView( IPageLayout.ID_OUTLINE );
    // if( outlineView != null )
    // outlineView.getSite().registerContextMenu(
    // menuMgr, m_modellView );

    // TODO: das nimmt nicht die outline view sondern irgendeine aktive
    // besser wäre wie im kommentar oben, aber die outline-view ist noch gar nicht da
    // was tun?
    if( page != null )
    {
      final IWorkbenchPart activePart = page.getActivePart();
      activePart.getSite().registerContextMenu( menuMgr, m_modellView );
    }
    m_modellView.getControl().setMenu( menu );
  }

  /**
   * @see org.eclipse.ui.part.IPage#setFocus()
   */
  public void setFocus( )
  {
    // bei jedem Focus, überprüfe ob outline beim StyleEditor registriert ist.
    final IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
    final StyleEditorViewPart part = (StyleEditorViewPart) window.getActivePage().findView( "org.kalypso.ui.editor.mapeditor.views.styleeditor" );

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
  public ISelection getSelection( )
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
     * final IStructuredSelection sel = (IStructuredSelection)event.getSelection(); if( !sel.isEmpty() )
     * m_gisEditor.postCommand( new EditPropertiesCommand( getControl().getShell(), (LayerType)sel.getFirstElement() ) );
     */
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModellView#getMapModell()
   */
  public MapPanel getMapPanel( )
  {
    return m_modellView.getMapPanel();
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModellView#setMapModell(org.kalypso.ogc.gml.mapmodel.IMapModell)
   */
  public void setMapPanel( final MapPanel panel )
  {   
    m_modellView.setMapPanel( panel );   
  }

  /**
   * @see org.kalypso.commons.list.IListManipulator#moveElementDown(java.lang.Object)
   */
  public void moveElementDown( final Object element )
  {
    final MoveThemeUpCommand moveThemeUpCommand = new MoveThemeUpCommand( getMapPanel().getMapModell(), (IKalypsoTheme) element );
    m_commandTarget.postCommand( moveThemeUpCommand, new SelectThemeRunner( (IKalypsoTheme) element ) );
  }

  /**
   * @see org.kalypso.commons.list.IListManipulator#moveElementUp(java.lang.Object)
   */
  public void moveElementUp( final Object element )
  {
    m_commandTarget.postCommand( new MoveThemeDownCommand( getMapPanel().getMapModell(), (IKalypsoTheme) element ), new SelectThemeRunner( (IKalypsoTheme) element ) );
  }

  /**
   * @see org.kalypso.commons.list.IListManipulator#removeElement(java.lang.Object)
   */
  public void removeElement( final Object element )
  {
    m_commandTarget.postCommand( new RemoveThemeCommand( getMapPanel().getMapModell(), (IKalypsoTheme) element ), new SelectThemeRunner( (IKalypsoTheme) element ) );
  }

  /**
   * @see org.kalypso.commons.list.IListManipulator#addElement(java.lang.Object)
   */
  public void addElement( final Object element )
  {
    // m_commandTarget.postCommand( new AddThemeCommand( getMapModell(),
    // (IKalypsoTheme)element ),
    // new SelectThemeRunner( (IKalypsoTheme)element ) );
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
    public void run( )
    {
      getControl().getDisplay().asyncExec( new Runnable()
      {
        public void run( )
        {
          setSelection( new StructuredSelection( m_theme ) );
        }
      } );
    }
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventListener#onModellChange(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    //
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
   */
  public void selectionChanged( final SelectionChangedEvent event )
  {
    // 
  }

  /**
   * @param command
   * @param runnable
   * @see org.kalypso.util.command.JobExclusiveCommandTarget#postCommand(org.kalypso.commons.command.ICommand, java.lang.Runnable)
   */
  public void postCommand( ICommand command, Runnable runnable )
  {
    m_commandTarget.postCommand( command, runnable );
  }
}