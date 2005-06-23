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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.resource.ImageDescriptor;
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
import org.kalypso.commons.list.IListManipulator;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.command.MoveThemeDownCommand;
import org.kalypso.ogc.gml.command.MoveThemeUpCommand;
import org.kalypso.ogc.gml.command.RemoveThemeCommand;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.IMapModellView;
import org.kalypso.ogc.gml.outline.AbstractOutlineAction;
import org.kalypso.ogc.gml.outline.ActivateThemeAction;
import org.kalypso.ogc.gml.outline.GisMapOutlineViewer;
import org.kalypso.ogc.gml.outline.MoveThemeDownAction;
import org.kalypso.ogc.gml.outline.MoveThemeUpAction;
import org.kalypso.ogc.gml.outline.OpenStyleDialogAction;
import org.kalypso.ogc.gml.outline.PluginMapOutlineActionDelegate;
import org.kalypso.ogc.gml.outline.RemoveRuleAction;
import org.kalypso.ogc.gml.outline.RemoveThemeAction;
import org.kalypso.ogc.gml.outline.SaveStyleAction;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.editor.mapeditor.views.StyleEditorViewPart;
import org.kalypso.util.command.JobExclusiveCommandTarget;
import org.kalypsodeegree.model.feature.event.ModellEvent;

/**
 * OutlinePage für das MapView-Template
 * 
 * @author gernot
 */
public class GisMapOutlinePage implements IContentOutlinePage, IDoubleClickListener, IMapModellView, IListManipulator,
    ISelectionChangedListener
{
  protected MoveThemeDownAction m_moveOneDownAction = null;

  protected MoveThemeUpAction m_moveOneUpAction = null;

  protected RemoveThemeAction m_removeAction = null;

  protected OpenStyleDialogAction m_openStyleDialogAction = null;

  protected RemoveRuleAction m_removeRuleAction = null;

  protected SaveStyleAction m_saveStyleAction = null;

  protected ActivateThemeAction m_activateThemeAction = null;

  private final JobExclusiveCommandTarget m_commandTarget;

  private final GisMapOutlineViewer m_modellView;

  public GisMapOutlineViewer getModellView()
  {
    return m_modellView;
  }

  private final List m_actionDelegates = new ArrayList();

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
        ImageProvider.IMAGE_MAPVIEW_OUTLINE_DOWN, "Thema eins nach unten verschieben", m_modellView, this );

    m_moveOneUpAction = new MoveThemeUpAction( "Thema nach oben verschieben", ImageProvider.IMAGE_MAPVIEW_OUTLINE_UP,
        "Thema eins nach oben verschieben", m_modellView, this );

    m_removeAction = new RemoveThemeAction( "Thema löschen", ImageProvider.IMAGE_MAPVIEW_OUTLINE_REMOVE,
        "Thema löschen", m_modellView, this );

    m_openStyleDialogAction = new OpenStyleDialogAction( "Style verändern", ImageProvider.IMAGE_ZML_REPOSITORY_RELOAD,
        "Style ändern", m_modellView );

    m_removeRuleAction = new RemoveRuleAction( "Regel löschen", ImageProvider.IMAGE_MAPVIEW_OUTLINE_REMOVE,
        "Regel löschen", m_modellView, this );

    m_saveStyleAction = new SaveStyleAction( "Style speichern", ImageProvider.IMAGE_STYLEEDITOR_SAVE,
        "Style speichern", m_modellView );

    m_activateThemeAction = new ActivateThemeAction( "Thema aktivieren", ImageProvider.IMAGE_MAPVIEW_OUTLINE_ADD,
        "Thema aktivieren", m_modellView );

    createControlsFromPlugins();

    onModellChange( null );

  }

  private void createControlsFromPlugins()
  {
    //get platform registry (all registered plugins at start up)
    IExtensionRegistry extensionRegistry = Platform.getExtensionRegistry();
    IExtensionPoint extensionPoint = extensionRegistry.getExtensionPoint( "org.kalypso.ui", "mapviewaction" );
    //check if extention point is registered on start up
    if( extensionPoint == null )
      return;
    IExtension[] extensions = extensionPoint.getExtensions();
    // no mapview extensions have been registered
    if( extensions == null )
      return;
    for( int i = 0; i < extensions.length; i++ )
    {
      IExtension extension = extensions[i];
      IConfigurationElement configurationElement = extension.getConfigurationElements()[0];
      String title = configurationElement.getAttribute( "title" );
      String resource = configurationElement.getAttribute( "icon" );
      //TODO handle custom icon from the extension point
      //the icon is not available from the kalypso ui icon resources
      //get it from the running class and create it after the class has been
      // initiated.
      ImageDescriptor icon = ImageProvider.id( resource );//try to get it from
      // kalypso ui icon
      // resources
      String tooltip = configurationElement.getAttribute( "tooltip" );
      //create action delegate
      PluginMapOutlineActionDelegate actionDelegate;
      try
      {
        actionDelegate = new PluginMapOutlineActionDelegate( title, icon, tooltip, m_modellView, configurationElement,
            this );
        m_actionDelegates.add( actionDelegate );
      }
      catch( CoreException e )
      {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }
    }
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
    if( m_saveStyleAction != null )
      m_saveStyleAction.dispose();
    if( m_removeRuleAction != null )
      m_removeRuleAction.dispose();
    if( m_activateThemeAction != null )
      m_activateThemeAction.dispose();
    for( Iterator iter = m_actionDelegates.iterator(); iter.hasNext(); )
    {
      AbstractOutlineAction action = (AbstractOutlineAction)iter.next();
      action.dispose();
    }
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
    toolBarManager.add( m_activateThemeAction );
    toolBarManager.add( m_moveOneDownAction );
    toolBarManager.add( m_moveOneUpAction );
    toolBarManager.add( m_moveOneDownAction );
    toolBarManager.add( m_removeAction );
    toolBarManager.add( m_openStyleDialogAction );

    for( Iterator iter = m_actionDelegates.iterator(); iter.hasNext(); )
    {
      AbstractOutlineAction action = (AbstractOutlineAction)iter.next();
      toolBarManager.add( action );
    }

    actionBars.updateActionBars();

    final MenuManager menuMgr = new MenuManager( "#ThemeContextMenu" );
    menuMgr.setRemoveAllWhenShown( true );
    menuMgr.addMenuListener( new IMenuListener()
    {
      public void menuAboutToShow( IMenuManager manager )
      {
        manager.add( m_activateThemeAction );
        manager.add( m_moveOneDownAction );
        manager.add( m_moveOneUpAction );
        manager.add( m_removeAction );
        manager.add( new Separator() );
        manager.add( new Separator( IWorkbenchActionConstants.MB_ADDITIONS ) );
        manager.add( m_openStyleDialogAction );
        manager.add( m_saveStyleAction );
        manager.add( new Separator( IWorkbenchActionConstants.MB_ADDITIONS ) );
        manager.add( m_removeRuleAction );
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
    // bei jedem Focus, überprüfe ob outline beim StyleEditor registriert ist.
    IWorkbenchWindow window = Workbench.getInstance().getActiveWorkbenchWindow();
    StyleEditorViewPart part = (StyleEditorViewPart)window.getActivePage().findView(
        "org.kalypso.ui.editor.mapeditor.views.styleeditor" );

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
   * final IStructuredSelection sel = (IStructuredSelection)event.getSelection(); if( !sel.isEmpty() )
   * m_gisEditor.postCommand( new EditPropertiesCommand( getControl().getShell(), (LayerType)sel.getFirstElement() ) );
   */
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModellView#getMapModell()
   */
  public IMapModell getMapModell()
  {
    return m_modellView.getMapModell();
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModellView#setMapModell(org.kalypso.ogc.gml.mapmodel.IMapModell)
   */
  public void setMapModell( final IMapModell modell )
  {
    m_modellView.setMapModell( modell );
  }

  /**
   * 
   * @see org.kalypso.commons.list.IListManipulator#moveElementDown(java.lang.Object)
   */
  public void moveElementDown( final Object element )
  {
    final MoveThemeUpCommand moveThemeUpCommand = new MoveThemeUpCommand( getMapModell(), (IKalypsoTheme)element );
    m_commandTarget.postCommand( moveThemeUpCommand, new SelectThemeRunner( (IKalypsoTheme)element ) );
  }

  /**
   * @see org.kalypso.commons.list.IListManipulator#moveElementUp(java.lang.Object)
   */
  public void moveElementUp( final Object element )
  {
    m_commandTarget.postCommand( new MoveThemeDownCommand( getMapModell(), (IKalypsoTheme)element ),
        new SelectThemeRunner( (IKalypsoTheme)element ) );
  }

  /**
   * @see org.kalypso.commons.list.IListManipulator#removeElement(java.lang.Object)
   */
  public void removeElement( final Object element )
  {
    m_commandTarget.postCommand( new RemoveThemeCommand( getMapModell(), (IKalypsoTheme)element ),
        new SelectThemeRunner( (IKalypsoTheme)element ) );
  }

  /**
   * @see org.kalypso.commons.list.IListManipulator#addElement(java.lang.Object)
   */
  public void addElement( final Object element )
  {
  //    m_commandTarget.postCommand( new AddThemeCommand( getMapModell(),
  // (IKalypsoTheme)element ),
  //        new SelectThemeRunner( (IKalypsoTheme)element ) );
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
   * @see org.kalypsodeegree.model.feature.event.ModellEventListener#onModellChange(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( ModellEvent modellEvent )
  {
  // nix tun
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
   */
  public void selectionChanged( final SelectionChangedEvent event )
  {
  // 
  }
}