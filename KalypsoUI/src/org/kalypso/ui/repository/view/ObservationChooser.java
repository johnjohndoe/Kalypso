/*
 * --------------- Kalypso-Header --------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ------------------------------------------------------------------------------------
 */
package org.kalypso.ui.repository.view;

import org.eclipse.compare.internal.AbstractViewer;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.repository.IRepository;
import org.kalypso.repository.container.IRepositoryContainer;
import org.kalypso.repository.container.IRepositoryContainerListener;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.repository.actions.AddRepositoryAction;
import org.kalypso.ui.repository.actions.CollapseAllAction;
import org.kalypso.ui.repository.actions.ConfigurePreviewAction;
import org.kalypso.ui.repository.actions.CopyLinkAction;
import org.kalypso.ui.repository.actions.DumpStructureAction;
import org.kalypso.ui.repository.actions.ExportAsFileAction;
import org.kalypso.ui.repository.actions.ReloadAction;
import org.kalypso.ui.repository.actions.RemoveRepositoryAction;
import org.kalypso.util.adapter.IAdaptable;

/**
 * A view that allows the user to choose an observation within a tree of repositories
 * 
 * created by
 * 
 * @author schlienger (19.05.2005)
 */
public class ObservationChooser extends AbstractViewer implements IRepositoryContainerListener, ISelectionProvider
{
  private final IRepositoryContainer m_repContainer;

  private final TreeViewer m_repViewer;

  private final IViewSite m_site;

  private AddRepositoryAction m_addRepAction;

  private RemoveRepositoryAction m_removeAction;

  private ConfigurePreviewAction m_confAction;

  private ReloadAction m_reloadAction;

  private CollapseAllAction m_collapseAction;

  private ExportAsFileAction m_exportAsFileAction;

  private CopyLinkAction m_copyLinkAction;

  private DumpStructureAction m_dumpAction;

  public ObservationChooser( final Composite parent )
  {
    this( parent, null );
  }

  public ObservationChooser( final Composite parent, final IViewSite site )
  {
    m_site = site;

    m_repContainer = KalypsoGisPlugin.getDefault().getRepositoryContainer();

    m_repContainer.addRepositoryContainerListener( this );

    m_repViewer = new TreeViewer( parent, SWT.H_SCROLL | SWT.V_SCROLL );
    m_repViewer.setContentProvider( new RepositoryTreeContentProvider() );
    m_repViewer.setLabelProvider( new RepositoryLabelProvider() );
    m_repViewer.setInput( m_repContainer );

    initActions();
    initContextMenu();
    initToolbar();
  }

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

    if( m_dumpAction != null )
      m_dumpAction.dispose();
  }

  private void initActions()
  {
    m_addRepAction = new AddRepositoryAction( this );
    m_removeAction = new RemoveRepositoryAction( this );
    m_confAction = new ConfigurePreviewAction( this );
    m_reloadAction = new ReloadAction( this );
    m_collapseAction = new CollapseAllAction( this );
    m_exportAsFileAction = new ExportAsFileAction( this );
    m_copyLinkAction = new CopyLinkAction( this );
    m_dumpAction = new DumpStructureAction( this );
  }

  private void initContextMenu()
  {
    final MenuManager menuMgr = new MenuManager();
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

    if( m_site != null )
      m_site.registerContextMenu( menuMgr, m_repViewer );
  }

  /**
   * Called when the context menu is about to open.
   * 
   * @param menu
   */
  protected void fillContextMenu( final IMenuManager menu )
  {
    menu.add( m_addRepAction );
    menu.add( m_removeAction );
    menu.add( new Separator() );
    menu.add( m_confAction );
    menu.add( m_reloadAction );
    menu.add( new Separator() );
    menu.add( m_collapseAction );
    menu.add( new Separator() );
    menu.add( m_exportAsFileAction );
    menu.add( m_dumpAction );

    if( isObservationSelected( m_repViewer.getSelection() ) != null )
    {
      menu.add( m_copyLinkAction );
      menu.add( new Separator() );
    }

    menu.add( new Separator( IWorkbenchActionConstants.MB_ADDITIONS ) );
  }

  private void initToolbar()
  {
    final IToolBarManager toolBarManager;

    if( m_site != null )
      toolBarManager = m_site.getActionBars().getToolBarManager();
    else
      toolBarManager = new ToolBarManager();

    toolBarManager.add( m_addRepAction );
    toolBarManager.add( m_removeAction );
    toolBarManager.add( new Separator() );
    toolBarManager.add( m_confAction );
    toolBarManager.add( m_reloadAction );
    toolBarManager.add( new Separator() );
    toolBarManager.add( m_collapseAction );
    toolBarManager.add( new Separator() );
    toolBarManager.add( m_exportAsFileAction );
    toolBarManager.add( m_dumpAction );

    if( m_site != null )
      m_site.getActionBars().updateActionBars();
  }

  /**
   * @return the IObservation object when the selection is an IAdaptable object that get deliver an IObservation.
   */
  public IObservation isObservationSelected( final ISelection selection )
  {
    final IStructuredSelection sel = (IStructuredSelection)selection;
    if( sel.isEmpty() )
      return null;

    final Object element = sel.getFirstElement();
    if( element instanceof IAdaptable )
    {
      final IObservation obs = (IObservation)( (IAdaptable)element ).getAdapter( IObservation.class );

      return obs;
    }

    return null;
  }

  /**
   * @return checks if given selection is a <code>IRepository</code>. Returns a repository or null if no repository
   *         is selected.
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

  public TreeViewer getViewer()
  {
    return m_repViewer;
  }

  /**
   * @see org.eclipse.jface.viewers.Viewer#getControl()
   */
  public Control getControl()
  {
    return m_repViewer.getControl();
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#getSelection()
   */
  public ISelection getSelection()
  {
    return m_repViewer.getSelection();
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#setSelection(org.eclipse.jface.viewers.ISelection)
   */
  public void setSelection( final ISelection selection )
  {
    m_repViewer.setSelection( selection );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#addSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
   */
  public void addSelectionChangedListener( ISelectionChangedListener listener )
  {
    m_repViewer.addSelectionChangedListener( listener );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#removeSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
   */
  public void removeSelectionChangedListener( ISelectionChangedListener listener )
  {
    m_repViewer.removeSelectionChangedListener( listener );
  }

  /**
   * @see org.kalypso.repository.container.IRepositoryContainerListener#onRepositoryContainerChanged()
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

    if( !getControl().isDisposed() )
      getControl().getDisplay().asyncExec( r );
  }

  public IRepositoryContainer getRepositoryContainer()
  {
    return m_repContainer;
  }

  public Shell getShell()
  {
    return getControl().getShell();
  }
}
