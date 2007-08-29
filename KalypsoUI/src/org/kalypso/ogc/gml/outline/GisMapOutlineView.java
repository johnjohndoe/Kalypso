/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 *
 *  and
 *
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Contact:
 *
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *
 *  ---------------------------------------------------------------------------*/
package org.kalypso.ogc.gml.outline;

import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.runtime.ISafeRunnable;
import org.eclipse.core.runtime.SafeRunner;
import org.eclipse.jface.util.SafeRunnable;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchPartSite;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.commons.command.ICommand;
import org.kalypso.contribs.eclipse.ui.partlistener.PartAdapter;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.listeners.IMapPanelListener;
import org.kalypso.ogc.gml.map.listeners.MapPanelAdapter;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.IMapModellView;
import org.kalypso.ogc.gml.mapmodel.IMapModellViewListener;
import org.kalypso.ui.editor.mapeditor.AbstractMapPart;
import org.kalypso.ui.views.map.MapView;
import org.kalypso.util.command.JobExclusiveCommandTarget;

/**
 * A legend view that uses the {@link GisMapOutlineViewer}. Provides the same view on a {@link IMapModell} as a
 * {@link org.kalypso.ui.editor.mapeditor.GisMapOutlinePage}. Usable with all view or editors that extend
 * {@link AbstractMapPart}.
 * 
 * @author Stefan Kurzbach
 */
public class GisMapOutlineView extends ViewPart implements IMapModellView
{
  public static final String ID = "org.kalypso.ui.views.outline";

  private final IMapPanelListener m_mapPanelListener = new MapPanelAdapter()
  {
    /**
     * @see org.kalypso.ogc.gml.map.MapPanelAdapter#onMapModelChanged(org.kalypso.ogc.gml.map.MapPanel,
     *      org.kalypso.ogc.gml.mapmodel.IMapModell, org.kalypso.ogc.gml.mapmodel.IMapModell)
     */
    @Override
    public void onMapModelChanged( final MapPanel source, final IMapModell oldModel, final IMapModell newModel )
    {
      handleMapModelChanged( newModel );
    }
  };

  private final Set<IMapModellViewListener> m_mapModellViewListeners = new HashSet<IMapModellViewListener>();

  protected GisMapOutlineViewer m_viewer;

  private JobExclusiveCommandTarget m_commandTarget;

  protected AbstractMapPart m_mapPart;

  private IPartListener m_partListener;

  private MapPanel m_panel;

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createPartControl( final Composite parent )
  {
    final Composite container = new Composite( parent, SWT.FILL );
    final GridLayout gridLayout = new GridLayout();
    gridLayout.horizontalSpacing = 0;
    gridLayout.verticalSpacing = 0;
    gridLayout.marginHeight = 0;
    gridLayout.marginWidth = 0;
    container.setLayout( gridLayout );
    m_viewer = new GisMapOutlineViewer( null, null );
    m_viewer.createControl( container );
    m_viewer.getControl().setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    final IWorkbenchPartSite site = getSite();
    site.setSelectionProvider( m_viewer );

    // initialize view with active map editor or view if any
    final IWorkbenchPage page = site.getPage();
    page.addPartListener( m_partListener );
    final IEditorPart activeEditor = page.getActiveEditor();

    // maybe just start this search in a job?
    // TODO: bad, here we lok for a specific view id; in the part-listener we look for any
    // view wich is a AbstractMapPart! This should be done here as well.
    // TODO: also if a Map-View is active this shoould be used in preference
    // TODO: what about non-active editors?
    final IViewPart mapView = page.findView( MapView.ID );
    // try to find map editor first
    if( activeEditor instanceof AbstractMapPart )
      setMapPart( (AbstractMapPart) activeEditor );
    else if( mapView instanceof AbstractMapPart )
      setMapPart( (AbstractMapPart) mapView );
  }

  /**
   * @see org.eclipse.ui.part.ViewPart#init(org.eclipse.ui.IViewSite, org.eclipse.ui.IMemento)
   */
  @Override
  public void init( final IViewSite site, final IMemento memento ) throws PartInitException
  {
    super.init( site, memento );
    m_partListener = new PartAdapter()
    {
      /**
       * @see org.kalypso.contribs.eclipse.ui.partlistener.PartAdapter#partActivated(org.eclipse.ui.IWorkbenchPart)
       */
      @Override
      public void partActivated( final IWorkbenchPart part )
      {
        if( (part instanceof AbstractMapPart) && (part instanceof IEditorPart) )
        {
          final IWorkbenchPartSite activatedSite = part.getSite();
          if( activatedSite != null )
          {
            final IWorkbenchWindow workbenchWindow = activatedSite.getWorkbenchWindow();
            if( workbenchWindow != null )
            {
              final IWorkbenchPage activePage = workbenchWindow.getActivePage();
              if( (activePage != null) && activePage.isEditorAreaVisible() )
                setMapPart( (AbstractMapPart) part );
            }
          }
        }
        else if( (part instanceof AbstractMapPart) && (part instanceof IViewPart) )
          setMapPart( (AbstractMapPart) part );
      }

      /**
       * @see org.kalypso.contribs.eclipse.ui.partlistener.PartAdapter#partClosed(org.eclipse.ui.IWorkbenchPart)
       */
      @Override
      public void partClosed( final IWorkbenchPart part )
      {
        if( part instanceof AbstractMapPart )
          if( m_mapPart == part )
            setMapPart( null );
      }
    };
  }

  protected void setMapPart( final AbstractMapPart mapPart )
  {
    if( m_mapPart == mapPart )
      return;

    m_mapPart = mapPart;

    String newName = Messages.GisMapOutlineView_1;
    if( m_mapPart != null )
    {
      newName += " (" + mapPart.getPartName() + ")";
      final JobExclusiveCommandTarget commandTarget = m_mapPart.getCommandTarget();
      if( commandTarget != m_commandTarget )
      {
        m_commandTarget = commandTarget;
        m_viewer.setCommandTarget( commandTarget );
      }
      setMapPanel( (MapPanel) m_mapPart.getAdapter( MapPanel.class ) );
    }
    else
      setMapPanel( null );

    setPartName( newName );
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#dispose()
   */
  @Override
  public void dispose( )
  {
    if( m_viewer != null )
    {
      getSite().setSelectionProvider( null );
      m_viewer.dispose();
    }

    super.dispose();
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
   */
  @Override
  public void setFocus( )
  {
    if( (m_viewer != null) && !m_viewer.getControl().isDisposed() )
      m_viewer.getControl().setFocus();
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModellView#getMapModell()
   */
  public MapPanel getMapPanel( )
  {
    return m_panel;
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModellView#setMapModell(org.kalypso.ogc.gml.mapmodel.IMapModell)
   */
  public void setMapPanel( final MapPanel panel )
  {
    final MapPanel oldPanel = m_panel;

    if( m_panel != null )
      m_panel.removeMapPanelListener( m_mapPanelListener );

    m_panel = panel;

    if( m_panel != null )
    {
      m_panel.addMapPanelListener( m_mapPanelListener );
      m_viewer.setMapModel( m_panel.getMapModell() );
    }
    else
      m_viewer.setMapModel( null );

    fireMapModellViewChanged( oldPanel, panel );
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
  public ISelection getSelection( )
  {
    return m_viewer.getSelection();
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#removeSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
   */
  public void removeSelectionChangedListener( final ISelectionChangedListener listener )
  {
    m_viewer.removeSelectionChangedListener( listener );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#setSelection(org.eclipse.jface.viewers.ISelection)
   */
  public void setSelection( final ISelection selection )
  {
    m_viewer.setSelection( selection );
  }

  /**
   * @param command
   * @param runnable
   * @see org.kalypso.util.command.JobExclusiveCommandTarget#postCommand(org.kalypso.commons.command.ICommand,
   *      java.lang.Runnable)
   */
  public void postCommand( final ICommand command, final Runnable runnable )
  {
    m_commandTarget.postCommand( command, runnable );
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModellView#getMapPart()
   */
  public IWorkbenchPart getMapPart( )
  {
    return m_mapPart;
  }

  protected void handleMapModelChanged( final IMapModell newModel )
  {
    if( m_viewer != null )
      m_viewer.setMapModel( newModel );
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModellView#addMapModellViewListener(org.kalypso.ogc.gml.mapmodel.IMapModellViewListener)
   */
  public void addMapModellViewListener( final IMapModellViewListener l )
  {
    m_mapModellViewListeners.add( l );
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModellView#removeMapModellViewListener(org.kalypso.ogc.gml.mapmodel.IMapModellViewListener)
   */
  public void removeMapModellViewListener( final IMapModellViewListener l )
  {
    m_mapModellViewListeners.remove( l );
  }

  private void fireMapModellViewChanged( final MapPanel oldPanel, final MapPanel newPanel )
  {
    final IMapModellViewListener[] listeners = m_mapModellViewListeners.toArray( new IMapModellViewListener[m_mapModellViewListeners.size()] );
    for( final IMapModellViewListener l : listeners )
    {
      final ISafeRunnable code = new SafeRunnable()
      {
        public void run( ) throws Exception
        {
          l.onMapPanelChanged( GisMapOutlineView.this, oldPanel, newPanel );
        }
      };

      SafeRunner.run( code );
    }
  }
}
