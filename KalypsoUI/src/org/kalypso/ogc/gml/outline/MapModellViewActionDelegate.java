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

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IViewActionDelegate;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.actions.ActionDelegate;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.listeners.IMapPanelListener;
import org.kalypso.ogc.gml.map.listeners.MapPanelAdapter;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.IMapModellListener;
import org.kalypso.ogc.gml.mapmodel.IMapModellView;
import org.kalypso.ogc.gml.mapmodel.IMapModellViewListener;
import org.kalypsodeegree.model.geometry.GM_Envelope;

/**
 * TODO: merge into PluginMapOutlineAction
 * 
 * @author Stefan Kurzbach
 */
public abstract class MapModellViewActionDelegate extends ActionDelegate implements IViewActionDelegate, PluginMapOutlineActionDelegate, IMapModellViewListener
{
  private final IMapPanelListener m_mapPanelListener = new MapPanelAdapter()
  {
    /**
     * @see org.kalypso.ogc.gml.map.MapPanelAdapter#onMapModelChanged(org.kalypso.ogc.gml.map.MapPanel,
     *      org.kalypso.ogc.gml.mapmodel.IMapModell, org.kalypso.ogc.gml.mapmodel.IMapModell)
     */
    @Override
    public void onMapModelChanged( final MapPanel source, final IMapModell oldModel, final IMapModell newModel )
    {
      setMapModell( newModel );
    }
  };

  private final IMapModellListener m_mapModellListener = new IMapModellListener()
  {
    public void repaintRequested( IMapModell source, GM_Envelope bbox )
    {
      handleModellChanged();
    }

    public void themeActivated( IMapModell source, IKalypsoTheme previouslyActive, IKalypsoTheme nowActive )
    {
      handleModellChanged();
    }

    public void themeAdded( IMapModell source, IKalypsoTheme theme )
    {
      handleModellChanged();
    }

    public void themeContextChanged( IMapModell source, IKalypsoTheme theme )
    {
      handleModellChanged();
    }

    public void themeOrderChanged( IMapModell source )
    {
      handleModellChanged();
    }

    public void themeRemoved( IMapModell source, IKalypsoTheme theme, boolean lastVisibility )
    {
      handleModellChanged();
    }

    public void themeStatusChanged( IMapModell source, IKalypsoTheme theme )
    {
      handleModellChanged();
    }

    public void themeVisibilityChanged( IMapModell source, IKalypsoTheme theme, boolean visibility )
    {
      handleModellChanged();
    }
  };

  private IMapModellView m_view;

  private ISelection m_selection;

  private MapPanel m_panel;

  private IMapModell m_modell;

  private IAction m_action;

  public IMapModellView getView( )
  {
    return m_view;
  }

  /**
   * @see org.eclipse.ui.actions.ActionDelegate#init(org.eclipse.jface.action.IAction)
   */
  @Override
  public void init( final IAction action )
  {
    m_action = action;
  }

  /**
   * @see org.eclipse.ui.actions.ActionDelegate#dispose()
   */
  @Override
  public void dispose( )
  {
    if( m_view != null )
      m_view.removeMapModellViewListener( this );

    if( m_panel != null )
      m_panel.removeMapPanelListener( m_mapPanelListener );

    if( m_modell != null )
      m_modell.removeMapModelListener( m_mapModellListener );

    m_view = null;
    m_panel = null;
    m_modell = null;

    super.dispose();
  }

  public void setView( final IMapModellView view )
  {
    if( m_view != null )
      m_view.removeMapModellViewListener( this );

    m_view = view;

    if( m_view != null )
      m_view.addMapModellViewListener( this );

    onMapPanelChanged( view, null, view.getMapPanel() );
  }

  protected ISelection getSelection( )
  {
    return m_selection;
  }

  public static IKalypsoTheme[] getSelectedThemes( final ISelection selection )
  {
    final List<IKalypsoTheme> themes = new ArrayList<IKalypsoTheme>();

    if( selection instanceof IStructuredSelection )
    {
      final IStructuredSelection s = (IStructuredSelection) selection;
      final Object[] elements = s.toArray();
      for( final Object element : elements )
      {
        if( element instanceof IKalypsoTheme )
          themes.add( (IKalypsoTheme) element );
      }

    }

    return themes.toArray( new IKalypsoTheme[themes.size()] );
  }

  /**
   * @see org.eclipse.ui.IViewActionDelegate#init(org.eclipse.ui.IViewPart)
   */
  public void init( final IViewPart view )
  {
    if( view instanceof IMapModellView )
    {
      setView( (IMapModellView) view );
    }
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  @Override
  public void selectionChanged( final IAction action, final ISelection selection )
  {
    m_action = action;
    m_selection = selection;
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModellViewListener#onMapPanelChanged(org.kalypso.ogc.gml.mapmodel.IMapModellView,
   *      org.kalypso.ogc.gml.map.MapPanel, org.kalypso.ogc.gml.map.MapPanel)
   */
  public void onMapPanelChanged( final IMapModellView source, final MapPanel oldPanel, final MapPanel newPanel )
  {
    /* Register as listener to the current map-modell, on every change, give actions chance to refresh */

    if( m_panel != null )
      m_panel.removeMapPanelListener( m_mapPanelListener );

    m_panel = newPanel;

    if( m_panel != null )
      m_panel.addMapPanelListener( m_mapPanelListener );

    setMapModell( m_panel == null ? null : m_panel.getMapModell() );
  }

  protected final void setMapModell( final IMapModell mapModell )
  {
    if( m_modell != null )
      m_modell.removeMapModelListener( m_mapModellListener );

    m_modell = mapModell;

    if( m_modell != null )
      m_modell.addMapModelListener( m_mapModellListener );
  }

  protected void handleModellChanged( )
  {
    /* Just call selectionChanged, so the action will recalculate if it is still enabled. */
    selectionChanged( m_action, getSelection() );
  }

}