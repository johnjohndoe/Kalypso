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
package org.kalypso.ui.views.map;

import java.awt.Point;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbenchPart;
import org.kalypso.contribs.eclipse.ui.partlistener.AdapterPartListener;
import org.kalypso.contribs.eclipse.ui.partlistener.EditorFirstAdapterFinder;
import org.kalypso.contribs.eclipse.ui.partlistener.IAdapterEater;
import org.kalypso.contribs.eclipse.ui.partlistener.IAdapterFinder;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.listeners.IMapPanelListener;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ui.editor.mapeditor.AbstractMapPart;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * This view displays the last active map-part in an overview window.
 * 
 * @author Gernot Belger
 */
public class MapOverView extends AbstractMapPart implements IAdapterEater<MapPanel>, IMapPanelListener, IViewPart
{
  private final IAdapterFinder<MapPanel> m_closeFinder = new EditorFirstAdapterFinder<MapPanel>();

  private final IAdapterFinder<MapPanel> m_initFinder = m_closeFinder;

  private final AdapterPartListener<MapPanel> m_adapterListener = new AdapterPartListener<MapPanel>( MapPanel.class, this, m_initFinder, m_closeFinder );

  private MapPanel m_panel;

  /**
   * @see org.kalypso.ui.editor.mapeditor.AbstractMapPart#init(org.eclipse.ui.IViewSite)
   */
  @Override
  public void init( final IViewSite site )
  {
    super.init( site );

    site.getPage().addPartListener( m_adapterListener );
  }

  /**
   * @see org.eclipse.ui.IViewPart#init(org.eclipse.ui.IViewSite, org.eclipse.ui.IMemento)
   */
  public void init( final IViewSite site, final IMemento memento )
  {
    init( site );
  }

  /**
   * @see org.eclipse.ui.IViewPart#saveState(org.eclipse.ui.IMemento)
   */
  public void saveState( final IMemento memento )
  {
  }

  /**
   * @see org.kalypso.ui.editor.mapeditor.AbstractMapPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createPartControl( final Composite parent )
  {
    super.createPartControl( parent );
  }

  /**
   * @see org.kalypso.ui.editor.mapeditor.AbstractMapPart#dispose()
   */
  @Override
  public void dispose( )
  {
    super.dispose();

    if( m_panel != null )
      m_panel.removeMapPanelListener( this );
  }

  /**
   * @see org.kalypso.contribs.eclipse.ui.partlistener.IAdapterEater#setAdapter(org.eclipse.ui.IWorkbenchPart,
   *      java.lang.Object)
   */
  public void setAdapter( final IWorkbenchPart part, final MapPanel adapter )
  {
    if( m_panel != null )
      m_panel.removeMapPanelListener( this );

    m_panel = adapter;

    if( m_panel != null )
    {
      m_panel.addMapPanelListener( this );
      setModell( m_panel.getMapModell() );
    }
  }

  private void setModell( final IMapModell mapModell )
  {
    final GisTemplateMapModell modelToSet;
    if( mapModell instanceof GisTemplateMapModell )
      modelToSet = (GisTemplateMapModell) mapModell;
    else
      modelToSet = null;

    final IMapModell oldMapModell = getMapPanel().getMapModell();
    if( oldMapModell != modelToSet )
    {
      setMapModell( modelToSet );
      if( modelToSet != null )
        getMapPanel().setBoundingBox( modelToSet.getFullExtentBoundingBox() );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.map.IMapPanelListener#onExtentChanged(org.kalypso.ogc.gml.map.MapPanel,
   *      org.kalypsodeegree.model.geometry.GM_Envelope, org.kalypsodeegree.model.geometry.GM_Envelope)
   */
  public void onExtentChanged( final MapPanel source, final GM_Envelope oldExtent, final GM_Envelope newExtent )
  {
    // TODO Auto-generated method stub

    // TODO: update extent layer
  }

  /**
   * @see org.kalypso.ogc.gml.map.IMapPanelListener#onMapModelChanged(org.kalypso.ogc.gml.map.MapPanel,
   *      org.kalypso.ogc.gml.mapmodel.IMapModell, org.kalypso.ogc.gml.mapmodel.IMapModell)
   */
  public void onMapModelChanged( final MapPanel source, final IMapModell oldModel, final IMapModell newModel )
  {
    setModell( newModel );
  }

  /**
   * @see org.kalypso.ogc.gml.map.IMapPanelListener#onMessageChanged(org.kalypso.ogc.gml.map.MapPanel, java.lang.String)
   */
  public void onMessageChanged( final MapPanel source, final String message )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.ogc.gml.map.listeners.IMapPanelListener#onMouseMoveEvent(org.kalypso.ogc.gml.map.MapPanel,
   *      java.awt.Point)
   */
  public void onMouseMoveEvent( final MapPanel source, final GM_Point gmPoint, final Point mousePosition )
  {
    // TODO Auto-generated method stub

  }
}
