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
package org.kalypso.ui.editor.mapeditor;

import org.kalypso.ogc.gml.map.IMapPanelListener;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.MapPanelAdapter;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.IMapModellListener;
import org.kalypso.ogc.gml.mapmodel.MapModellAdapter;
import org.kalypsodeegree.model.feature.event.ModellEventProviderAdapter;
import org.kalypsodeegree.model.geometry.GM_Envelope;

/**
 * @author Gernot Belger
 */
public class MapPanelModellEventProvider extends ModellEventProviderAdapter
{
  private final IMapPanelListener m_mapPanelListener = new MapPanelAdapter()
  {
    /**
     * @see org.kalypso.ogc.gml.map.MapPanelAdapter#onMapModelChanged(org.kalypso.ogc.gml.map.MapPanel,
     *      org.kalypso.ogc.gml.mapmodel.IMapModell, org.kalypso.ogc.gml.mapmodel.IMapModell)
     */
    @Override
    public void onMapModelChanged( MapPanel source, IMapModell oldModel, IMapModell newModel )
    {
      setModel( newModel );
    }

    /**
     * @see org.kalypso.ogc.gml.map.MapPanelAdapter#onExtentChanged(org.kalypso.ogc.gml.map.MapPanel,
     *      org.kalypsodeegree.model.geometry.GM_Envelope, org.kalypsodeegree.model.geometry.GM_Envelope)
     */
    @Override
    public void onExtentChanged( MapPanel source, GM_Envelope oldExtent, GM_Envelope newExtent )
    {
      fireModellEvent( null );
    }
  };

  private final IMapModellListener m_mapModellListener = new MapModellAdapter()
  {
    /**
     * @see org.kalypso.ogc.gml.mapmodel.MapModellAdapter#repaintRequested(org.kalypso.ogc.gml.mapmodel.IMapModell,
     *      org.kalypsodeegree.model.geometry.GM_Envelope)
     */
    @Override
    public void repaintRequested( final IMapModell source, final GM_Envelope bbox )
    {
      fireModellEvent( null );
    }
  };

  private final MapPanel m_panel;

  private IMapModell m_mapModell;

  public MapPanelModellEventProvider( final MapPanel panel )
  {
    m_panel = panel;

    m_panel.addMapPanelListener( m_mapPanelListener );

    setModel( m_panel.getMapModell() );
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventProvider#dispose()
   */
  @Override
  public void dispose( )
  {
    m_panel.removeMapPanelListener( m_mapPanelListener );
  }

  protected void setModel( final IMapModell mapModell )
  {
    if( m_mapModell != null )
      m_mapModell.removeMapModelListener( m_mapModellListener );

    m_mapModell = mapModell;

    if( m_mapModell != null )
      m_mapModell.addMapModelListener( m_mapModellListener );

    fireModellEvent( null );
  }

}
