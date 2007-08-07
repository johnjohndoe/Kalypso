/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.ogc.gml.map.listeners;

import java.awt.Point;

import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * Base implementation for the {@link IMapPanelListener}, implements everything empty.
 * 
 * @author Gernot Belger
 */
public abstract class MapPanelAdapter implements IMapPanelListener
{
  /**
   * @see org.kalypso.ogc.gml.map.IMapPanelListener#onExtentChanged(org.kalypso.ogc.gml.map.MapPanel,
   *      org.kalypsodeegree.model.geometry.GM_Envelope, org.kalypsodeegree.model.geometry.GM_Envelope)
   */
  public void onExtentChanged( final MapPanel source, final GM_Envelope oldExtent, final GM_Envelope newExtent )
  {
  }

  /**
   * @see org.kalypso.ogc.gml.map.IMapPanelListener#onMapModelChanged(org.kalypso.ogc.gml.map.MapPanel,
   *      org.kalypso.ogc.gml.mapmodel.IMapModell, org.kalypso.ogc.gml.mapmodel.IMapModell)
   */
  public void onMapModelChanged( final MapPanel source, final IMapModell oldModel, final IMapModell newModel )
  {
  }

  /**
   * @see org.kalypso.ogc.gml.map.IMapPanelListener#onMessageChanged(org.kalypso.ogc.gml.map.MapPanel, java.lang.String)
   */
  public void onMessageChanged( final MapPanel source, final String message )
  {
  }

  /**
   * @see org.kalypso.ogc.gml.map.IMapPanelListener#onMouseMoveEvent(org.kalypso.ogc.gml.map.MapPanel,
   *      org.eclipse.swt.graphics.Point)
   */
  public void onMouseMoveEvent( final MapPanel source, final GM_Point gmPoint, final Point mousePosition )
  {
  }
}
