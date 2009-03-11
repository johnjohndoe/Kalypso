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
package org.kalypso.ogc.gml.mapmodel;

import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypsodeegree.model.geometry.GM_Envelope;

/**
 * Default implementation of {@link IMapModellListener}. All methods are empty.
 * 
 * @author Gernot Belger
 */
public abstract class MapModellAdapter implements IMapModellListener
{
  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModellListener#repaintRequested(org.kalypso.ogc.gml.mapmodel.IMapModell,
   *      org.kalypsodeegree.model.geometry.GM_Envelope)
   */
  public void repaintRequested( final IMapModell source, final GM_Envelope bbox )
  {
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModellListener#themeActivated(org.kalypso.ogc.gml.mapmodel.IMapModell,
   *      org.kalypso.ogc.gml.IKalypsoTheme, org.kalypso.ogc.gml.IKalypsoTheme)
   */
  public void themeActivated( final IMapModell source, final IKalypsoTheme previouslyActive, final IKalypsoTheme nowActive )
  {
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModellListener#themeAdded(org.kalypso.ogc.gml.mapmodel.IMapModell,
   *      org.kalypso.ogc.gml.IKalypsoTheme)
   */
  public void themeAdded( final IMapModell source, final IKalypsoTheme theme )
  {
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModellListener#themeOrderChanged(org.kalypso.ogc.gml.mapmodel.IMapModell)
   */
  public void themeOrderChanged( final IMapModell source )
  {
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModellListener#themeRemoved(org.kalypso.ogc.gml.mapmodel.IMapModell,
   *      org.kalypso.ogc.gml.IKalypsoTheme)
   */
  public void themeRemoved( final IMapModell source, final IKalypsoTheme theme, final boolean lastVisibility )
  {
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModellListener#themeVisibilityChanged(org.kalypso.ogc.gml.mapmodel.IMapModell,
   *      org.kalypso.ogc.gml.IKalypsoTheme, boolean)
   */
  public void themeVisibilityChanged( final IMapModell source, final IKalypsoTheme theme, final boolean visibility )
  {
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModellListener#contextChanged(org.kalypso.ogc.gml.mapmodel.IMapModell)
   */
  public void themeContextChanged( final IMapModell source, final IKalypsoTheme theme )
  {
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModellListener#themeStatusChanged(org.kalypso.ogc.gml.mapmodel.IMapModell,
   *      org.kalypso.ogc.gml.IKalypsoTheme)
   */
  public void themeStatusChanged( final IMapModell source, final IKalypsoTheme theme )
  {
  }
}
