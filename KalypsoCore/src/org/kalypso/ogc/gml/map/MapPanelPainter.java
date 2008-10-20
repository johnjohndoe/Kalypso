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
package org.kalypso.ogc.gml.map;

import java.awt.Graphics2D;
import java.awt.image.BufferedImage;

/**
 * Helper class, handles painting of the map
 * 
 * @author Gernot Belger
 */
public class MapPanelPainter
{
  private MapModellPainter m_normalPainter = null;

  private MapModellPainter m_selectionPainter = null;

  private final IMapPanel m_mapPanel;

  public MapPanelPainter( final IMapPanel mapPanel )
  {
    m_mapPanel = mapPanel;
  }

  public synchronized void dispose( )
  {
    if( m_normalPainter != null )
      m_normalPainter.dispose();

    if( m_selectionPainter != null )
      m_selectionPainter.dispose();
  }

  public synchronized void invalidate( final boolean onlySelection )
  {
    /* Determine painter depending on state of model. */
    if( !onlySelection )
    {
      /* Cancel old job if still running. */
      if( m_normalPainter != null )
      {
        m_normalPainter.dispose();
        m_normalPainter = null;
      }

      m_normalPainter = new MapModellPainter( m_mapPanel, false );
      // delay the Schedule, so if another invalidate comes within that time-span, no repaint happens at all
      m_normalPainter.schedule( 250 );
    }

    if( m_selectionPainter != null )
    {
      m_selectionPainter.dispose();
      m_selectionPainter = null;
    }

    /* Selection always get repainted */
    m_selectionPainter = new MapModellPainter( m_mapPanel, true );
    // delay the Schedule, so if another invalidate comes within that time-span, no repaint happens at all
    m_selectionPainter.schedule( 250 );
  }

  public synchronized void paint( final Graphics2D g )
  {
    if( m_normalPainter != null )
      m_normalPainter.paint( g );

    if( m_selectionPainter != null )
      m_selectionPainter.paint( g );
  }

  public synchronized void cancel( )
  {
    if( m_normalPainter != null )
      m_normalPainter.cancel();
    if( m_selectionPainter != null )
      m_selectionPainter.cancel();
  }

  public BufferedImage getNormalImage( )
  {
    if( m_normalPainter == null )
      return null;

    return m_normalPainter.getImage();
  }

}
