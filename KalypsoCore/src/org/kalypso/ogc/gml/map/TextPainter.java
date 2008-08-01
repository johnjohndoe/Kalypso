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

import java.awt.Color;
import java.awt.Graphics2D;

/**
 * Paints a centered text.
 * 
 * @author Gernot Belger
 */
public class TextPainter implements IPainter
{
  private final String m_text;

  private final int m_width;

  private final int m_height;

  public TextPainter( final String text, final int width, final int height )
  {
    m_text = text;
    m_width = width;
    m_height = height;
  }

  /**
   * @see org.kalypso.ogc.gml.map.IPainter#paint(java.awt.Graphics2D)
   */
  public void paint( final Graphics2D g )
  {
    final int stringWidth = g.getFontMetrics().stringWidth( m_text );

    g.setColor( Color.white );
    g.fillRect( 0, 0, m_width, m_height );
    g.setColor( Color.black );

    g.drawString( m_text, (m_width - stringWidth) / 2, m_height / 2 );
  }

  /**
   * @see org.kalypso.ogc.gml.map.IPainter#dispose()
   */
  public void dispose( )
  {
    // nothing to dispose
  }
}
