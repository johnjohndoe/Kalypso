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
package org.kalypso.ogc.gml.widgets.aew;

import java.awt.Color;
import java.awt.Graphics;

import org.kalypso.ogc.gml.widgets.tools.IPointHighLighter;

/**
 * @author kuch
 *
 */
public interface IAdvancedEditWidgetDelegate
{
   static final IPointHighLighter VERTEX = new IPointHighLighter()
  {
    Color cVertex = new Color( 0x3e, 0x79, 0xd9 );

    @Override
    public void draw( final Graphics g, final java.awt.Point point )
    {
      final Color original = g.getColor();
      g.setColor( cVertex );
      g.drawRect( point.x - 6 / 2, point.y - 6 / 2, 6, 6 );
      g.setColor( original );
    }
  };

   static final IPointHighLighter SNAP = new IPointHighLighter()
  {
    Color cSnap = new Color( 0x40, 0xde, 0x28 );

    int size = 10;

    @Override
    public void draw( final Graphics g, final java.awt.Point point )
    {
      final Color original = g.getColor();
      g.setColor( cSnap );
      g.fillOval( point.x - size / 2, point.y - size / 2, size, size );
      g.setColor( original );
    }
  };

   static final IPointHighLighter MOVED_SNAP_POINT = new IPointHighLighter()
  {
    Color cSnap = new Color( 0x31, 0x47, 0xa0, 128 );

    int size = 5;

    @Override
    public void draw( final Graphics g, final java.awt.Point point )
    {
      final Color original = g.getColor();
      g.setColor( cSnap );
      g.fillOval( point.x - size / 2, point.y - size / 2, size, size );
      g.setColor( original );
    }
  };
  
  
  public void paint( final Graphics g );
}
