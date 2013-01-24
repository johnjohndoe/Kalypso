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
package org.kalypso.model.wspm.tuhh.core.wspwin.prf;

import java.util.ArrayList;
import java.util.List;

import org.kalypso.wspwin.core.prf.datablock.CoordDataBlock;
import org.kalypso.wspwin.core.prf.datablock.DataBlockHeader;
import org.kalypso.wspwin.core.prf.datablock.IDataBlock;

/**
 * Helper class that creates an coordinate data block.
 * 
 * @author Gernot Belger
 */
public class CoordDataBlockCreator
{
  private final DataBlockHeader m_header;

  private final List<Double> m_xs = new ArrayList<Double>();

  private final List<Double> m_ys = new ArrayList<Double>();

  public CoordDataBlockCreator( final String firstLine, final String secondLine )
  {
    m_header = new DataBlockHeader( firstLine );
    m_header.setSecondLine( secondLine );

  }

  public IDataBlock createDataBlock( )
  {
    final CoordDataBlock db = new CoordDataBlock( m_header );
    final Double[] xArray = m_xs.toArray( new Double[m_xs.size()] );
    final Double[] yArray = m_ys.toArray( new Double[m_ys.size()] );
    db.setCoords( xArray, yArray );
    return db;
  }

  public void add( final double x, final double y )
  {
    m_xs.add( x );
    m_ys.add( y );
  }

}
