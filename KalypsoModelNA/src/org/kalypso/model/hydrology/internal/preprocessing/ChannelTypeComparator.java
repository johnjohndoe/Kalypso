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
package org.kalypso.model.hydrology.internal.preprocessing;

import java.util.Comparator;

import org.kalypso.model.hydrology.binding.model.Channel;
import org.kalypso.model.hydrology.binding.model.KMChannel;
import org.kalypso.model.hydrology.binding.model.StorageChannel;
import org.kalypso.model.hydrology.binding.model.VirtualChannel;
import org.kalypso.model.hydrology.internal.IDManager;
import org.kalypso.model.hydrology.internal.preprocessing.net.NetElement;

/**
 * Sorts channels by type and then by ascii id
 * 
 * @author Gernot Belger
 */
public class ChannelTypeComparator implements Comparator<NetElement>
{
  private final IDManager m_idManager;

  public ChannelTypeComparator( final IDManager idManager )
  {
    m_idManager = idManager;
  }

  /**
   * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
   */
  @Override
  public int compare( final NetElement o1, final NetElement o2 )
  {
    final Channel c1 = o1.getChannel();
    final Channel c2 = o2.getChannel();

    if( c1.getClass() == c2.getClass() )
      return compareById( c1, c2 );

    final int i1 = getSortOrdinal( c1 );
    final int i2 = getSortOrdinal( c2 );

    final int diff = i1 - i2;
    if( diff == 0 )
      return compareById( c1, c2 );

    return diff;
  }

  private int compareById( final Channel o1, final Channel o2 )
  {
    final int id1 = m_idManager.getAsciiID( o1 );
    final int id2 = m_idManager.getAsciiID( o2 );
    return id1 - id2;
  }

  private int getSortOrdinal( final Channel channel )
  {
    if( channel instanceof VirtualChannel )
      return 1;

    if( channel instanceof StorageChannel )
      return 2;

    if( channel instanceof KMChannel )
      return 3;


    return 0;
  }

}
