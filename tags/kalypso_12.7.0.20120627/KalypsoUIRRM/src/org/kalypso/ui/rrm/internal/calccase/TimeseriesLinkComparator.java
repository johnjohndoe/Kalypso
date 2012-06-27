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
package org.kalypso.ui.rrm.internal.calccase;

import java.util.Comparator;

import org.kalypso.zml.obslink.TimeseriesLinkType;

/**
 * This comparator compares timeseries links by their href.
 * 
 * @author Holger Albert
 */
public class TimeseriesLinkComparator implements Comparator<TimeseriesLinkType>
{
  /**
   * The constructor.
   */
  public TimeseriesLinkComparator( )
  {
  }

  @Override
  public int compare( final TimeseriesLinkType o1, final TimeseriesLinkType o2 )
  {
    final String href1 = o1.getHref();
    final String href2 = o2.getHref();

    if( href1 == null && href2 == null )
      return 0;

    if( href1 == null && href2 != null )
      return -1;

    if( href1 != null && href2 == null )
      return 1;

    return href1.compareTo( href2 );
  }
}