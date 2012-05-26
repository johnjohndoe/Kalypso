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
package org.kalypso.ui.rrm.internal.cm.view;

import java.util.Comparator;

import org.kalypso.model.hydrology.binding.timeseriesMappings.TimeseriesMappingType;

/**
 * @author Gernot Belger
 */
public final class TimeseriesMappingElementsComparator implements Comparator<Object>
{
  @Override
  public int compare( final Object o1, final Object o2 )
  {
    final int cat1 = getCategory( o1 );
    final int cat2 = getCategory( o2 );

    if( cat1 != cat2 )
      return cat1 - cat2;

    if( o1 instanceof String )
    {
      final String s1 = (String) o1;
      final String s2 = (String) o2;
      return s1.compareTo( s2 );
    }

    final TimeseriesMappingType t1 = (TimeseriesMappingType) o1;
    final TimeseriesMappingType t2 = (TimeseriesMappingType) o2;

    return t1.ordinal() - t2.ordinal();
  }

  private int getCategory( final Object o1 )
  {
    if( o1 instanceof String )
      return 0;

    if( o1 instanceof TimeseriesMappingType )
      return 1;

    throw new IllegalStateException();
  }
}