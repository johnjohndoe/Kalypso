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
package org.kalypso.ui.rrm.internal.utils.featureTree;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerComparator;
import org.joda.time.Period;
import org.joda.time.Seconds;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.model.hydrology.timeseries.binding.ITimeseries;

/**
 * @author Gernot Belger
 */
public class TreeNodeLabelComparator extends ViewerComparator
{
  @Override
  public int compare( final Viewer viewer, final Object e1, final Object e2 )
  {
    final TreeNode n1 = (TreeNode) e1;
    final TreeNode n2 = (TreeNode) e2;

    final ITimeseries t1 = (ITimeseries) n1.getAdapter( ITimeseries.class );
    final ITimeseries t2 = (ITimeseries) n2.getAdapter( ITimeseries.class );
    if( Objects.allNotNull( t1, t2 ) )
      return comparteTimeseries( t1, t2 );

    final String l1 = n1.getLabel();
    final String l2 = n2.getLabel();

    return l1.compareTo( l2 );
  }

  private int comparteTimeseries( final ITimeseries t1, final ITimeseries t2 )
  {
    final Period s1 = t1.getTimestep();
    final Period s2 = t2.getTimestep();
    if( Objects.allNotNull( s1, s2 ) )
    {
      final Seconds sec1 = s1.toStandardSeconds();
      final Seconds sec2 = s2.toStandardSeconds();

      final int compared = sec1.compareTo( sec2 );
      if( compared != 0 )
        return compared;
    }

    final String q1 = Objects.firstNonNull( t1.getQuality(), StringUtils.EMPTY );
    final String q2 = Objects.firstNonNull( t2.getQuality(), StringUtils.EMPTY );

    return q1.compareTo( q2 );
  }
}