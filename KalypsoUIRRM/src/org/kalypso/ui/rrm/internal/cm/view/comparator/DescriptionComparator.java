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
package org.kalypso.ui.rrm.internal.cm.view.comparator;

import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerComparator;
import org.kalypso.model.rcm.binding.IRainfallGenerator;
import org.kalypso.ui.rrm.internal.cm.view.CatchmentBean;

/**
 * A viewer comparator.
 * 
 * @author Holger Albert
 */
public class DescriptionComparator extends ViewerComparator
{
  /**
   * The constructor.
   */
  public DescriptionComparator( )
  {
  }

  @Override
  public int compare( final Viewer viewer, final Object e1, final Object e2 )
  {
    if( e1 instanceof CatchmentBean && e2 instanceof CatchmentBean )
    {
      final CatchmentBean c1 = (CatchmentBean) e1;
      final CatchmentBean c2 = (CatchmentBean) e2;

      final String catchmentDescription1 = c1.getCatchmentDescription();
      final String catchmentDescription2 = c2.getCatchmentDescription();

      if( catchmentDescription1 == null && catchmentDescription2 != null )
        return -1;
      else if( catchmentDescription1 != null && catchmentDescription2 == null )
        return 1;
      else if( catchmentDescription1 == null && catchmentDescription2 == null )
        return 0;

      return catchmentDescription1.compareTo( catchmentDescription2 );
    }

    if( e1 instanceof IRainfallGenerator && e2 instanceof IRainfallGenerator )
    {
      final IRainfallGenerator g1 = (IRainfallGenerator) e1;
      final IRainfallGenerator g2 = (IRainfallGenerator) e2;

      final String description1 = g1.getDescription();
      final String description2 = g2.getDescription();

      if( description1 == null && description2 != null )
        return -1;
      else if( description1 != null && description2 == null )
        return 1;
      else if( description1 == null && description2 == null )
        return 0;

      return description1.compareTo( description2 );
    }

    return super.compare( viewer, e1, e2 );
  }
}