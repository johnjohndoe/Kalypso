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
package org.kalypso.kalypsomodel1d2d.ui.map.temsys;

import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DNode;

/**
 * @author madanago
 *
 */
public class FENodeViewerSorter extends ViewerSorter
{

    private static final int ASCENDING = 0;

    private static final int DESCENDING = 1;

    private int column;

    private int direction;

    /**
     * Does the sort. If it's a different column from the previous sort, do an
     * ascending sort. If it's the same column as the last sort, toggle the sort
     * direction.
     * 
     * @param column
     */
    public void doSort(int column) {
      if (column == this.column) {
        // Same column as last sort; toggle the direction
        direction = 1 - direction;
      } else {
        // New column; do an ascending sort
        this.column = column;
        direction = ASCENDING;
      }
    }

    /**
     * Compares the object for sorting
     */
    public int compare(Viewer viewer, IFE1D2DNode p1, IFE1D2DNode p2) {
      int rc = 0;
      // Determine which column and do the appropriate sort
      switch (column) {
      case 0:
        rc = collator.compare(FENodeLabelProvider.getNameOrID(p1),
                              FENodeLabelProvider.getNameOrID(p2));
        break;
      case 1:
        rc = collator.compare(FENodeLabelProvider.getElevationString(p1),
                              FENodeLabelProvider.getElevationString(p2));
        break;
      }
      // If descending order, flip the direction
      if (direction == DESCENDING)
        rc = -rc;
      return rc;

  }
}
