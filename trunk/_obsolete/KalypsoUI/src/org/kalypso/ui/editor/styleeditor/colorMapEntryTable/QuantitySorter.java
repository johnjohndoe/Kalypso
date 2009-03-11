/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ui.editor.styleeditor.colorMapEntryTable;

import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;
import org.kalypsodeegree.graphics.sld.ColorMapEntry;

public class QuantitySorter extends ViewerSorter
{

  public QuantitySorter( )
  {
    super();
  }

  @Override
  public int compare( Viewer viewer, Object o1, Object o2 )
  {
    final ColorMapEntry entry1 = (ColorMapEntry) o1;
    final ColorMapEntry entry2 = (ColorMapEntry) o2;

    return compareQuantities( entry1, entry2 );

  }

  private int compareQuantities( ColorMapEntry entry1, ColorMapEntry entry2 )
  {
    double test = entry1.getQuantity() - entry2.getQuantity();
    int result = test < 0 ? -1 : (test > 0) ? 1 : 0;
    return result;
  }

}