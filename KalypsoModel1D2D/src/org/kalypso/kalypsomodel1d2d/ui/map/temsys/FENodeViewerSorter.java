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

import java.text.CollationKey;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.swt.widgets.TableColumn;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * @author Madanagopal
 *
 */
public class FENodeViewerSorter extends ViewerSorter
{
  private Map sortMap = new HashMap();
    
  
  public FENodeViewerSorter(TableColumn defaultColumn) {
      setCurrentColumn(defaultColumn);
  }
  
  /**
   * Pushs the current sortorder in a map which key is the
   * table-column.
   * @param column
   */
  public void pushSortCriteria(TableColumn column) {
      if (this.sortMap.get(column) == null) {
          this.sortMap.put(column,new Boolean(true));
      }
      else {
          boolean newSort = !((Boolean)this.sortMap.get(column)).booleanValue();
          this.sortMap.put(column,new Boolean(newSort));
      }
  }
  
  /**
   * Asks for the current sort-order and inverts the sort-order
   * @param column the requested column
   * @return true if the sortIndex is descending, else false.
   */
  public boolean isDescending(TableColumn column) {
      boolean returnValue = true;
      if (this.sortMap.get(column) != null) {
          returnValue = ((Boolean)this.sortMap.get(column)).booleanValue();
      } else {
          pushSortCriteria(column);
      }
      return returnValue;
  }
  
  private TableColumn currentColumn = null;
  
  
  
  
  @Override
  public int compare(Viewer viewer, Object obj1, Object obj2) {
      int rc = -1;
      // get the data
      IFE1D2DNode data1 = (IFE1D2DNode) obj1;
      IFE1D2DNode data2 = (IFE1D2DNode) obj2;
      
      CollationKey key1 = null;
      CollationKey key2 = null;
      
      if (this.currentColumn == ((TableViewer)viewer).getTable().getColumn(0)) {
          key1 = getCollator().getCollationKey(getNameOrID(data1));
          key2 = getCollator().getCollationKey(getNameOrID( data2 ));
          
      }
      else if (this.currentColumn == ((TableViewer)viewer).getTable().getColumn(1)){
          key1 = getCollator().getCollationKey(getElevationString( data1 ));
          key2 = getCollator().getCollationKey(getElevationString( data2 ));
      }
      // replace null-strings with empty-strings
      if (key1 == null)
          key1 = getCollator().getCollationKey(""); //$NON-NLS-1$
      
      if (key2 == null)
          key2 = getCollator().getCollationKey(""); //$NON-NLS-1$

      if (isDescending(this.currentColumn)) {
              rc = key1.compareTo(key2);
      }
      else {
              rc = key2.compareTo(key1);
      }
      return rc;
  }
  
  /**
   * Sets the sort column.
   * @param currentColumn The currentColumn to set.
   */
  public void setCurrentColumn(TableColumn currentColumn) {
      this.currentColumn  = currentColumn;
      pushSortCriteria(currentColumn);
  }

  public static final String getNameOrID(IFE1D2DNode node)
  {
    String name = node.getName();
    if(name!=null)
    {
      name=name.trim();
      if(name.length()==0)
      {
        name=node.getGmlID();
      }
      return name;
    }
    else
    {
      return node.getGmlID();
    }
  }
 
  
  public static final String getElevationString(IFE1D2DNode node)
  {
    GM_Point point = node.getPoint();
    if(point.getCoordinateDimension()<=2)
    {
      return String.valueOf( Double.NaN );
    }
    else
    {
      return String.valueOf( point.getZ() );
    }
  }


  
}
