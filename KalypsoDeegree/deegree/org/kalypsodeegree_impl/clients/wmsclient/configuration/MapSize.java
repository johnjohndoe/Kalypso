/*----------------    FILE HEADER  ------------------------------------------

 This file is part of deegree.
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon Fitzke/Fretter/Poth GbR
 http://www.lat-lon.de

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

 Andreas Poth
 lat/lon Fitzke/Fretter/Poth GbR
 Meckenheimer Allee 176
 53115 Bonn
 Germany
 E-Mail: poth@lat-lon.de

 Jens Fitzke
 Department of Geography
 University of Bonn
 Meckenheimer Allee 166
 53115 Bonn
 Germany
 E-Mail: jens.fitzke@uni-bonn.de

 
 ---------------------------------------------------------------------------*/
package org.deegree_impl.clients.wmsclient.configuration;

/**
 * 
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 */
public class MapSize
{
  private boolean selected = false;

  private boolean free = false;

  private int height = 0;

  private int width = 0;

  /** Creates a new instance of MapSize */
  public MapSize( int width, int height, boolean selected, boolean free )
  {
    this.width = width;
    this.height = height;
    this.selected = selected;
    this.free = free;
  }

  /**
   * returns the map width.
   * 
   * @return
   */
  public int getWidth()
  {
    return width;
  }

  /**
   * returns the map height.
   * 
   * @return
   */
  public int getHeight()
  {
    return height;
  }

  /**
   * returns true if the map size is selected. the user have to ensure that only
   * one map size is in selected state
   * 
   * @return
   */
  public boolean isSelected()
  {
    return selected;
  }

  /**
   * 
   * 
   * @param selected
   */
  public void setSelected( boolean selected )
  {
    this.selected = selected;
  }

  /**
   * returns true if the <tt>MapSize</tt> represents a field (combination)
   * that allows the user to enter his own map size
   */
  public boolean isFree()
  {
    return free;
  }

}