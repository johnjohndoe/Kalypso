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
package org.deegree.clients.context;

/**
 * this interface describes the content of an area of a GUI. a GUI area contains
 * zero ... n modules described by the <tt>Module</tt> interface. A GUI area
 * may be can be switched to be invisible. indicated by the hidden attribute.
 * 
 * @version $Revision$
 * @author $author$
 */
public interface GUIArea
{

  public static int WEST = 0;

  public static int EAST = 1;

  public static int SOUTH = 2;

  public static int NORTH = 3;

  public static int CENTER = 4;

  /**
   * returns area (north, west, east ...) assigned to an instance
   * 
   * @return
   */
  int getArea();

  /**
   * sets the name of a module
   * 
   * @param area
   */
  void setArea( int area );

  /**
   * returns true if the GUIArea is hidden.
   * 
   * @return
   */
  boolean isHidden();

  /**
   * sets the GUIArea to be hidden or visible.
   * 
   * @param hidden
   */
  void setHidden( boolean hidden );

  /**
   * 
   * 
   * @param name
   * 
   * @return
   */
  Module getModule( String name );

  /**
   * 
   * 
   * @return
   */
  Module[] getModules();

  /**
   * 
   * 
   * @param mosules
   */
  void setModules( Module[] mosules );

  /**
   * 
   * 
   * @param module
   */
  void addModul( Module module );

  /**
   * 
   * 
   * @param name
   * 
   * @return
   */
  Module removeModule( String name );
}