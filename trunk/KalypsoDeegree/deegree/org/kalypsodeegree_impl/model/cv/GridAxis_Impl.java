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

package org.deegree_impl.model.cv;

import org.deegree.model.coverage.GridAxis;

/**
 * describts the axis of a grid
 * 
 * <p>
 * -----------------------------------------------------------------------
 * </p>
 * 
 * @author Andreas Poth
 * @version $Revision$ $Date$
 *          <p>
 */
public class GridAxis_Impl implements GridAxis
{

  private String description = null;

  private String name = null;

  private int orientation = 0;

  public GridAxis_Impl( String name, String description, int orientation )
      throws InvalidAxisDefinitionException
  {
    if( orientation < 0 || orientation > 6 )
    {
      throw new InvalidAxisDefinitionException( "orientation must have a "
          + "a value between 0 and 6" );
    }
    this.name = name;
    this.description = description;
    this.orientation = orientation;
  }

  /**
   * returns a short description of the axis
   *  
   */
  public String getDescription()
  {
    return description;
  }

  /**
   * returns the name of the axis
   *  
   */
  public String getName()
  {
    return name;
  }

  /**
   * returns the axis orientation
   *  
   */
  public int getOrientation()
  {
    return orientation;
  }

}