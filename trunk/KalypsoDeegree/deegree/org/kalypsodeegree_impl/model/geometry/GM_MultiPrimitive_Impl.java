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
package org.deegree_impl.model.geometry;

import java.io.Serializable;

import org.deegree.model.geometry.GM_Aggregate;
import org.deegree.model.geometry.GM_Exception;
import org.deegree.model.geometry.GM_MultiPrimitive;
import org.deegree.model.geometry.GM_Primitive;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * default implementation of the GM_MultiPrimitive interface of package
 * jago.model.
 * 
 * <p>
 * ------------------------------------------------------------
 * </p>
 * 
 * @version 5.6.2001
 * @author Andreas Poth
 *         <p>
 */
class GM_MultiPrimitive_Impl extends GM_Aggregate_Impl implements GM_MultiPrimitive, Serializable
{
  /** Use serialVersionUID for interoperability. */
  private final static long serialVersionUID = 7228377539686274411L;

  /**
   * Creates a new GM_MultiPrimitive_Impl object.
   * 
   * @param crs
   */
  public GM_MultiPrimitive_Impl( CS_CoordinateSystem crs )
  {
    super( crs );
  }

  /**
   * merges this aggregation with another one
   * 
   * @exception GM_Exception
   *              will be thrown if the submitted isn't the same type as the
   *              recieving one.
   */
  public void merge( GM_Aggregate aggregate ) throws GM_Exception
  {
    if( !( aggregate instanceof GM_MultiPrimitive ) )
    {
      throw new GM_Exception( "The submitted aggregation isn't a GM_MultiPrimitive" );
    }

    super.merge( aggregate );
  }

  /**
   * returns the GM_Primitive at the submitted index.
   */
  public GM_Primitive getPrimitiveAt( int index )
  {
    return (GM_Primitive)super.getObjectAt( index );
  }

  /**
   * returns all GM_Primitives as array
   */
  public GM_Primitive[] getAllPrimitives()
  {
    GM_Primitive[] gmos = new GM_Primitive[this.getSize()];

    return (GM_Primitive[])aggregate.toArray( gmos );
  }

  protected void calculateParam()
  {}

  public int getCoordinateDimension()
  {
    return -1;
  }

  public int getDimension()
  {
    return 2;
  }

}