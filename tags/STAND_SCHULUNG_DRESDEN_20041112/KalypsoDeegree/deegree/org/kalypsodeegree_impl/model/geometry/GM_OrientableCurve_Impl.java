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

import org.deegree.model.geometry.GM_Exception;
import org.deegree.model.geometry.GM_OrientableCurve;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * default implementierung of the GM_OrientableSurface interface from package
 * jago.model. the implementation is abstract because only initialization of the
 * spatial reference system is unique to all orientated curves
 * 
 * <p>
 * ------------------------------------------------------------
 * </p>
 * 
 * @version 11.6.2001
 * @author Andreas Poth
 *         <p>
 */
abstract class GM_OrientableCurve_Impl extends GM_OrientablePrimitive_Impl implements
    GM_OrientableCurve, Serializable
{
  /** Use serialVersionUID for interoperability. */
  private final static long serialVersionUID = -133652834691775077L;

  /**
   * Creates a new GM_OrientableCurve_Impl object.
   * 
   * @param crs
   * 
   * @throws GM_Exception
   */
  protected GM_OrientableCurve_Impl( CS_CoordinateSystem crs ) throws GM_Exception
  {
    super( crs, '+' );
  }

  /**
   * Creates a new GM_OrientableCurve_Impl object.
   * 
   * @param crs
   * @param orientation
   * 
   * @throws GM_Exception
   */
  protected GM_OrientableCurve_Impl( CS_CoordinateSystem crs, char orientation )
      throws GM_Exception
  {
    super( crs, orientation );
  }
}