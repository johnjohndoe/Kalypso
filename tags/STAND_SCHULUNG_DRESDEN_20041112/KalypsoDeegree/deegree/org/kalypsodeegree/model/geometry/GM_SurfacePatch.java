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

package org.deegree.model.geometry;

import org.opengis.cs.CS_CoordinateSystem;

/**
 * 
 * Defining the iso geometry <code>GM_SurfacePatch</code> which is used for
 * building surfaces. A surface patch is made of one exterior ring and 0..n
 * interior rings. By definition there can't be a surface patch with no exterior
 * ring. A polygon is a specialized surface patch.
 * 
 * -----------------------------------------------------
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */

public interface GM_SurfacePatch extends GM_GenericSurface
{

  /**
   * The interpolation determines the surface interpolation mechanism used for
   * this GM_SurfacePatch. This mechanism uses the control points and control
   * parameters defined in the various subclasses to determine the position of
   * this GM_ SurfacePatch.
   */
  GM_SurfaceInterpolation getInterpolation();

  /**
   * returns the exterior ring of the surface
   */
  GM_Position[] getExteriorRing();

  /**
   * returns the interior rings of the surface
   */
  GM_Position[][] getInteriorRings();

  /**
   * returns the coordinate system of the surface patch
   */
  CS_CoordinateSystem getCoordinateSystem();

  /**
   * The Boolean valued operation "intersects" shall return TRUE if this
   * GM_Object intersects another GM_Object. Within a GM_Complex, the
   * GM_Primitives do not intersect one another. In general, topologically
   * structured data uses shared geometric objects to capture intersection
   * information.
   */
  boolean intersects( GM_Object gmo );

  /**
   * The Boolean valued operation "contains" shall return TRUE if this GM_Object
   * contains another GM_Object.
   */
  boolean contains( GM_Object gmo );

  /**
   * The operation "centroid" shall return the mathematical centroid for this
   * GM_Object. The result is not guaranteed to be on the object.
   */
  public GM_Point getCentroid();

  /**
   * The operation "area" shall return the area of this GM_GenericSurface. The
   * area of a 2 dimensional geometric object shall be a numeric measure of its
   * surface area Since area is an accumulation (integral) of the product of two
   * distances, its return value shall be in a unit of measure appropriate for
   * measuring distances squared.
   */
  public double getArea();

  /**
   * @link aggregationByValue
   * @clientCardinality 1..*
   */
  /* #GM_GenericCurve lnkGM_GenericCurve; */
}