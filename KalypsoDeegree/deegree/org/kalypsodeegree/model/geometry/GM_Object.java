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
 * The basic interface for all geometries. it declares the methods that are
 * common to all geometries. this doesn't means for example that all geometries
 * defines a valid boundary but is there asked for they should be able to answer
 * (with null).
 * 
 * <p>
 * -----------------------------------------------------
 * </p>
 * 
 * @author Andreas Poth
 * @version $Revision$ $Date$
 *          <p>
 */
public interface GM_Object
{
  /* #CS_CoordinateSystem lnkCS_CoordinateSystem; */

  /**
   * returns the bounding box of a geometry
   */
  GM_Envelope getEnvelope();

  /**
   * returns the boundary of a geometry
   */
  GM_Boundary getBoundary();

  /**
   * The operation "dimension" shall return the inherent dimension of this
   * GM_Object, which shall be less than or equal to the coordinate dimension.
   * The dimension of a collection of geometric objects shall be the largest
   * dimension of any of its pieces. Points are 0-dimensional, curves are
   * 1-dimensional, surfaces are 2-dimensional, and solids are 3-dimensional.
   */
  int getDimension();

  /**
   * The operation "coordinateDimension" shall return the dimension of the
   * coordinates that define this GM_Object, which must be the same as the
   * coordinate dimension of the coordinate reference system for this GM_Object.
   */
  int getCoordinateDimension();

  /**
   * returns the spatial reference system of a geometry
   */
  CS_CoordinateSystem getCoordinateSystem();

  /**
   * returns true if no geometry values resp. points stored within the geometry.
   */
  boolean isEmpty();

  /**
   * The operation "distance" shall return the distance between this GM_Object
   * and another GM_Object. This distance is defined to be the greatest lower
   * bound of the set of distances between all pairs of points that include one
   * each from each of the two GM_Objects. A "distance" value shall be a
   * positive number associated to distance units such as meters or standard
   * foot. If necessary, the second geometric object shall be transformed into
   * the same coordinate reference system as the first before the distance is
   * calculated.
   * <p>
   * </p>
   * If the geometric objects overlap, or touch, then their distance apart shall
   * be zero. Some current implementations use a "negative" distance for such
   * cases, but the approach is neither consistent between implementations, nor
   * theoretically viable.
   */
  double distance( GM_Object gmo );

  /**
   * translate a geometry by the submitted values. if the length of <tt>d</tt>
   * is smaller then the dimension of the geometry only the first d.length'th
   * coordinates will be translated. If the length of <tt>d</tt> is larger
   * then the dimension of the geometry an ArrayIndexOutOfBoundExceptions
   * raises.
   */
  void translate( double[] d );

  /**
   * The operation "centroid" shall return the mathematical centroid for this
   * GM_Object. The result is not guaranteed to be on the object. For
   * heterogeneous collections of primitives, the centroid only takes into
   * account those of the largest dimension. For example, when calculating the
   * centroid of surfaces, an average is taken weighted by area. Since curves
   * have no area they do not contribute to the average.
   */
  GM_Point getCentroid();

  /**
   * The operation "convexHull" shall return a GM_Object that represents the
   * convex hull of this GM_Object.
   */
  GM_Object getConvexHull();

  /**
   * The operation "buffer" shall return a GM_Object containing all points whose
   * distance from this GM_Object is less than or equal to the "distance" passed
   * as a parameter. The GM_Object returned is in the same reference system as
   * this original GM_Object. The dimension of the returned GM_Object is
   * normally the same as the coordinate dimension - a collection of GM_Surfaces
   * in 2D space and a collection of GM_Solids in 3D space, but this may be
   * application defined.
   */
  GM_Object getBuffer( double distance );

  /**
   * The Boolean valued operation "contains" shall return TRUE if this GM_Object
   * contains another GM_Object.
   */
  boolean contains( GM_Object gmo );

  /**
   * The Boolean valued operation "contains" shall return TRUE if this GM_Object
   * contains a single point given by a coordinate.
   */
  boolean contains( GM_Position position );

  /**
   * The Boolean valued operation "intersects" shall return TRUE if this
   * GM_Object intersects another GM_Object. Within a GM_Complex, the
   * GM_Primitives do not intersect one another. In general, topologically
   * structured data uses shared geometric objects to capture intersection
   * information.
   */
  boolean intersects( GM_Object gmo );

  /**
   * The "union" operation shall return the set theoretic union of this
   * GM_Object and the passed GM_Object.
   */
  GM_Object union( GM_Object gmo );

  /**
   * The "intersection" operation shall return the set theoretic intersection of
   * this GM_Object and the passed GM_Object.
   */
  GM_Object intersection( GM_Object gmo );

  /**
   * The "difference" operation shall return the set theoretic difference of
   * this GM_Object and the passed GM_Object.
   */
  GM_Object difference( GM_Object gmo );

  /*
   * provide optimized proximity queries within for a distance . calvin added on
   * 10/21/2003
   */
  boolean isWithinDistance( GM_Object gmo, double distance );

}