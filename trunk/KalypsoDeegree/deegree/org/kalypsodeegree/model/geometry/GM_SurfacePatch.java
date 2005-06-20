/*--------------- Kalypso-Deegree-Header ------------------------------------------------------------

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
 
 
 history:
 
 Files in this package are originally taken from deegree and modified here
 to fit in kalypso. As goals of kalypso differ from that one in deegree
 interface-compatibility to deegree is wanted but not retained always. 
 
 If you intend to use this software in other ways than in kalypso 
 (e.g. OGC-web services), you should consider the latest version of deegree,
 see http://www.deegree.org .

 all modifications are licensed as deegree, 
 original copyright:
 
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon GmbH
 http://www.lat-lon.de
 
 ---------------------------------------------------------------------------------------------------*/

package org.kalypsodeegree.model.geometry;

import org.opengis.cs.CS_CoordinateSystem;

/**
 * 
 * Defining the iso geometry <code>GM_SurfacePatch</code> which is used for building surfaces. A surface patch is made
 * of one exterior ring and 0..n interior rings. By definition there can't be a surface patch with no exterior ring. A
 * polygon is a specialized surface patch.
 * 
 * -----------------------------------------------------
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */

public interface GM_SurfacePatch extends GM_GenericSurface
{

  /**
   * The interpolation determines the surface interpolation mechanism used for this GM_SurfacePatch. This mechanism uses
   * the control points and control parameters defined in the various subclasses to determine the position of this GM_
   * SurfacePatch.
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
   * The Boolean valued operation "intersects" shall return TRUE if this GM_Object intersects another GM_Object. Within
   * a GM_Complex, the GM_Primitives do not intersect one another. In general, topologically structured data uses shared
   * geometric objects to capture intersection information.
   */
  boolean intersects( GM_Object gmo );

  /**
   * The Boolean valued operation "contains" shall return TRUE if this GM_Object contains another GM_Object.
   */
  boolean contains( GM_Object gmo );

  /**
   * The operation "centroid" shall return the mathematical centroid for this GM_Object. The result is not guaranteed to
   * be on the object.
   */
  public GM_Point getCentroid();

  /**
   * The operation "area" shall return the area of this GM_GenericSurface. The area of a 2 dimensional geometric object
   * shall be a numeric measure of its surface area Since area is an accumulation (integral) of the product of two
   * distances, its return value shall be in a unit of measure appropriate for measuring distances squared.
   */
  public double getArea();

  /**
   * @link aggregationByValue
   * @clientCardinality 1..*
   */
  /* #GM_GenericCurve lnkGM_GenericCurve; */
}