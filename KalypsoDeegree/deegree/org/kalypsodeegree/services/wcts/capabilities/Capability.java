/*----------------    FILE HEADER  ------------------------------------------

 This file is part of Deegree.
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

 Markus Mueller
 Department of Geography
 University of Bonn
 Meckenheimer Allee 166
 53115 Bonn
 Germany
 E-Mail: mm@giub.uni-bonn.de

 ---------------------------------------------------------------------------*/
package org.deegree.services.wcts.capabilities;

/**
 * This section of the Capabilities-document describes the operations made
 * available by the CTS. Apart from the operations defined as mandatory
 * (GetCapabilities, IsTransformable, Transform), the optional operation
 * DescribeTransformation can be described within the element &lt;Request&gt;.
 * 
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version 2002-07-10
 */

public interface Capability // extends
{
  /**
   * <p>
   * In many cases, it is possible to perform a transformation of coordinates
   * from one reference system into another in different (mathematical) kinds.
   * Under normal conditions the CTS selects a suitable procedure on the basis
   * the source and the target reference system and accomplishes the
   * transformation. For different reasons it can make sense for a user to
   * specify the transformation steps themselves.
   * 
   * <p>
   * default="false"
   */
  public boolean getUserDefinedCoordinateSystems();

  /**
   * <p>
   * In many cases, it is possible to perform a transformation of coordinates
   * from one reference system into another in different (mathematical) kinds.
   * Under normal conditions the CTS selects a suitable procedure on the basis
   * the source and the target reference system and accomplishes the
   * transformation. For different reasons it can make sense for a user to
   * specify the transformation steps themselves.
   * 
   * <p>
   * default="false"
   */
  public boolean getUserDefinedTransformations();

  /**
   * A Coordinate transformation service (CTS) acts as Web service, which knows
   * the four different Requests
   */
  public WCTS_Request getRequest();

  /**
   * <p>
   * Each CTS must know at least one transformation-type. These concern
   * fundamental methods for the transformation of coordinates out of a
   * reference system into another. It <b>doesn't </b> concern concrete
   * transformations between two determined references system.
   * <p>
   * Examples of kinds of transformation are: Longitude rotation, Abridged
   * Molodenski, Geocentric_to_Ellipsoid etc...
   */
  public KnownTransformationType[] getKnownTransformationTypes();

  /**
   * Each CTS must know at least <b>two </b> spatial reference-System, so that
   * at least one transformation is possible.
   */
  public KnownCoordinateReferenceSystem[] getKnownCoordinateReferenceSystems();

  /**
   * With the &lt;VendorSpecificCapabilities&gt; element, it is possible to
   * define additional Capabilities.
   */
  public String[] getVendorSpecificCapabilities();

}