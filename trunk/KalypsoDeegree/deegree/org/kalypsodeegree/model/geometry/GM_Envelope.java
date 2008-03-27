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

/**
 * a boundingbox as child of a GM_Polygon isn't part of the iso19107 spec but it simplifies the geometry handling within
 * jago
 */
public interface GM_Envelope extends Cloneable
{
  /**
   * @link aggregationByValue
   * @clientCardinality 2
   */
  /* #GM_Position lnkGM_position; */

  /**
   * returns the width of bounding box
   */
  double getWidth( );

  /**
   * returns the height of bounding box
   */
  double getHeight( );

  /**
   * returns the minimum coordinates of bounding box
   */
  GM_Position getMin( );

  /**
   * returns the maximum coordinates of bounding box
   */
  GM_Position getMax( );

  /**
   * returns true if the bounding box contains the submitted position
   */
  boolean contains( double x, double y );

  /**
   * returns true if the bounding box contains the submitted position
   */
  boolean contains( GM_Position position );

  /**
   * returns true if this envelope intersects the submitted envelope
   */
  boolean intersects( GM_Envelope bb );

  /**
   * returns true if all positions of the submitted bounding box are within this bounding box
   */
  boolean contains( GM_Envelope bb );

  /**
   * returns a new GM_Envelope object representing the intersection of this GM_Envelope with the specified GM_Envelope.
   */
  GM_Envelope createIntersection( GM_Envelope bb );

  /**
   * merges two GM_Envelops and returns the minimum envelope containing both.
   * 
   * @return merged envelope
   */
  GM_Envelope getMerged( GM_Envelope envelope );

  /**
   * creates a new envelope
   */
  GM_Envelope getBuffer( double b );

  GM_Envelope getPaned( GM_Point centroid );

  GM_Envelope getMerged( GM_Position position );

  public Object clone( );

  /**
   * This function returns the coordinate system, the coordinates of the contained positions are in.
   * 
   * @return The coordinate system.
   */
  public String getCoordinateSystem( );

  /**
   * This function sets the coordinate system, the coordinates of the contained positions are in.
   * 
   * @param coordinateSystem
   *            The coordinate system.
   */
  public void setCoordinateSystem( String coordinateSystem );
}