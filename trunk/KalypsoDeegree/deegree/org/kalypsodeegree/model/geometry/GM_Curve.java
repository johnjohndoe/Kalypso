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

/**
 * 
 * The interface defines the access to curve geometries. Curves are made of one
 * or more curve segments.
 * 
 * <p>
 * -----------------------------------------------------
 * </p>
 * 
 * @author Andreas Poth
 * @version $Revision$ $Date$
 *          <p>
 */
public interface GM_Curve extends GM_OrientableCurve, GM_GenericCurve
{
  /**
   * @link aggregationByValue
   * @clientCardinality 1..*
   */
  /* #GM_CurveSegment lnkGM_CurveSegment; */

  /**
   * returns the number of segments building the curve
   */
  int getNumberOfCurveSegments();

  /**
   * returns the curve segment at the submitted index
   */
  GM_CurveSegment getCurveSegmentAt( int index ) throws GM_Exception;

  /**
   * writes a segment to the curve at submitted position. the old point will be
   * deleted
   */
  void setCurveSegmentAt( GM_CurveSegment segment, int index ) throws GM_Exception;

  /**
   * inserts a segment in the curve at the submitted position. all points with a
   * position that equals index or is higher will be shifted
   */
  void insertCurveSegmentAt( GM_CurveSegment segment, int index ) throws GM_Exception;

  /**
   * adds a segment at the end of the curve
   */
  void addCurveSegment( GM_CurveSegment segment ) throws GM_Exception;

  /**
   * deletes the segment at the submitted index
   */
  void deleteCurveSegmentAt( int index ) throws GM_Exception;

}