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
 * Defining the surface geometry of the iso geometry model. a surface is made of
 * 1..n surface patches. for convention it is defined that GM_Surface is a
 * closed geometry. that means each surface patch a surface is made of must
 * touch at least one other surface patch if a surface is made of more then one
 * surface patch
 * 
 * <p>
 * -----------------------------------------------------
 * </p>
 * 
 * @author Andreas Poth
 * @version $Revision$ $Date$
 *          <p>
 */

public interface GM_Surface extends GM_OrientableSurface, GM_GenericSurface
{
  /**
   * @link aggregationByValue
   * @clientCardinality 1..*
   */
  /* #GM_SurfacePatch lnkGM_SurfacePatch; */

  /**
   * returns the number of patches building the surface
   */
  int getNumberOfSurfacePatches();

  /**
   * returns the surface patch at the submitted index
   */
  GM_SurfacePatch getSurfacePatchAt( int index ) throws GM_Exception;

  /**
   * writes a surface patch to the surface at submitted position. the old patch
   * will be deleted
   */
  void setSurfacePatchAt( GM_SurfacePatch segment, int index ) throws GM_Exception;

  /**
   * inserts a surface patch in the curve at the submitted position. all points
   * with a position that equals index or is higher will be shifted
   */
  void insertSurfacePatchAt( GM_SurfacePatch segment, int index ) throws GM_Exception;

  /**
   * adds a surface patch at the end of the curve
   */
  void addSurfacePatch( GM_SurfacePatch segment );

  /**
   * deletes the surface patch at the submitted index
   */
  void deleteSurfacePatchAt( int index ) throws GM_Exception;

}