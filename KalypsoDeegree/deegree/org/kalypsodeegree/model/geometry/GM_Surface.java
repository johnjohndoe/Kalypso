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