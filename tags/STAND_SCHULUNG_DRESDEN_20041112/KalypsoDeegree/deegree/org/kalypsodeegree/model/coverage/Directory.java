/*----------------    FILE HEADER  ------------------------------------------
 
 This file is part of deegree (Java Framework for Geospatial Solutions).
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
package org.deegree.model.coverage;

import org.deegree.model.geometry.GM_Envelope;

/**
 * 
 * 
 * <p>
 * -----------------------------------------------------------------------
 * </p>
 * 
 * @author Andreas Poth
 * @version $Revision$ $Date$
 *          <p>
 */
public interface Directory
{

  /**
   * returns the file extension known by the directory
   */
  String[] getExtensions();

  /**
   * returns the resource name of the directory
   */
  String getResource();

  /**
   * returns the bounding box of the <tt>Directory</tt>
   */
  GM_Envelope getBoundingBox();

  /**
   * returns the width of the tiles contained within the diirectory in
   * measurement of its CRS
   */
  double getWidthCRS();

  /**
   * returns the height of the tiles contained within the diirectory in
   * measurement of its CRS
   */
  double getHeightCRS();

  /**
   * returns the embeded <tt>Level</tt> if one exists. otherwise <tt>null</tt>
   * will be returned.
   */
  Level getLevel();

  /**
   * if the tiles are ordered at a quad tree or something like this an instance
   * of <tt>Directory</tt> will contain one or more <tt>Directory</tt>
   * instances with a smaller bounding box.
   */
  Directory[] getDirectories();

  /**
   * returns all <tt>Tiles</tt> containted within the directory that fits the
   * submitted bounding box
   */
  Tile[] getTiles( GM_Envelope bbox ) throws CoverageCreationException;

  /**
   * returns all <tt>Tiles</tt> containted within the directory that fits the
   * submitted bounding box
   */
  Tile[] getTiles() throws CoverageCreationException;

}