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
package org.deegree.model.coverage;

import java.net.URL;

import org.deegree.model.geometry.GM_Envelope;

/**
 * <p>
 * ---------------------------------------------------------------------
 * </p>
 * 
 * @author Andreas Poth
 * @version 31.10.2002
 */
public interface Tile
{

  /**
   * returns the URL of the resource assigned to a <tt>Tile</tt> that
   */
  URL getResourceURL();

  /**
   * returns the bounding box of the <tt>Tile</tt>
   */
  GM_Envelope getBoundingBox();

  /**
   * returns the embeded <tt>Level</tt> if one exists. otherwise <tt>null</tt>
   * will be returned.
   */
  Level getLevel();

  /**
   * if the tiles are ordered at a quad tree or something like this an instance
   * of <tt>Tile</tt> will contain one or more <tt>Tile</tt> instances with
   * a smaller bounding box.
   */
  Tile[] getTiles();

}