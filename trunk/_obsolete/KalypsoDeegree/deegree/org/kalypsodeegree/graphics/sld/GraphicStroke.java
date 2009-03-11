/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 * 
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always. 
 * 
 * If you intend to use this software in other ways than in kalypso 
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree, 
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */
package org.kalypsodeegree.graphics.sld;

/**
 * The GraphicStroke element both indicates that a repeated-linear-graphic stroke type will be used.
 * <p>
 * </p>
 * The Graphic sub-element specifies the linear graphic. Proper stroking with a linear graphic requires two hot-spot
 * points within the space of the graphic to indicate where the rendering line starts and stops. In the case of raster
 * images with no special mark-up, this line will be assumed to be middle pixel row of the image, starting from the
 * first pixel column and ending at the last pixel column.
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public interface GraphicStroke
{

  /**
   * A Graphic is a graphic symbol with an inherent shape, color(s), and possibly size. A graphic can be very informally
   * defined as a little picture and can be of either a raster or vector-graphic source type. The term graphic is used
   * since the term symbol is similar to symbolizer which is used in a different context in SLD.
   * 
   * @return graphic
   */
  Graphic getGraphic();

  /**
   * Sets the Graphic.
   * 
   * @param graphic
   */
  void setGraphic( Graphic graphic );
}