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
package org.deegree.graphics.sld;

import java.util.HashMap;

/**
 * This is the top level interface of <tt>Fill</tt> and <tt>Stroke</tt>
 * defining the methods <tt>getGraphicFill()</tt> and
 * <tt>getCssParameters()</tt> that are common to both.
 * <p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public interface Drawing
{

  /**
   * The GraphicFill element both indicates that a stipple-fill repeated graphic
   * will be used and specifies the fill graphic.
   * 
   * @return the GraphicFill-Element
   */
  GraphicFill getGraphicFill();

  /**
   * The GraphicFill element both indicates that a stipple-fill repeated graphic
   * will be used and specifies the fill graphic.
   * 
   * @param graphicFill
   *          the GraphicFill-Element
   */
  void setGraphicFill( GraphicFill graphicFill );

  /**
   * A simple SVG/CSS2 styling parameters are given with the CssParameter
   * element.
   * <p>
   * </p>
   * This method is for technical use. The user should access the specialized
   * methods of the derived classes.
   * 
   * @return the CssParameters
   */
  HashMap getCssParameters();

  /**
   * A simple SVG/CSS2 styling parameters are given with the CssParameter
   * element.
   * <p>
   * </p>
   * This method sets CssParameters.
   * 
   * @param cssParameters
   *          the CssParameters
   */
  void setCssParameters( HashMap cssParameters );

  /**
   * A simple SVG/CSS2 styling parameters are given with the CssParameter
   * element.
   * <p>
   * </p>
   * This method adds a CssParameter to a given set of CssParameters.
   * <p>
   * 
   * @param key
   *          the key of the object to insert
   * @param value
   *          the value of the object to insert
   */
  void addCssParameter( Object key, Object value );

  /**
   * A simple SVG/CSS2 styling parameters are given with the CssParameter
   * element.
   * <p>
   * </p>
   * This method removes a CssParameter from a given set of CssParameters.
   * 
   * @param key
   *          the key of the object to remove
   */
  void removeCssParameter( Object value );
}