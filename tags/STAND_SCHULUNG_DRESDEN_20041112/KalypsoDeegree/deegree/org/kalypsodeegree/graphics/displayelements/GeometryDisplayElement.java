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

package org.deegree.graphics.displayelements;

import org.deegree.graphics.sld.Symbolizer;
import org.deegree.model.geometry.GM_Object;

/**
 * Basic interface of all display elements that are related to a geometry.
 * Usually this will be the case.
 * <p>
 * ------------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public interface GeometryDisplayElement extends DisplayElement
{

  /**
   * Overwrites the default placement of the <tt>DisplayElement</tt>. This
   * method is used by the <tt>PlacementOptimizer</tt> to minimize the
   * overlapping of labels, for example.
   * <p>
   * 
   * @param o
   *          the placement to be used
   */
  void setPlacement( Object o );

  /**
   * sets the geometry that determines the position the DisplayElement will be
   * rendered to
   * 
   * @geometry geometry the <tt>DisplayElement</tt> is based on
   */
  void setGeometry( GM_Object geometry );

  /**
   * returns the geometry that determines the position the DisplayElement will
   * be rendered to
   */
  GM_Object getGeometry();

  /**
   * sets the rule that determines how the geometry will be rendered
   * 
   * @param rule
   *          symbolizer defining rendering style
   */
  void setSymbolizer( Symbolizer rule );

  /**
   * Returns the symbolizer that determines how the geometry will be rendered.
   */
  Symbolizer getSymbolizer();

  /**
   * sets the rule that determines how the geometry will be rendered when it's
   * highlighted
   * 
   * @param rule
   *          symbolizer defining rendering style
   */
  void setHighlightSymbolizer( Symbolizer rule );

  /**
   * returns the symbolizer that determines how the geometry will be rendered if
   * it's highlighted
   */
  Symbolizer getHighlightSymbolizer();

  /**
   * sets the rule that determines how the geometry will be rendered when it's
   * selected
   * 
   * @param rule
   *          symbolizer defining rendering style
   */
  void setSelectedSymbolizer( Symbolizer rule );

  /**
   * returns the symbolizer that determines how the geometry will be rendered if
   * it's selected
   */
  Symbolizer getSelectedSymbolizer();
}