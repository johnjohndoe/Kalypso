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
package org.kalypsodeegree_impl.graphics.sld;

import org.kalypsodeegree.graphics.sld.Graphic;
import org.kalypsodeegree.graphics.sld.LegendGraphic;
import org.kalypsodeegree.xml.Marshallable;
import org.kalypsodeegree_impl.tools.Debug;

/**
 * The LegendGraphic element gives an optional explicit Graphic symbol to be displayed in a legend for this rule.
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:k.lupp@web.de">Katharina Lupp </a>
 * @version $Revision$ $Date$
 */
public class LegendGraphic_Impl implements LegendGraphic, Marshallable
{
  private Graphic graphic = null;

  /**
   * constructor initializing the class with the <LegendGraphic>
   */
  LegendGraphic_Impl( Graphic graphic )
  {
    setGraphic( graphic );
  }

  /**
   * A Graphic is a graphic symbol with an inherent shape, color(s), and possibly size. A graphic can be very informally
   * defined as a little picture and can be of either a raster or vector-graphic source type. The term graphic is used
   * since the term symbol is similar to symbolizer which is used in a different context in SLD.
   * 
   * @return graphic
   */
  public Graphic getGraphic()
  {
    return graphic;
  }

  /**
   * sets the <Graphic>
   * 
   * @param graphic
   */
  public void setGraphic( Graphic graphic )
  {
    this.graphic = graphic;
  }

  /**
   * exports the content of the Font as XML formated String
   * 
   * @return xml representation of the Font
   */
  public String exportAsXML()
  {
    Debug.debugMethodBegin();

    StringBuffer sb = new StringBuffer( 1000 );
    sb.append( "<LegendGraphic>" );
    sb.append( ( (Marshallable)graphic ).exportAsXML() );
    sb.append( "</LegendGraphic>" );

    Debug.debugMethodEnd();
    return sb.toString();
  }
}