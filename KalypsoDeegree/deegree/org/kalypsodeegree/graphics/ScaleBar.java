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
package org.kalypsodeegree.graphics;

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;

/**
 * 
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 */
public interface ScaleBar
{

  public static int L_NONE = -1;

  public static int L_SCALE = 0;

  public static int L_SCALEDENOMINATOR = 1;

  /**
   * will paint the scale bar to the passed graphic context
   * 
   * @param g
   *          graphic context
   */
  void paint( Graphics g );

  /**
   * sets the type of the label above the scale bar
   * 
   * @param labelType
   *          lable type
   */
  void setTopLabel( int labelType );

  /**
   * sets the type of the label below the scale bar
   * 
   * @param labelType
   *          lable type
   */
  void setBottomLabel( int labelType );

  /**
   * sets the scale as defined in the OGC WMS 1.1.1 specification. Scale is defined as the diagonal size of a pixel in
   * the center of a map measured in meter. The setting of the scale will affect the value of the scale denominator
   * 
   * @parameter scale map scale
   */
  void setScale( double scale );

  /**
   * sets the scale denominator for the scale bar. The scale denominator is the scale expression as we know it for
   * printed maps (e.g. 1:10000 1:5000). The passed value is expressed in meters. The setting of the scale denominator
   * will affect the value of the scale
   * 
   * @param scaleDen
   *          scale denominator value
   */
  void setScaleDenominator( int scaleDen );

  /**
   * sets the units the scale and the scale denominater will be expressed at. Settings other than meter will cause that
   * the passed values for scale and scale denominater will be recalculated for painting. it depends on the
   * implementation what units are supported.
   * 
   * @param units
   *          name units (meter, miles, feet etc.)
   */
  void setUnits( String units );

  /**
   * sets the front color of the scale bar
   */
  void setBarColor( Color color );

  /**
   * sets the label color of the scale bar
   */
  void setLabelColor( Color color );

  /**
   * sets the background color of the scale bar
   */
  void setBackgroundColor( Color color );

  /**
   * sets the style of the scale bar. default style is |--------| the list of known styles depends on the implementation
   * 
   * @param style
   *          style name
   */
  void setStyle( String style );

  /**
   * sets the font for label rendering
   * 
   * @param font
   *          awt font object
   */
  void setFont( Font font );

}