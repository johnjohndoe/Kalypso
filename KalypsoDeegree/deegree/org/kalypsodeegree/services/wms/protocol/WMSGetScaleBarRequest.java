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
package org.deegree.services.wms.protocol;

import java.awt.Color;
import java.awt.Font;

import org.deegree.services.OGCWebServiceRequest;

/**
 * interface definition for the deegree specific WMS request: GetScaleBar
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 */
public interface WMSGetScaleBarRequest extends OGCWebServiceRequest
{

  public static int L_SCALE = 0;

  public static int L_SIZE = 1;

  public static int L_NONE = -1;

  /**
   * returns the name of the units the scale bar should use for measuring
   * 
   * @return name of the units
   */
  String getUnits();

  /**
   * returns the size of the scale bar in pixel
   * 
   * @return size of the scale bar in pixel
   */
  int getSize();

  /**
   * get the label type that shall be displayed above the scale bar. Possible
   * values are L_SCALE, L_SIZE and L_NONE.
   * 
   * @return type of label to display above the scale bar
   */
  int getTopLabel();

  /**
   * get the label type that shall be displayed below the scale bar. Possible
   * values are L_SCALE, L_SIZE and L_NONE
   * 
   * @return type of label to display below the scale bar
   */
  int getBottomLabel();

  double getScaleDenominator();

  /**
   * returns the name of the style the scale bar should be drawed. beside
   * 'default' and 'ALTERNATE' the available styles depends on the
   * implementation.
   * 
   * @return style of the scale bar
   */
  String getStyle();

  /**
   * returns the color will be painted or <tt>null</tt> if the scale bar
   * hasn't a unique color (depends on the style)
   * 
   * @return color of the scale bar
   */
  Color getColor();

  /**
   * returns the background color of the scale bar
   * 
   * @return background color
   */
  Color getBGColor();

  /**
   * returns the color the scale bars label will be painted
   * 
   * @return color of the labels
   */
  Color getLabelColor();

  /**
   * returns the image format (MIME Type) the scale bar will be produced in.
   * 
   * @return image format of the scale bar
   */
  String getFormat();

  /**
   * returns the pixel size in milli meter. This value is needed for calculating
   * scale of the map. For usual the exact pixel size isn't known or different
   * pixels size at different clients are used at the same time. So in most
   * cases this method returns the default pixels size of 0.28 mm.
   * 
   * @return client pixel size in milli meter
   */
  double getPixelSize();

  /**
   * return the size of the scale bar in pixel
   * 
   * @return
   */
  int getBarSize();

  /**
   * return the the font to be used for drawing the labels
   * 
   * @return
   */
  Font getFont();
}