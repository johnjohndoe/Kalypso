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
package org.deegree_impl.services.wms.protocol;

import java.awt.Color;
import java.awt.Font;
import java.util.HashMap;

import org.deegree.services.WebServiceException;
import org.deegree.services.wms.protocol.WMSGetScaleBarRequest;
import org.deegree_impl.services.OGCWebServiceRequest_Impl;

/**
 * class for the deegree specific WMS request: GetScaleBar
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @author <a href="mailto:wanhoff@uni-bonn.de">Jeronimo Wanhoff </a>
 */
public class WMSGetScaleBarRequest_Impl extends OGCWebServiceRequest_Impl implements
    WMSGetScaleBarRequest
{
  private Color allgColor;

  private Color barColor;

  private Color labelColor;

  private Font font;

  private String barStyle; // = "default";

  private String units;

  private String format;

  private double scale;

  private int scaleDenominator;

  private int barSize;

  private int bottomLabel; // = L_NONE;

  private int topLabel; // = L_NONE;

  /**
   * Constructor
   * 
   * @param version
   * @param id
   * @param vendorSpecificParameter
   * @param topLabel
   * @param bottomLabel
   * @param scale
   * @param scaleDenominator
   * @param units
   * @param labelColor
   * @param barColor
   * @param bgColor
   * @param barStyle
   * @param font
   */
  public WMSGetScaleBarRequest_Impl( String version, String id, HashMap vendorSpecificParameter,
      String units, int topLabel, int bottomLabel, Color labelColor, Font font, String barStyle,
      Color barColor, Color bgColor, int size, double scale, int scaleDenominator, String format )
  {
    super( "GetScaleBarRequest", "WMS", id, version, vendorSpecificParameter );
    setTopLabel( topLabel );
    setBottomLabel( bottomLabel );
    setScale( scale );
    setScaleDenominator( scaleDenominator );
    setUnits( units );
    setLabelColor( labelColor );
    setColor( barColor );
    setBGColor( bgColor );
    setStyle( barStyle );
    setFont( font );
    setFormat( format );
    setBarSize( size );
  }

  /**
   * set the label type that shall be displayed above the scale bar. Possible
   * values are WMSGetScaleBarRequest.L_SCALE, WMSGetScaleBarRequest.L_SIZE
   * WMSGetScaleBarRequest.and L_NONE.
   * 
   * @param topLabel
   *          type of label to display above the scale bar
   */
  public void setTopLabel( int topLabel )
  {
    this.topLabel = topLabel;
  }

  /**
   * get the label type that shall be displayed above the scale bar. Possible
   * values are WMSGetScaleBarRequest.L_SCALE, WMSGetScaleBarRequest.L_SIZE and
   * WMSGetScaleBarRequest.L_NONE.
   * 
   * @return type of label to display above the scale bar
   */
  public int getTopLabel()
  {
    return topLabel;
  }

  /**
   * set the label type that shall be displayed below the scale bar. Possible
   * values are WMSGetScaleBarRequest.L_SCALE, WMSGetScaleBarRequest.L_SIZE and
   * WMSGetScaleBarRequest.L_NONE
   * 
   * @param bLabel
   *          type of label to display below the scale bar
   */
  public void setBottomLabel( int bLabel )
  {
    this.bottomLabel = bLabel;
  }

  /**
   * get the label type that shall be displayed below the scale bar. Possible
   * values are WMSGetScaleBarRequest.L_SCALE, WMSGetScaleBarRequest.L_SIZE and
   * WMSGetScaleBarRequest.L_NONE
   * 
   * @return type of label to display below the scale bar
   */
  public int getBottomLabel()
  {
    return bottomLabel;
  }

  /**
   * 
   * 
   * @param s
   */
  public void setScale( double s )
  {
    scale = s;
  }

  /**
   * 
   * 
   * @return
   */
  public double getScale()
  {
    return scale;
  }

  /**
   * 
   * 
   * @param sd
   */
  public void setScaleDenominator( int sd )
  {
    scaleDenominator = sd;
  }

  /**
   * 
   * 
   * @return
   */
  public int getScaleDenominator()
  {
    return scaleDenominator;
  }

  /**
   * sets the name of the units the scale bar should use for measuring
   * 
   * @param uName
   *          name of the units
   */
  public void setUnits( String uName )
  {
    units = uName;
  }

  /**
   * returns the name of the units the scale bar should use for measuring
   * 
   * @return name of the units
   */
  public String getUnits()
  {
    return units;
  }

  /**
   * sets the size of the scale bar in pixel
   * 
   * @param size
   *          of the scale bar in pixel
   */
  public void setSize( int size )
  {
    barSize = size;
  }

  /**
   * returns the size of the scale bar in pixel
   * 
   * @return size of the scale bar in pixel
   */
  public int getSize()
  {
    return barSize;
  }

  /**
   * sets the color will be painted or <tt>null</tt> if the scale bar hasn't a
   * unique color (depends on the style)
   * 
   * @param bColor
   *          color of the scale bar
   */
  public void setColor( Color bColor )
  {
    barColor = bColor;
  }

  /**
   * returns the color will be painted or <tt>null</tt> if the scale bar
   * hasn't a unique color (depends on the style)
   * 
   * @return color of the scale bar
   */
  public Color getColor()
  {
    return barColor;
  }

  /**
   * sets the background color of the scale bar
   * 
   * @param bgColor
   *          background color
   */
  public void setBGColor( Color bgColor )
  {
    allgColor = bgColor;
  }

  /**
   * returns the background color of the scale bar
   * 
   * @return background color
   */
  public Color getBGColor()
  {
    return allgColor;
  }

  /**
   * sets the color the scale bars label will be painted
   * 
   * @param lColor
   *          color of the labels
   */
  public void setLabelColor( Color lColor )
  {
    labelColor = lColor;
  }

  /**
   * returns the color the scale bars label will be painted
   * 
   * @return color of the labels
   */
  public Color getLabelColor()
  {
    return labelColor;
  }

  /**
   * returns the name of the style the scale bar should be drawed. beside
   * 'default' and 'ALTERNATE' the available styles depends on the
   * implementation.
   * 
   * @return style of the scale bar
   */
  public String getStyle()
  {
    return barStyle;
  }

  /**
   * sets the name of the style the scale bar should be drawed. beside 'default'
   * and 'ALTERNATE' the available styles depends on the implementation.
   * 
   * @param bStyle
   *          style of the scale bar
   */
  public void setStyle( String bStyle )
  {
    barStyle = bStyle;
  }

  /**
   * returns the image format (MIME Type) the scale bar will be produced in.
   * 
   * @return image format of the scale bar
   */
  public String getFormat()
  {
    return format;
  }

  /**
   * sets the image format (MIME Type) the scale bar will be produced in.
   * 
   * @param format
   */
  public void setFormat( String format )
  {
    this.format = format;
  }

  /**
   * returns the pixel size in milli meter. This value is needed for calculating
   * scale of the map. For usual the exact pixel size isn't known or different
   * pixels size at different clients are used at the same time. So in most
   * cases this method returns the default pixels size of 0.28 mm.
   * 
   * @return client pixel size in milli meter
   */
  public double getPixelSize()
  {
    return 0.28;
    //TODO was ordentliches dafür schreiben und eventuell
    // getWMSScaleBarRequest() in der Klasse WMSProtokolFactory anpassen
  }

  /**
   * 
   * 
   * @return @throws
   *         WebServiceException
   */
  public String getRequestParameter() throws WebServiceException
  {
    return null;
  }

  /**
   * return the size of the scale bar in pixel
   * 
   * @return
   */
  public int getBarSize()
  {
    return barSize;
  }

  /**
   * sets the size of the scale bar in pixel
   * 
   * @param barSize
   */
  public void setBarSize( int barSize )
  {
    this.barSize = barSize;
  }

  /**
   * returns the font to be used for drawing the labels
   * 
   * @return
   */
  public Font getFont()
  {
    return font;
  }

  /**
   * sets the font to be used for drawing the labels
   */
  public void setFont( Font font )
  {
    this.font = font;
  }
}