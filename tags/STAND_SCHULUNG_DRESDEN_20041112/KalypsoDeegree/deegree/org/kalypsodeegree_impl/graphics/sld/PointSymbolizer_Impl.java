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
package org.deegree_impl.graphics.sld;

import org.deegree.graphics.sld.Fill;
import org.deegree.graphics.sld.Geometry;
import org.deegree.graphics.sld.Graphic;
import org.deegree.graphics.sld.Mark;
import org.deegree.graphics.sld.PointSymbolizer;
import org.deegree.graphics.sld.Stroke;
import org.deegree.xml.Marshallable;
import org.deegree_impl.tools.Debug;

/**
 * Used to render a "graphic" at a point. If a line-string or polygon geometry
 * is used with this symbol, then the semantic is to use the centroid of the
 * geometry, or any similar representative point. The meaning of the contained
 * elements are discussed with the element definitions below. If the Geometry
 * element is omitted, then the "default" geometry for the feature type is used.
 * (Many feature types will have only one geometry attribute.) If the Graphic
 * element is omitted, then nothing will be plotted.
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:k.lupp@web.de">Katharina Lupp </a>
 * @version $Revision$ $Date$
 */
public class PointSymbolizer_Impl extends Symbolizer_Impl implements PointSymbolizer, Marshallable
{
  private Graphic graphic = null;

  /**
   * Creates a new PointSymbolizer_Impl object.
   */
  public PointSymbolizer_Impl()
  {
    super();
    Stroke stroke = new Stroke_Impl();
    Fill fill = new Fill_Impl();
    Mark mark = new Mark_Impl( "square", stroke, fill );
    graphic = StyleFactory.createGraphic( null, mark, 1, 5, 0 );
  }

  /**
   * constructor initializing the class with the <PointSymbolizer>
   */
  PointSymbolizer_Impl( Graphic graphic, Geometry geometry, double min, double max )
  {
    super( geometry );

    if( graphic == null )
    {
      graphic = new Graphic_Impl();
    }

    setGraphic( graphic );
    setMinScaleDenominator( min );
    setMaxScaleDenominator( max );
  }

  /**
   * A Graphic is a "graphic symbol" with an inherent shape, color, and size.
   * Graphics can either be referenced from an external URL in a common format
   * (such as GIF or SVG) or may be derived from a Mark. Multiple external URLs
   * may be referenced with the semantic that they all provide the same graphic
   * in different formats. The "hot spot" to use for rendering at a point or the
   * start and finish handle points to use for rendering a graphic along a line
   * must either be inherent in the external format or are system- dependent.
   * The default size of an image format (such as GIF) is the inherent size of
   * the image. The default size of a format without an inherent size is 16
   * pixels in height and the corresponding aspect in width. If a size is
   * specified, the height of the graphic will be scaled to that size and the
   * corresponding aspect will be used for the width. The default if neither an
   * ExternalURL nor a Mark is specified is to use the default Mark with a size
   * of 6 pixels. The size is in pixels and the rotation is in degrees
   * clockwise, with 0 (default) meaning no rotation. In the case that a Graphic
   * is derived from a font-glyph Mark, the Size specified here will be used for
   * the final rendering. Allowed CssParameters are "opacity", "size", and
   * "rotation".
   * 
   * @return the graphic of the point
   */
  public Graphic getGraphic()
  {
    return graphic;
  }

  /**
   * sets the <Graphic>
   * 
   * @param graphic
   *          the graphic of the point
   */
  public void setGraphic( Graphic graphic )
  {
    this.graphic = graphic;
  }

  /**
   * exports the content of the PointSymbolizer as XML formated String
   * 
   * @return xml representation of the PointSymbolizer
   */
  public String exportAsXML()
  {
    Debug.debugMethodBegin();

    StringBuffer sb = new StringBuffer( 1000 );
    sb.append( "<PointSymbolizer>" );
    if( geometry != null )
    {
      sb.append( ( (Marshallable)geometry ).exportAsXML() );
    }
    if( graphic != null )
    {
      sb.append( ( (Marshallable)graphic ).exportAsXML() );
    }
    sb.append( "</PointSymbolizer>" );

    Debug.debugMethodEnd();
    return sb.toString();
  }
}