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
package org.kalypsodeegree_impl.graphics.sld;

import org.eclipse.swt.graphics.GC;
import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.graphics.sld.Fill;
import org.kalypsodeegree.graphics.sld.Geometry;
import org.kalypsodeegree.graphics.sld.Graphic;
import org.kalypsodeegree.graphics.sld.Mark;
import org.kalypsodeegree.graphics.sld.PointSymbolizer;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.xml.Marshallable;
import org.kalypsodeegree_impl.tools.Debug;

/**
 * Used to render a "graphic" at a point. If a line-string or polygon geometry is used with this symbol, then the
 * semantic is to use the centroid of the geometry, or any similar representative point. The meaning of the contained
 * elements are discussed with the element definitions below. If the Geometry element is omitted, then the "default"
 * geometry for the feature type is used. (Many feature types will have only one geometry attribute.) If the Graphic
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
  private Graphic m_graphic = null;

  /**
   * Creates a new PointSymbolizer_Impl object.
   */
  public PointSymbolizer_Impl( )
  {
    super();
    final Stroke stroke = new Stroke_Impl();
    final Fill fill = new Fill_Impl();
    final Mark mark = new Mark_Impl( "square", stroke, fill );
    m_graphic = StyleFactory.createGraphic( null, mark, 1, 5, 0 );
  }

  /**
   * constructor initializing the class with the <PointSymbolizer>
   */
  PointSymbolizer_Impl( Graphic graphic, final Geometry geometry, final double min, final double max, final UOM uom )
  {
    super( geometry, uom );

    if( graphic == null )
    {
      graphic = new Graphic_Impl();
    }

    setGraphic( graphic );
    setMinScaleDenominator( min );
    setMaxScaleDenominator( max );
  }

  /**
   * @see org.kalypsodeegree_impl.graphics.sld.Symbolizer_Impl#paint(org.eclipse.swt.graphics.GC,
   *      org.kalypsodeegree.model.feature.Feature)
   */
  @Override
  public void paint( final GC gc, final Feature feature ) throws FilterEvaluationException
  {
    if( m_graphic == null )
      super.paint( gc, feature );

    m_graphic.paint( gc, feature );
  }

  /**
   * A Graphic is a "graphic symbol" with an inherent shape, color, and size. Graphics can either be referenced from an
   * external URL in a common format (such as GIF or SVG) or may be derived from a Mark. Multiple external URLs may be
   * referenced with the semantic that they all provide the same graphic in different formats. The "hot spot" to use for
   * rendering at a point or the start and finish handle points to use for rendering a graphic along a line must either
   * be inherent in the external format or are system- dependent. The default size of an image format (such as GIF) is
   * the inherent size of the image. The default size of a format without an inherent size is 16 pixels in height and
   * the corresponding aspect in width. If a size is specified, the height of the graphic will be scaled to that size
   * and the corresponding aspect will be used for the width. The default if neither an ExternalURL nor a Mark is
   * specified is to use the default Mark with a size of 6 pixels. The size is in pixels and the rotation is in degrees
   * clockwise, with 0 (default) meaning no rotation. In the case that a Graphic is derived from a font-glyph Mark, the
   * Size specified here will be used for the final rendering. Allowed CssParameters are "opacity", "size", and
   * "rotation".
   * 
   * @return the graphic of the point
   */
  public Graphic getGraphic( )
  {
    return m_graphic;
  }

  /**
   * sets the <Graphic>
   * 
   * @param graphic
   *            the graphic of the point
   */
  public void setGraphic( final Graphic graphic )
  {
    this.m_graphic = graphic;
  }

  /**
   * exports the content of the PointSymbolizer as XML formated String
   * 
   * @return xml representation of the PointSymbolizer
   */
  public String exportAsXML( )
  {
    Debug.debugMethodBegin();

    final StringBuffer sb = new StringBuffer( 1000 );
    sb.append( "<PointSymbolizer" );

    final UOM uom = getUom();
    if( uom != null )
    {
      sb.append( " uom=\"" + uom.name() + "\">\n" );
    }
    else
      sb.append( ">\n" );

    final Geometry geometry = getGeometry();
    if( geometry != null )
    {
      sb.append( ((Marshallable) geometry).exportAsXML() );
    }
    if( m_graphic != null )
    {
      sb.append( ((Marshallable) m_graphic).exportAsXML() );
    }
    sb.append( "</PointSymbolizer>" );

    Debug.debugMethodEnd();
    return sb.toString();
  }
}