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

/**
 * Used to render a text label, according to the parameters. A missing Geometry,
 * Label, Font, or LabelPlacement element selects the default value or behavior
 * for the element. The default Label, Font, and LabelPlacement are system-
 * dependent. Multiple Font elements may be used to specify alternate fonts in
 * order of preference in case a map server does not support the first
 * preference. A missing Halo or Fill element means that no halo or fill will be
 * plotted, respectively. The Fill is rendered over top of the Halo, and the
 * Halo includes the interiors of the font glyphs.
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public interface TextSymbolizer extends Symbolizer
{

  /**
   * returns the Label as a <tt>ParameterValueType</tt> to be renderd
   * 
   * @return the label
   */
  ParameterValueType getLabel();

  /**
   * sets the Label as a <tt>ParameterValueType</tt> to be renderd
   * 
   * @param label
   *          the label
   */
  void setLabel( ParameterValueType label );

  /**
   * Identifies a Font of a certain family, style, and size.
   * 
   * @return the font
   */
  Font getFont();

  /**
   * sets the Font
   * 
   * @param font
   *          the font
   */
  void setFont( Font font );

  /**
   * Used to position a label relative to a point or a line string. For a point,
   * you can specify the anchor point of the label and a linear displacement
   * from the point (so that you can also plot a graphic symbol at the point).
   * For a line-string placement, you can specify a perpendicular offset (so you
   * can draw a stroke on the line).
   * <p>
   * </p>
   * MORE PARAMETERS ARE PROBABLY NEEDED HERE.
   * 
   * @return the labelPlacement
   */
  LabelPlacement getLabelPlacement();

  /**
   * Sets the LabelPlacement
   * 
   * @param labelPlacement
   *          the labelPlacement
   */
  void setLabelPlacement( LabelPlacement labelPlacement );

  /**
   * A Halo is an extension (sub-type) of a Fill and is applied to the
   * backgrounds of font glyphs. Either a Radius or a Block halo type can be
   * used. The radius is computed from the outside edge of the font glyph (or
   * inside of "holes"). The default is a Radius of 1.0 (pixels) but if no Halo
   * is selected in a containing structure, no halo will be rendered. The
   * default is a solid white (Color "#FFFFFF") opaque halo.
   * 
   * @return the halo
   */
  Halo getHalo();

  /**
   * Sets the Halo.
   * 
   * @param halo
   *          the halo
   */
  void setHalo( Halo halo );

  /**
   * A Fill allows area geometries to be filled. There are two types of fills:
   * solid-color and repeated GraphicFill. In general, if a Fill element is
   * omitted in its containing element, no fill will be rendered. The default is
   * a solid 50%-gray (color "#808080") opaque fill.
   * 
   * @return the fill
   */
  Fill getFill();

  /**
   * Sets the Fill.
   * 
   * @param fill
   *          the fill
   */
  void setFill( Fill fill );
}