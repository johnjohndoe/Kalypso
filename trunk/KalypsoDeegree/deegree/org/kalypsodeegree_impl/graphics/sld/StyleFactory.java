/*----------------    FILE HEADER  ------------------------------------------
 
 This file is part of deegree.
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon Fitzke/Fretter/Poth GbR
 http://www.lat-lon.de
 
 The basic version of this class was taken from the Geotools2 
 project (StyleBuilder.java):
 Geotools2 - OpenSource mapping toolkit
 http://geotools.org
 (C) 2002, Geotools Project Managment Committee (PMC)
 
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

import java.awt.Color;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;

import org.deegree.graphics.sld.CssParameter;
import org.deegree.graphics.sld.ExternalGraphic;
import org.deegree.graphics.sld.FeatureTypeStyle;
import org.deegree.graphics.sld.Fill;
import org.deegree.graphics.sld.Font;
import org.deegree.graphics.sld.Geometry;
import org.deegree.graphics.sld.Graphic;
import org.deegree.graphics.sld.GraphicFill;
import org.deegree.graphics.sld.GraphicStroke;
import org.deegree.graphics.sld.Halo;
import org.deegree.graphics.sld.LabelPlacement;
import org.deegree.graphics.sld.LegendGraphic;
import org.deegree.graphics.sld.LinePlacement;
import org.deegree.graphics.sld.LineSymbolizer;
import org.deegree.graphics.sld.Mark;
import org.deegree.graphics.sld.ParameterValueType;
import org.deegree.graphics.sld.PointPlacement;
import org.deegree.graphics.sld.PointSymbolizer;
import org.deegree.graphics.sld.PolygonSymbolizer;
import org.deegree.graphics.sld.Rule;
import org.deegree.graphics.sld.Stroke;
import org.deegree.graphics.sld.Style;
import org.deegree.graphics.sld.Symbolizer;
import org.deegree.graphics.sld.TextSymbolizer;
import org.deegree.services.wfs.filterencoding.Expression;
import org.deegree.services.wfs.filterencoding.Filter;

/**
 * An utility class designed to easy creation of style by convinience methods.
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 */
public class StyleFactory
{

  /**
   * creates a <tt>ParameterValueType</tt> instance with a <tt>String</tt>
   * as value
   * 
   * @param value
   *          value of the <tt>ParameterValueType</tt>
   * 
   * @return the ParameterValueType created
   */
  public static ParameterValueType createParameterValueType( String value )
  {
    return new ParameterValueType_Impl( new Object[]
    { value } );
  }

  /**
   * creates a <tt>ParameterValueType</tt> instance with a <tt>int</tt> as
   * value
   * 
   * @param value
   *          value of the <tt>ParameterValueType</tt>
   * 
   * @return the ParameterValueType created
   */
  public static ParameterValueType createParameterValueType( int value )
  {
    return new ParameterValueType_Impl( new Object[]
    { "" + value } );
  }

  /**
   * creates a <tt>ParameterValueType</tt> instance with a <tt>String</tt>
   * as value
   * 
   * @param value
   *          value of the <tt>ParameterValueType</tt>
   * 
   * @return the ParameterValueType created
   */
  public static ParameterValueType createParameterValueType( double value )
  {
    return new ParameterValueType_Impl( new Object[]
    { "" + value } );
  }

  /**
   * creates a <tt>ParameterValueType</tt> instance with an array of
   * <tt>Expression</tt> s as value
   * 
   * @param expressions
   * 
   * @return the the ParameterValueType created
   */
  public static ParameterValueType createParameterValueType( Expression[] expressions )
  {
    return new ParameterValueType_Impl( expressions );
  }

  /**
   * creates a CssParameter with a name and a value
   * 
   * @param name
   *          name of the css parameter
   * @param value
   *          value of the css parameter
   * 
   * @return the CssParameter created
   */
  public static CssParameter createCssParameter( String name, String value )
  {
    ParameterValueType pvt = createParameterValueType( value );
    return new CssParameter_Impl( name, pvt );
  }

  /**
   * creates a CssParameter with a name and a value
   * 
   * @param name
   *          name of the css parameter
   * @param value
   *          value of the css parameter
   * 
   * @return the CssParameter created
   */
  public static CssParameter createCssParameter( String name, int value )
  {
    ParameterValueType pvt = createParameterValueType( value );
    return new CssParameter_Impl( name, pvt );
  }

  /**
   * creates a CssParameter with a name and a value
   * 
   * @param name
   *          name of the css parameter
   * @param value
   *          value of the css parameter
   * 
   * @return the CssParameter created
   */
  public static CssParameter createCssParameter( String name, double value )
  {
    ParameterValueType pvt = createParameterValueType( value );
    return new CssParameter_Impl( name, pvt );
  }

  /**
   * creates a <tt>GraphicStroke</tt> from a <tt>Graphic</tt> object
   * 
   * @param graphic
   *          <tt>Graphic</tt object
   *
   * @return the GraphicStroke created
   */
  public static GraphicStroke createGraphicStroke( Graphic graphic )
  {
    return new GraphicStroke_Impl( graphic );
  }

  /**
   * creates a <tt>GraphicFill</tt> from a <tt>Graphic</tt> object
   * 
   * @param graphic
   *          <tt>Graphic</tt object
   *
   * @return the GraphicFill created
   */
  public static GraphicFill createGraphicFill( Graphic graphic )
  {
    return new GraphicFill_Impl( graphic );
  }

  /**
   * create a default Stroke that black, 1 pixel width, complete opaque, with
   * round linejoin and square line cap
   * 
   * @return the Stroke created
   */
  public static Stroke createStroke()
  {
    return createStroke( Color.BLACK, 1, "round", "square" );
  }

  /**
   * create a default stroke with the supplied width
   * 
   * @param width
   *          the width of the line
   * 
   * @return the stroke created
   */
  public static Stroke createStroke( double width )
  {
    return createStroke( Color.BLACK, width );
  }

  /**
   * Create a default stroke with the supplied color
   * 
   * @param color
   *          the color of the line
   * 
   * @return the created stroke
   */
  public static Stroke createStroke( Color color )
  {
    return createStroke( color, 1 );
  }

  /**
   * create a stroke with the passed width and color
   * 
   * @param color
   *          the color of the line
   * @param width
   *          the width of the line
   * 
   * @return the created stroke
   */
  public static Stroke createStroke( Color color, double width )
  {
    return createStroke( color, width, "round", "square" );
  }

  /**
   * create a stroke with color, width, linejoin type and lineCap type.
   * 
   * @param color
   *          the color of the line
   * @param width
   *          the width of the line
   * @param lineJoin
   *          the type of join to be used at points along the line
   * @param lineCap
   *          the type of cap to be used at the end of the line
   * 
   * @return the stroke created
   */
  public static Stroke createStroke( Color color, double width, String lineJoin, String lineCap )
  {
    return createStroke( color, width, 1, null, lineJoin, lineCap );
  }

  /**
   * create a stroke with color, width, linejoin type and lineCap type.
   * 
   * @param color
   *          the color of the line
   * @param width
   *          the width of the line
   * @param opacity
   *          the opacity or <I>see throughness </I> of the line, 0 - is
   *          transparent, 1 is completely drawn
   * @param lineJoin
   *          the type of join to be used at points along the line
   * @param lineCap
   *          the type of cap to be used at the end of the line
   * 
   * @return the stroke created
   */
  public static Stroke createStroke( Color color, double width, double opacity, float[] dashArray,
      String lineJoin, String lineCap )
  {
    HashMap cssParams = new HashMap();

    CssParameter stroke = createCssParameter( "stroke", getColorAsHex( color ) );
    cssParams.put( "stroke", stroke );
    CssParameter strokeOp = createCssParameter( "stroke-opacity", opacity );
    cssParams.put( "stroke-opacity", strokeOp );
    CssParameter strokeWi = createCssParameter( "stroke-width", width );
    cssParams.put( "stroke-width", strokeWi );
    CssParameter strokeLJ = createCssParameter( "stroke-linejoin", lineJoin );
    cssParams.put( "stroke-linejoin", strokeLJ );
    CssParameter strokeCap = createCssParameter( "stroke-linecap", lineCap );
    cssParams.put( "stroke-linecap", strokeCap );

    if( dashArray != null )
    {
      String s = "";
      for( int i = 0; i < dashArray.length - 1; i++ )
      {
        s = s + dashArray[i] + ",";
      }
      s = s + dashArray[dashArray.length - 1];
      CssParameter strokeDash = createCssParameter( "stroke-dasharray", s );
      cssParams.put( "stroke-dasharray", strokeDash );
    }

    return new Stroke_Impl( cssParams, null, null );
  }

  /**
   * create a dashed line of color and width
   * 
   * @param color
   *          the color of the line
   * @param width
   *          the width of the line
   * @param dashArray
   *          an array of floats describing the length of line and spaces
   * 
   * @return the stroke created
   */
  public static Stroke createStroke( Color color, double width, float[] dashArray )
  {
    HashMap cssParams = new HashMap();

    CssParameter stroke = createCssParameter( "stroke", getColorAsHex( color ) );
    cssParams.put( "stroke", stroke );
    CssParameter strokeOp = createCssParameter( "stroke-opacity", "1" );
    cssParams.put( "stroke-opacity", strokeOp );
    CssParameter strokeWi = createCssParameter( "stroke-width", width );
    cssParams.put( "stroke-width", strokeWi );
    CssParameter strokeLJ = createCssParameter( "stroke-linejoin", "mitre" );
    cssParams.put( "stroke-linejoin", strokeLJ );
    CssParameter strokeCap = createCssParameter( "stroke-linejoin", "butt" );
    cssParams.put( "stroke-linecap", strokeCap );

    if( dashArray != null )
    {
      String s = "";
      for( int i = 0; i < dashArray.length - 1; i++ )
      {
        s = s + dashArray[i] + ",";
      }
      s = s + dashArray[dashArray.length - 1];
      CssParameter strokeDash = createCssParameter( "stroke-dasharray", s );
      cssParams.put( "stroke-dasharray", strokeDash );
    }

    return new Stroke_Impl( cssParams, null, null );
  }

  /**
   * create a stroke with color, width and opacity supplied
   * 
   * @param color
   *          the color of the line
   * @param width
   *          the width of the line
   * @param opacity
   *          the opacity or <I>see throughness </I> of the line, 0 - is
   *          transparent, 1 is completely drawn
   * 
   * @return the stroke created
   */
  public static Stroke createStroke( Color color, double width, double opacity )
  {
    return createStroke( color, width, opacity, null, "mitre", "butt" );
  }

  /**
   * create a default fill 50% gray
   * 
   * @return the fill created
   */
  public static Fill createFill()
  {
    return createFill( Color.GRAY, 1d, null );
  }

  /**
   * create a fill of color
   * 
   * @param color
   *          the color of the fill
   * 
   * @return the fill created
   */
  public static Fill createFill( Color color )
  {
    return createFill( color, 1d, null );
  }

  /**
   * create a fill with the supplied color and opacity
   * 
   * @param color
   *          the color to fill with
   * @param opacity
   *          the opacity of the fill 0 - transparent, 1 - completly filled
   * 
   * @return the fill created
   */
  public static Fill createFill( Color color, double opacity )
  {
    return createFill( color, opacity, null );
  }

  /**
   * create a fill with color and opacity supplied and uses the graphic fill
   * supplied for the fill
   * 
   * @param color
   *          the foreground color
   * @param opacity
   *          the opacity of the fill
   * @param fill
   *          the graphic object to use to fill the fill
   * 
   * @return the fill created
   */
  public static Fill createFill( Color color, double opacity, GraphicFill fill )
  {
    HashMap cssParams = new HashMap();
    CssParameter fillCo = createCssParameter( "fill", getColorAsHex( color ) );
    cssParams.put( "fill", fillCo );
    CssParameter fillOp = createCssParameter( "fill-opacity", opacity );
    cssParams.put( "fill-opacity", fillOp );
    return new Fill_Impl( cssParams, fill );
  }

  /**
   * create the named mark
   * 
   * @param wellKnownName
   *          the wellknown name of the mark
   * 
   * @return the mark created
   */
  public static Mark createMark( String wellKnownName )
  {
    return new Mark_Impl( wellKnownName, createStroke(), createFill() );
  }

  /**
   * create the named mark with the colors etc supplied
   * 
   * @param wellKnownName
   *          the well known name of the mark
   * @param fillColor
   *          the color of the mark
   * @param borderColor
   *          the outline color of the mark
   * @param borderWidth
   *          the width of the outline
   * 
   * @return the mark created
   */
  public static Mark createMark( String wellKnownName, Color fillColor, Color borderColor,
      double borderWidth )
  {
    Stroke stroke = createStroke( borderColor, borderWidth );
    Fill fill = createFill( fillColor );
    return new Mark_Impl( wellKnownName, stroke, fill );
  }

  /**
   * create a mark with default fill (50% gray) and the supplied outline
   * 
   * @param wellKnownName
   *          the well known name of the mark
   * @param borderColor
   *          the outline color
   * @param borderWidth
   *          the outline width
   * 
   * @return the mark created
   */
  public static Mark createMark( String wellKnownName, Color borderColor, double borderWidth )
  {
    Stroke stroke = createStroke( borderColor, borderWidth );
    Fill fill = createFill();
    return new Mark_Impl( wellKnownName, stroke, fill );
  }

  /**
   * create a mark of the supplied color and a default outline (black)
   * 
   * @param wellKnownName
   *          the well known name of the mark
   * @param fillColor
   *          the color of the mark
   * 
   * @return the created mark
   */
  public static Mark createMark( String wellKnownName, Color fillColor )
  {
    Stroke stroke = createStroke();
    Fill fill = createFill( fillColor );
    return new Mark_Impl( wellKnownName, stroke, fill );
  }

  /**
   * create a mark with the supplied fill and stroke
   * 
   * @param wellKnownName
   *          the well known name of the mark
   * @param fill
   *          the fill to use
   * @param stroke
   *          the stroke to use
   * 
   * @return the mark created
   */
  public static Mark createMark( String wellKnownName, Fill fill, Stroke stroke )
  {
    return new Mark_Impl( wellKnownName, stroke, fill );
  }

  /**
   * wrapper for stylefactory method
   * 
   * @param uri
   *          the uri of the image
   * @param format
   *          mime type of the image
   * 
   * @return the external graphic
   */
  public static ExternalGraphic createExternalGraphic( String uri, String format )
      throws MalformedURLException
  {
    return createExternalGraphic( new URL( uri ), format );
  }

  /**
   * wrapper for stylefactory method
   * 
   * @param url
   *          the url of the image
   * @param format
   *          mime type of the image
   * 
   * @return the external graphic
   */
  public static ExternalGraphic createExternalGraphic( java.net.URL url, String format )
      throws MalformedURLException
  {
    return new ExternalGraphic_Impl( format, url );
  }

  /**
   * creates a graphic object
   * 
   * @param externalGraphic
   *          an external graphic to use if displayable
   * @param mark
   *          a mark to use
   * @param opacity -
   *          the opacity of the graphic
   * @param size -
   *          the size of the graphic
   * @param rotation -
   *          the rotation from the top of the page of the graphic
   * 
   * @return the graphic created
   */
  public static Graphic createGraphic( ExternalGraphic externalGraphic, Mark mark, double opacity,
      double size, double rotation )
  {

    Object[] mae = null;
    if( externalGraphic != null && mark != null )
    {
      mae = new Object[]
      { externalGraphic, mark };
    }
    else if( externalGraphic != null )
    {
      mae = new Object[]
      { externalGraphic };
    }
    else if( mark != null )
    {
      mae = new Object[]
      { mark };
    }
    ParameterValueType op_pvt = createParameterValueType( opacity );
    ParameterValueType sz_pvt = createParameterValueType( size );
    ParameterValueType ro_pvt = createParameterValueType( rotation );
    return new Graphic_Impl( mae, op_pvt, sz_pvt, ro_pvt );
  }

  /**
   * wrapper round Stylefactory Method
   * 
   * @return the default pointplacement
   */
  public static PointPlacement createPointPlacement()
  {
    return new PointPlacement_Impl();
  }

  /**
   * wrapper round Stylefactory Method
   * 
   * @param anchorX -
   *          the X coordinate
   * @param anchorY -
   *          the Y coordinate
   * @param rotation -
   *          the rotaion of the label
   * 
   * @return the pointplacement created
   */
  public static PointPlacement createPointPlacement( double anchorX, double anchorY, double rotation )
  {
    ParameterValueType pvt1 = createParameterValueType( anchorX );
    ParameterValueType pvt2 = createParameterValueType( anchorY );
    ParameterValueType[] anchorPoint = new ParameterValueType[]
    { pvt1, pvt2 };
    ParameterValueType rot = createParameterValueType( rotation );
    return new PointPlacement_Impl( anchorPoint, null, rot, false );
  }

  /**
   * wrapper round Stylefactory Method
   * 
   * @param anchorX -
   *          the X coordinate
   * @param anchorY -
   *          the Y coordinate
   * @param displacementX -
   *          the X distance from the anchor
   * @param displacementY -
   *          the Y distance from the anchor
   * @param rotation -
   *          the rotaion of the label
   * 
   * @return the pointplacement created
   */
  public static PointPlacement createPointPlacement( double anchorX, double anchorY,
      double displacementX, double displacementY, double rotation )
  {
    ParameterValueType pvt1 = createParameterValueType( anchorX );
    ParameterValueType pvt2 = createParameterValueType( anchorY );
    ParameterValueType[] anchorPoint = new ParameterValueType[]
    { pvt1, pvt2 };

    ParameterValueType pvt3 = createParameterValueType( displacementX );
    ParameterValueType pvt4 = createParameterValueType( displacementY );
    ParameterValueType[] displacement = new ParameterValueType[]
    { pvt3, pvt4 };

    ParameterValueType rot = createParameterValueType( rotation );
    return new PointPlacement_Impl( anchorPoint, displacement, rot, false );
  }

  /**
   * 
   * @param anchorX -
   *          the X coordinate
   * @param anchorY -
   *          the Y coordinate
   * @param displacementX -
   *          the X distance from the anchor
   * @param displacementY -
   *          the Y distance from the anchor
   * @param rotation -
   *          the rotaion of the label
   * @param auto -
   *          auto positioning of the label
   * 
   * @return the pointplacement created
   */
  public static PointPlacement createPointPlacement( double anchorX, double anchorY,
      double displacementX, double displacementY, double rotation, boolean auto )
  {
    ParameterValueType pvt1 = createParameterValueType( anchorX );
    ParameterValueType pvt2 = createParameterValueType( anchorY );
    ParameterValueType[] anchorPoint = new ParameterValueType[]
    { pvt1, pvt2 };

    ParameterValueType pvt3 = createParameterValueType( displacementX );
    ParameterValueType pvt4 = createParameterValueType( displacementY );
    ParameterValueType[] displacement = new ParameterValueType[]
    { pvt3, pvt4 };

    ParameterValueType rot = createParameterValueType( rotation );
    return new PointPlacement_Impl( anchorPoint, displacement, rot, auto );
  }

  /**
   * creates a <tt>LinePlacement</tt> with a user defined distance between the
   * labels and the lines. A positive value indicates a position above the line,
   * a negative value indicates a position below. The line width is asumed to be
   * 2 pixel and the gap between the labels is set to factor 10 of the label
   * width.
   * 
   * @param offset -
   *          the distance between the line and the label
   * 
   * @return the LinePlacement created
   */
  public static LinePlacement createLinePlacement( double offset )
  {

    ParameterValueType perpendicularOffset = createParameterValueType( offset );
    ParameterValueType lineWidth = createParameterValueType( 2 );
    ParameterValueType gap = createParameterValueType( 10 );

    return new LinePlacement_Impl( perpendicularOffset, lineWidth, gap );
  }

  /**
   * creates a <tt>LinePlacement</tt> with a relative position of the label
   * according to the line the lines. The line width is asumed to be 2 pixel and
   * the gap between the labels is set to factor 10 of the label width.
   * 
   * @param position
   *          of the label relative to the line
   * 
   * @return the LinePlacement created
   */
  public static LinePlacement createLinePlacement( String position )
  {

    ParameterValueType perpendicularOffset = createParameterValueType( position );
    ParameterValueType lineWidth = createParameterValueType( 2 );
    ParameterValueType gap = createParameterValueType( 10 );

    return new LinePlacement_Impl( perpendicularOffset, lineWidth, gap );
  }

  /**
   * creates a <tt>LinePlacement</tt> with a user defined distance between the
   * labels and the lines. A positive value indicates a position above the line,
   * a negative value indicates a position below.
   * 
   * @param offset -
   *          the distance between the line and the label
   * @param lineWidth -
   *          assumed lineWidth
   * @param gap -
   *          gap between the labels measured in label width
   * 
   * @return the LinePlacement created
   */
  public static LinePlacement createLinePlacement( double offset, double lineWidth, int gap )
  {

    ParameterValueType perpendicularOffset = createParameterValueType( offset );
    ParameterValueType lineWidth_ = createParameterValueType( lineWidth );
    ParameterValueType gap_ = createParameterValueType( gap );

    return new LinePlacement_Impl( perpendicularOffset, lineWidth_, gap_ );
  }

  /**
   * creates a <tt>LinePlacement</tt> with a user defined distance between the
   * labels and the lines. A positive value indicates a position above the line,
   * a negative value indicates a position below.
   * 
   * @param position -
   *          relative position of the label to the line
   * @param lineWidth -
   *          assumed lineWidth
   * @param gap -
   *          gap between the labels measured in label width
   * 
   * @return the LinePlacement created
   */
  public static LinePlacement createLinePlacement( String position, double lineWidth, int gap )
  {

    ParameterValueType perpendicularOffset = createParameterValueType( position );
    ParameterValueType lineWidth_ = createParameterValueType( lineWidth );
    ParameterValueType gap_ = createParameterValueType( gap );

    return new LinePlacement_Impl( perpendicularOffset, lineWidth_, gap_ );
  }

  /**
   * creates a label placement that is orientated on a line
   * 
   * @param linePlacement
   *          description of the line where the lable will be orientated on
   * @return created LabelPlacement
   */
  public static LabelPlacement createLabelPlacement( LinePlacement linePlacement )
  {
    return new LabelPlacement_Impl( linePlacement );
  }

  /**
   * creates a label placement that is orientated on a point
   * 
   * @param pointPlacement
   *          description of the point where the lable will be orientated on
   * @return created LabelPlacement
   */
  public static LabelPlacement createLabelPlacement( PointPlacement pointPlacement )
  {
    return new LabelPlacement_Impl( pointPlacement );
  }

  /**
   * create a geotools font object from a java font
   * 
   * @param font -
   *          the font to be converted
   * 
   * @return - the deegree sld font
   */
  public static Font createFont( java.awt.Font font )
  {
    return createFont( font.getFamily(), font.isItalic(), font.isBold(), font.getSize() );
  }

  /**
   * create font of supplied family and size
   * 
   * @param fontFamily -
   *          the font family
   * @param fontSize -
   *          the size of the font in points
   * 
   * @return the font object created
   */
  public static Font createFont( String fontFamily, double fontSize )
  {
    return createFont( fontFamily, false, false, fontSize );
  }

  /**
   * create font of supplied family, size and weight/style
   * 
   * @param fontFamily -
   *          the font family
   * @param italic -
   *          should the font be italic?
   * @param bold -
   *          should the font be bold?
   * @param fontSize -
   *          the size of the font in points
   * 
   * @return the new font object
   */
  public static Font createFont( String fontFamily, boolean italic, boolean bold, double fontSize )
  {
    HashMap cssParams = new HashMap();

    cssParams.put( "font-family", createCssParameter( "font-family", fontFamily ) );
    cssParams.put( "font-size", createCssParameter( "font-size", "" + fontSize ) );
    if( bold )
    {
      cssParams.put( "font-weight", createCssParameter( "font-weight", "bold" ) );
    }
    else
    {
      cssParams.put( "font-weight", createCssParameter( "font-weight", "normal" ) );
    }
    if( italic )
    {
      cssParams.put( "font-style", createCssParameter( "font-style", "italic" ) );
    }
    else
    {
      cssParams.put( "font-style", createCssParameter( "font-style", "normal" ) );
    }

    return new Font_Impl( cssParams );
  }

  /**
   * wrapper round StyleFactory method to create default halo
   * 
   * @return the new halo
   */
  public static Halo createHalo()
  {
    return createHalo( createFill(), createStroke(), -1 );
  }

  /**
   * wrapper round StyleFactory method to create halo
   * 
   * @param color -
   *          the color of the halo
   * @param radius -
   *          the radius of the halo use a value <= 0 for rectangle
   * 
   * @return the new halo
   */
  public static Halo createHalo( Color color, double radius )
  {
    return createHalo( createFill( color ), createStroke(), radius );
  }

  /**
   * wrapper round StyleFactory method to create halo
   * 
   * @param fillColor -
   *          the fill color of the halo
   * @param opacity -
   *          the opacity of the halo fill 0 - transparent 1 - solid
   * @param strokeColor -
   *          the stroke color of the halo
   * @param radius -
   *          the radius of the halo use a value <= 0 for rectangle
   * 
   * @return the new halo
   */
  public static Halo createHalo( Color fillColor, double opacity, Color strokeColor, double radius )
  {
    Fill fill = createFill( fillColor, opacity );
    Stroke stroke = createStroke( strokeColor );
    return createHalo( fill, stroke, radius );
  }

  /**
   * wrapper round StyleFactory method to create halo
   * 
   * @param fill -
   *          the fill of the halo
   * @param stroke -
   *          the stroke of the halo
   * @param radius -
   *          the radius of the halo use a value <= 0 for rectangle
   * 
   * @return the new halo
   */
  public static Halo createHalo( Fill fill, Stroke stroke, double radius )
  {
    ParameterValueType pvt = null;
    if( radius > 0 )
    {
      pvt = createParameterValueType( radius );
    }
    return new Halo_Impl( pvt, fill, stroke );
  }

  /**
   * create a default line symboliser
   * 
   * @return the new line symbolizer
   */
  public static LineSymbolizer createLineSymbolizer()
  {
    return createLineSymbolizer( createStroke( 1 ), null );
  }

  /**
   * create a new line symbolizer
   * 
   * @param width
   *          the width of the line
   * 
   * @return the new line symbolizer
   */
  public static LineSymbolizer createLineSymbolizer( double width )
  {
    return createLineSymbolizer( createStroke( width ), null );
  }

  /**
   * create a LineSymbolizer
   * 
   * @param color -
   *          the color of the line
   * 
   * @return the new line symbolizer
   */
  public static LineSymbolizer createLineSymbolizer( Color color )
  {
    return createLineSymbolizer( createStroke( color ), null );
  }

  /**
   * create a LineSymbolizer
   * 
   * @param color -
   *          the color of the line
   * @param width -
   *          the width of the line
   * 
   * @return the new line symbolizer
   */
  public static LineSymbolizer createLineSymbolizer( Color color, double width )
  {
    return createLineSymbolizer( createStroke( color, width ), null );
  }

  /**
   * create a LineSymbolizer
   * 
   * @param color -
   *          the color of the line
   * @param width -
   *          the width of the line
   * @param geometryPropertyName -
   *          the name of the geometry to be drawn
   * 
   * @return the new line symbolizer
   */
  public static LineSymbolizer createLineSymbolizer( Color color, double width,
      String geometryPropertyName )
  {
    return createLineSymbolizer( createStroke( color, width ), geometryPropertyName );
  }

  /**
   * create a LineSymbolizer
   * 
   * @param stroke -
   *          the stroke to be used to draw the line
   * 
   * @return the new line symbolizer
   */
  public static LineSymbolizer createLineSymbolizer( Stroke stroke )
  {
    return createLineSymbolizer( stroke, null );
  }

  /**
   * create a LineSymbolizer
   * 
   * @param stroke -
   *          the stroke to be used to draw the line
   * @param geometryPropertyName -
   *          the name of the geometry to be drawn
   * 
   * @return the new line symbolizer
   */
  public static LineSymbolizer createLineSymbolizer( Stroke stroke, String geometryPropertyName )
  {
    return createLineSymbolizer( stroke, geometryPropertyName, 0, Double.MAX_VALUE );
  }

  /**
   * create a LineSymbolizer
   * 
   * @param stroke -
   *          the stroke to be used to draw the line
   * @param geometryPropertyName -
   *          the name of the geometry to be drawn
   * @param min
   *          min scale denominator
   * @param max
   *          max scale denominator
   * 
   * @return the new line symbolizer
   */
  public static LineSymbolizer createLineSymbolizer( Stroke stroke, String geometryPropertyName,
      double min, double max )
  {
    Geometry geom = null;    
    if( geometryPropertyName != null )
    {
    	geom = new Geometry_Impl( geometryPropertyName, null );
    }   
    return new LineSymbolizer_Impl( stroke, geom, min, max );
  }

  /**
   * create a default polygon symbolizer
   * 
   * @return the new polygon symbolizer
   */
  public static PolygonSymbolizer createPolygonSymbolizer()
  {
    return createPolygonSymbolizer( createStroke(), createFill() );
  }

  /**
   * create a polygon symbolizer
   * 
   * @param fillColor -
   *          the color to fill the polygon
   * 
   * @return the new polygon symbolizer
   */
  public static PolygonSymbolizer createPolygonSymbolizer( Color fillColor )
  {
    return createPolygonSymbolizer( createStroke(), createFill( fillColor ) );
  }

  /**
   * create a polygon symbolizer
   * 
   * @param fillColor -
   *          the color to fill the polygon
   * @param borderColor -
   *          the outline color of the polygon
   * @param borderWidth -
   *          the width of the outline
   * 
   * @return the new polygon symbolizer
   */
  public static PolygonSymbolizer createPolygonSymbolizer( Color fillColor, Color borderColor,
      double borderWidth )
  {
    return createPolygonSymbolizer( createStroke( borderColor, borderWidth ),
        createFill( fillColor ) );
  }

  /**
   * create a polygon symbolizer
   * 
   * @param borderColor -
   *          the outline color of the polygon
   * @param borderWidth -
   *          the width of the outline
   * 
   * @return the new polygon symbolizer
   */
  public static PolygonSymbolizer createPolygonSymbolizer( Color borderColor, double borderWidth )
  {
    Stroke stroke = createStroke( borderColor, borderWidth );
    return createPolygonSymbolizer( stroke, createFill() );
  }

  /**
   * create a polygon symbolizer
   * 
   * @param stroke -
   *          the stroke to use to outline the polygon
   * @param fill -
   *          the fill to use to color the polygon
   * 
   * @return the new polygon symbolizer
   */
  public static PolygonSymbolizer createPolygonSymbolizer( Stroke stroke, Fill fill )
  {
    return createPolygonSymbolizer( stroke, fill, null );
  }

  /**
   * create a polygon symbolizer
   * 
   * @param stroke -
   *          the stroke to use to outline the polygon
   * @param fill -
   *          the fill to use to color the polygon
   * @param geometryPropertyName -
   *          the name of the geometry to be drawn
   * 
   * @return the new polygon symbolizer
   */
  public static PolygonSymbolizer createPolygonSymbolizer( Stroke stroke, Fill fill,
      String geometryPropertyName )
  {
    return createPolygonSymbolizer( stroke, fill, geometryPropertyName, 0, Double.MAX_VALUE );
  }

  /**
   * create a polygon symbolizer
   * 
   * @param stroke -
   *          the stroke to use to outline the polygon
   * @param fill -
   *          the fill to use to color the polygon
   * @param geometryPropertyName -
   *          the name of the geometry to be drawn
   * @param min
   *          min scale denominator
   * @param max
   *          max scale denominator
   * 
   * @return the new polygon symbolizer
   */
  public static PolygonSymbolizer createPolygonSymbolizer( Stroke stroke, Fill fill,
      String geometryPropertyName, double min, double max )
  {
    Geometry geom = null;
    if( geometryPropertyName != null )
    {
      geom = new Geometry_Impl( geometryPropertyName, null );
    }
    return new PolygonSymbolizer_Impl( fill, stroke, geom, min, max );
  }

  /**
   * create a default point symbolizer
   * 
   * @return the new point symbolizer
   */
  public static PointSymbolizer createPointSymbolizer()
  {
    Graphic graphic = createGraphic( null, null, 1, 5, 0 );
    return createPointSymbolizer( graphic );
  }

  /**
   * create a point symbolizer
   * 
   * @param graphic -
   *          the graphic object to draw at the point
   * 
   * @return the new point symbolizer
   */
  public static PointSymbolizer createPointSymbolizer( Graphic graphic )
  {
    return createPointSymbolizer( graphic, null );
  }

  /**
   * create a point symbolizer
   * 
   * @param graphic -
   *          the graphic object to draw at the point
   * @param geometryPropertyName -
   *          the name of the geometry to be drawn
   * 
   * @return the new point symbolizer
   */
  public static PointSymbolizer createPointSymbolizer( Graphic graphic, String geometryPropertyName )
  {
    // TODO evaluate geometry from geometryPropertyName
    return createPointSymbolizer( graphic, geometryPropertyName, 0, Double.MAX_VALUE );
  }

  /**
   * create a point symbolizer
   * 
   * @param graphic -
   *          the graphic object to draw at the point
   * @param geometryPropertyName -
   *          the name of the geometry to be drawn
   * @param min
   *          min scale denominator
   * @param max
   *          max scale denominator
   * 
   * @return the new point symbolizer
   */
  public static PointSymbolizer createPointSymbolizer( Graphic graphic,
      String geometryPropertyName, double min, double max )
  {
    Geometry geom = null;    
    if( geometryPropertyName != null )
    {
    	geom = new Geometry_Impl( geometryPropertyName, null );    	
    }
    return new PointSymbolizer_Impl( graphic, geom, min, max );
  }

  /**
   * create a textsymbolizer
   * 
   * @param color
   *          the color of the text
   * @param font
   *          the font to use
   * @param attributeName
   *          the attribute to use for the label
   * 
   * @return the new textsymbolizer
   *  
   */
  public static TextSymbolizer createTextSymbolizer( Color color, Font font, String attributeName,
      LabelPlacement labelPlacement )
  {
    ParameterValueType label = createParameterValueType( attributeName );
    Fill fill = createFill( color );
    Halo halo = createHalo();
    return createTextSymbolizer( null, label, font, labelPlacement, halo, fill, 0, Double.MAX_VALUE );
  }

  /**
   * create a textsymbolizer
   * 
   * @param geometryPropertyName
   *          geometry assigned to the TextSymbolizer
   * @param attribute
   *          attribute to draw/print
   * @param labelPlacement
   *          defines the placement of the text
   * 
   * @return the new textsymbolizer
   *  
   */
  public static TextSymbolizer createTextSymbolizer( String geometryPropertyName, String attribute,
      LabelPlacement labelPlacement )
  {
    Font font = createFont( java.awt.Font.decode( "Sans Serif" ) );
    return createTextSymbolizer( geometryPropertyName, attribute, font, labelPlacement,
        createHalo(), createFill(), 0, Double.MAX_VALUE );
  }

  /**
   * create a textsymbolizer
   * 
   * @param geometryPropertyName
   *          geometry assigned to the TextSymbolizer
   * @param attribute
   *          attribute to draw/print
   * @param font
   *          font to use for the text
   * @param labelPlacement
   *          defines the placement of the text
   * @param halo
   *          halo/backgroud of the text
   * @param fill
   *          color, opacity of the text
   * @param min
   *          min scale denominator
   * @param max
   *          max scale denominator
   * 
   * @return the new textsymbolizer
   *  
   */
  public static TextSymbolizer createTextSymbolizer( String geometryPropertyName, String attribute,
      Font font, LabelPlacement labelPlacement, Halo halo, Fill fill, double min, double max )
  {
    Geometry geom = null;
    if( geometryPropertyName != null )
    {
      geom = new Geometry_Impl( geometryPropertyName, null );
    }
    ParameterValueType label = createParameterValueType( attribute );
    return createTextSymbolizer( geom, label, font, labelPlacement, halo, fill, min, max );
  }

  /**
   * create a textsymbolizer
   * 
   * @param geometry
   *          geometry assigned to the TextSymbolizer
   * @param label
   *          attribute to draw/print
   * @param font
   *          font to use for the text
   * @param labelPlacement
   *          defines the placement of the text
   * @param halo
   *          halo/backgroud of the text
   * @param fill
   *          color, opacity of the text
   * @param min
   *          min scale denominator
   * @param max
   *          max scale denominator
   * 
   * @return the new textsymbolizer
   *  
   */
  public static TextSymbolizer createTextSymbolizer( Geometry geometry, ParameterValueType label,
      Font font, LabelPlacement labelPlacement, Halo halo, Fill fill, double min, double max )
  {
    return new TextSymbolizer_Impl( geometry, label, font, labelPlacement, halo, fill, min, max );
  }

  /**
   * create a textsymbolizer which doesn't change
   * 
   * @param color
   *          the color of the text
   * @param font
   *          the font to use
   * @param label
   *          the label to use
   * 
   * @return the new textsymbolizer
   */
  public static TextSymbolizer createStaticTextSymbolizer( Color color, Font font, String label )
  {
    return null;
  }

  /**
   * create a textsymbolizer which doesn't change
   * 
   * @param color
   *          the color of the text
   * @param fonts
   *          an array of fonts to use from the first to last
   * @param label
   *          the label to use
   * 
   * @return the new textsymbolizer
   */
  public static TextSymbolizer createStaticTextSymbolizer( Color color, Font[] fonts, String label )
  {
    return null;
  }

  /**
   * create a simple styling rule
   * 
   * @param symbolizer -
   *          the symbolizer to use
   * 
   * @return the new rule
   */
  public static Rule createRule( Symbolizer symbolizer )
  {
    return createRule( symbolizer, 0, Double.MAX_VALUE );
  }

  /**
   * reate a simple styling rule
   * 
   * @param symbolizers -
   *          an array of symbolizers to use
   * 
   * @return the new rule
   */
  public static Rule createRule( Symbolizer[] symbolizers )
  {
    return createRule( symbolizers, 0, Double.MAX_VALUE );
  }

  /**
   * create a simple styling rule, see the SLD Spec for more details of
   * scaleDenominators
   * 
   * @param symbolizer -
   *          the symbolizer to use
   * @param minScaleDenominator -
   *          the minimim scale to draw the feature at
   * @param maxScaleDenominator -
   *          the maximum scale to draw the feature at
   * 
   * @return the new rule
   */
  public static Rule createRule( Symbolizer symbolizer, double minScaleDenominator,
      double maxScaleDenominator )
  {
    return createRule( new Symbolizer[]
    { symbolizer }, minScaleDenominator, maxScaleDenominator );
  }

  /**
   * create a simple styling rule, see the SLD Spec for more details of
   * scaleDenominators
   * 
   * @param symbolizers -
   *          an array of symbolizers to use
   * @param minScaleDenominator -
   *          the minimim scale to draw the feature at
   * @param maxScaleDenominator -
   *          the maximum scale to draw the feature at
   * 
   * @return the new rule
   */
  public static Rule createRule( Symbolizer[] symbolizers, double minScaleDenominator,
      double maxScaleDenominator )
  {
    return createRule( symbolizers, "default", "default", "default", minScaleDenominator,
        maxScaleDenominator );
  }

  /**
   * create a simple styling rule, see the SLD Spec for more details of
   * scaleDenominators
   * 
   * @param symbolizers -
   *          an array of symbolizers to use
   * @param name -
   *          name of the rule
   * @param title -
   *          title of the rule
   * @param abstract_ -
   *          text describing throws rule
   * @param minScaleDenominator -
   *          the minimim scale to draw the feature at
   * @param maxScaleDenominator -
   *          the maximum scale to draw the feature at
   * 
   * @return the new rule
   */
  public static Rule createRule( Symbolizer[] symbolizers, String name, String title,
      String abstract_, double minScaleDenominator, double maxScaleDenominator )
  {
    return createRule( symbolizers, name, title, abstract_, null, null, false, minScaleDenominator,
        maxScaleDenominator );
  }

  /**
   * create a complex styling rule, see the SLD Spec for more details of
   * scaleDenominators
   * 
   * @param symbolizers -
   *          an array of symbolizers to use
   * @param name -
   *          name of the rule
   * @param title -
   *          title of the rule
   * @param abstract_ -
   *          text describing throws rule
   * @param filter -
   *          filter to use with the rule
   * @param elseFilter -
   *          true if the passed is an ElseFilter (see SLD spec)
   * @param minScaleDenominator -
   *          the minimim scale to draw the feature at
   * @param maxScaleDenominator -
   *          the maximum scale to draw the feature at
   * 
   * @return the new rule
   */
  public static Rule createRule( Symbolizer[] symbolizers, String name, String title,
      String abstract_, LegendGraphic legendGraphic, Filter filter, boolean elseFilter,
      double minScaleDenominator, double maxScaleDenominator )
  {
    return new Rule_Impl( symbolizers, name, title, abstract_, legendGraphic, filter, elseFilter,
        minScaleDenominator, maxScaleDenominator );
  }

  /**
   * create a Feature type styler
   * 
   * @param symbolizer -
   *          the symbolizer to use
   * 
   * @return the new feature type styler
   */
  public static FeatureTypeStyle createFeatureTypeStyle( Symbolizer symbolizer )
  {
    return createFeatureTypeStyle( null, symbolizer, 0, Double.MAX_VALUE );
  }

  /**
   * create a Feature type styler see the SLD Spec for more details of
   * scaleDenominators
   * 
   * @param symbolizer -
   *          the symbolizer to use
   * @param minScaleDenominator -
   *          the minimim scale to draw the feature at
   * @param maxScaleDenominator -
   *          the maximum scale to draw the feature at
   * 
   * @return the new feature type styler
   */
  public static FeatureTypeStyle createFeatureTypeStyle( Symbolizer symbolizer,
      double minScaleDenominator, double maxScaleDenominator )
  {
    return createFeatureTypeStyle( null, symbolizer, minScaleDenominator, maxScaleDenominator );
  }

  /**
   * create a Feature type styler see the SLD Spec for more details of
   * scaleDenominators
   * 
   * @param symbolizers -
   *          an array of symbolizers to use
   * @param minScaleDenominator -
   *          the minimim scale to draw the feature at
   * @param maxScaleDenominator -
   *          the maximum scale to draw the feature at
   * 
   * @return the new feature type styler
   */
  public static FeatureTypeStyle createFeatureTypeStyle( Symbolizer[] symbolizers,
      double minScaleDenominator, double maxScaleDenominator )
  {
    return createFeatureTypeStyle( null, symbolizers, minScaleDenominator, maxScaleDenominator );
  }

  /**
   * create a Feature type styler
   * 
   * @param featureTypeStyleName -
   *          name for the feature type styler
   * @param symbolizer -
   *          the symbolizer to use
   * 
   * @return the new feature type styler
   */
  public static FeatureTypeStyle createFeatureTypeStyle( String featureTypeStyleName,
      Symbolizer symbolizer )
  {
    return createFeatureTypeStyle( featureTypeStyleName, symbolizer, 0, Double.MAX_VALUE );
  }

  /**
   * create a Feature type styler
   * 
   * @param featureTypeStyleName -
   *          name for the feature type styler
   * @param symbolizers -
   *          an array of symbolizers to use
   * 
   * @return the new feature type styler
   */
  public static FeatureTypeStyle createFeatureTypeStyle( String featureTypeStyleName,
      Symbolizer[] symbolizers )
  {
    return createFeatureTypeStyle( featureTypeStyleName, symbolizers, 0, Double.MAX_VALUE );
  }

  /**
   * create a Feature type styler see the SLD Spec for more details of
   * scaleDenominators
   * 
   * @param featureTypeStyleName -
   *          name for the feature type styler
   * @param symbolizer -
   *          the symbolizer to use
   * @param minScaleDenominator -
   *          the minimim scale to draw the feature at
   * @param maxScaleDenominator -
   *          the maximum scale to draw the feature at
   * 
   * @return the new feature type styler
   */
  public static FeatureTypeStyle createFeatureTypeStyle( String featureTypeStyleName,
      Symbolizer symbolizer, double minScaleDenominator, double maxScaleDenominator )
  {
    return createFeatureTypeStyle( featureTypeStyleName, new Symbolizer[]
    { symbolizer }, minScaleDenominator, maxScaleDenominator );
  }

  /**
   * create a Feature type styler see the SLD Spec for more details of
   * scaleDenominators
   * 
   * @param featureTypeStyleName -
   *          name for the feature type styler
   * @param symbolizers -
   *          an array of symbolizers to use
   * @param minScaleDenominator -
   *          the minimim scale to draw the feature at
   * @param maxScaleDenominator -
   *          the maximum scale to draw the feature at
   * 
   * @return the new feature type styler
   */
  public static FeatureTypeStyle createFeatureTypeStyle( String featureTypeStyleName,
      Symbolizer[] symbolizers, double minScaleDenominator, double maxScaleDenominator )
  {
    Rule rule = createRule( symbolizers, minScaleDenominator, maxScaleDenominator );

    return createFeatureTypeStyle( featureTypeStyleName, rule );
  }

  /**
   * create a Feature type styler
   * 
   * @param rule -
   *          rule contained in the featureTypeStyle
   * 
   * @return the new feature type styler
   */
  public static FeatureTypeStyle createFeatureTypeStyle( Rule rule )
  {

    return createFeatureTypeStyle( new Rule[]
    { rule } );
  }

  /**
   * create a Feature type styler
   * 
   * @param rules -
   *          rules contained in the featureTypeStyle
   * 
   * @return the new feature type styler
   */
  public static FeatureTypeStyle createFeatureTypeStyle( Rule[] rules )
  {

    return createFeatureTypeStyle( null, rules );
  }

  /**
   * create a Feature type styler
   * 
   * @param featureTypeStyleName -
   *          name for the feature type styler
   * @param rule -
   *          rule contained in the featureTypeStyle
   * 
   * @return the new feature type styler
   */
  public static FeatureTypeStyle createFeatureTypeStyle( String featureTypeStyleName, Rule rule )
  {

    return createFeatureTypeStyle( featureTypeStyleName, new Rule[]
    { rule } );
  }

  /**
   * create a Feature type styler
   * 
   * @param featureTypeStyleName -
   *          name for the feature type styler
   * @param rules -
   *          rules contained in the featureTypeStyle
   * 
   * @return the new feature type styler
   */
  public static FeatureTypeStyle createFeatureTypeStyle( String featureTypeStyleName, Rule[] rules )
  {

    return createFeatureTypeStyle( featureTypeStyleName, null, null, null, rules );
  }

  /**
   * create a Feature type styler
   * 
   * @param featureTypeStyleName -
   *          name for the feature type styler
   * @param featureTypeName -
   *          name of the feature type the Feature type style shall be assigned
   *          to
   * @param title -
   *          title of the FeatureTypeStyle
   * @param abstract_ -
   *          text describing the FeatureTypeStyle
   * @param rules -
   *          rules contained in the featureTypeStyle
   * 
   * @return the new feature type styler
   */
  public static FeatureTypeStyle createFeatureTypeStyle( String featureTypeStyleName, String title,
      String abstract_, String featureTypeName, Rule[] rules )
  {

    return new FeatureTypeStyle_Impl( featureTypeStyleName, title, abstract_, featureTypeName,
        null, rules );
  }

  /**
   * create a new style
   * 
   * @param symbolizer -
   *          the symbolizer to use
   * 
   * @return the new style
   */
  public static Style createStyle( Symbolizer symbolizer )
  {
    return createStyle( null, symbolizer, 0, Double.MAX_VALUE );
  }

  /**
   * create a new style with name 'default'
   * 
   * @param symbolizer -
   *          the symbolizer to use
   * @param minScaleDenominator -
   *          the minimim scale to draw the feature at
   * @param maxScaleDenominator -
   *          the maximum scale to draw the feature at
   * 
   * @return the new style
   */
  public static Style createStyle( Symbolizer symbolizer, double minScaleDenominator,
      double maxScaleDenominator )
  {
    return createStyle( "default", symbolizer, minScaleDenominator, maxScaleDenominator );
  }

  /**
   * create a new style
   * 
   * @param name -
   *          the name of the style
   * @param symbolizer -
   *          the symbolizer to use
   * 
   * @return the new style
   */
  public static Style createStyle( String name, Symbolizer symbolizer )
  {
    return createStyle( name, symbolizer, 0, Double.MAX_VALUE );
  }

  /**
   * create a new style
   * 
   * @param name -
   *          the name of the style
   * @param symbolizer -
   *          the symbolizer to use
   * @param minScaleDenominator -
   *          the minimim scale to draw the feature at
   * @param maxScaleDenominator -
   *          the maximum scale to draw the feature at
   * 
   * @return the new style
   */
  public static Style createStyle( String name, Symbolizer symbolizer, double minScaleDenominator,
      double maxScaleDenominator )
  {
    // create the feature type style
    FeatureTypeStyle fts = createFeatureTypeStyle( name, symbolizer, minScaleDenominator,
        maxScaleDenominator );

    return createStyle( name, null, null, fts );
  }

  /**
   * create a style
   * 
   * @param name -
   *          the name of the style
   * @param featureTypeName -
   *          name of the feature type the Feature type style shall be assigned
   *          to
   * @param title -
   *          title of the FeatureTypeStyle
   * @param abstract_ -
   *          text describing the FeatureTypeStyle
   * @param rules -
   *          rules contained in the featureTypeStyle
   * 
   * @return the new style
   */
  public static Style createStyle( String name, String title, String abstract_,
      String featureTypeName, Rule[] rules )
  {

    FeatureTypeStyle fts = createFeatureTypeStyle( name, title, abstract_, featureTypeName, rules );
    return createStyle( name, null, null, fts );
  }

  /**
   * create a new style
   * 
   * @param name -
   *          the name of the style
   * @param title -
   *          title of the style
   * @param abstract_ -
   *          text describing the style
   * @param featureTypeStyle -
   *          featureTypeStyle
   * 
   * @return the new style
   */
  public static Style createStyle( String name, String title, String abstract_,
      FeatureTypeStyle featureTypeStyle )
  {
    return createStyle( name, title, abstract_, new FeatureTypeStyle[]
    { featureTypeStyle } );
  }

  /**
   * create a new style
   * 
   * @param name -
   *          the name of the style
   * @param title -
   *          title of the style
   * @param abstract_ -
   *          text describing the style
   * @param featureTypeStyles -
   *          featureTypeStyle
   * 
   * @return the new style
   */
  public static Style createStyle( String name, String title, String abstract_,
      FeatureTypeStyle[] featureTypeStyles )
  {
    return new UserStyle_Impl( name, title, abstract_, false, featureTypeStyles );
  }

  /**
   * creates a style with name 'defaultPoint' for rendering point geometries
   * 
   * @param wellKnownName
   *          the well known name of the mark
   * @param fillColor
   *          the color of the mark
   * @param borderColor
   *          the outline color of the mark
   * @param borderWidth
   *          the width of the outline
   * @param opacity -
   *          the opacity of the graphic
   * @param size -
   *          the size of the graphic
   * @param rotation -
   *          the rotation from the top of the page of the graphic
   * @param min -
   *          the minimim scale to draw the feature at
   * @param max -
   *          the maximum scale to draw the feature at
   * 
   * @return the style created
   */
  public static Style createPointStyle( String wellKnownName, Color fillColor, Color borderColor,
      double borderWidth, double opacity, double size, double rotation, double min, double max )
  {
    Mark mark = createMark( wellKnownName, fillColor, borderColor, borderWidth );
    Graphic graphic = createGraphic( null, mark, opacity, size, rotation );
    Symbolizer symbolizer = createPointSymbolizer( graphic, null, min, max );
    return createStyle( "defaultPoint", symbolizer );
  }

  /**
   * creates a style with name 'defaultLine' for rendering line geometries
   * 
   * @param color
   *          the line color
   * @param width
   *          the width of the line
   * @param opacity -
   *          the opacity of the line
   * @param min -
   *          the minimim scale to draw the feature at
   * @param max -
   *          the maximum scale to draw the feature at
   * 
   * @return the style created
   */
  public static Style createLineStyle( Color color, double width, double opacity, double min,
      double max )
  {
    Stroke stroke = createStroke( color, width, opacity );
    Symbolizer symbolizer = createLineSymbolizer( stroke, null, min, max );
    return createStyle( "defaultLine", symbolizer );
  }

  /**
   * creates a style with name 'defaultPolygon' for rendering polygon geometries
   * 
   * @param fillColor -
   *          the fill color of the polygon
   * @param fillOpacity -
   *          the fill opacity of the polygon
   * @param strokeColor -
   *          the line color
   * @param strokeWidth -
   *          the width of the line
   * @param strokeOpacity -
   *          the opacity of the line
   * @param min -
   *          the minimim scale to draw the feature at
   * @param max -
   *          the maximum scale to draw the feature at
   * 
   * @return the style created
   */
  public static Style createPolygonStyle( Color fillColor, double fillOpacity, Color strokeColor,
      double strokeWidth, double strokeOpacity, double min, double max )
  {
    Stroke stroke = createStroke( strokeColor, strokeWidth, strokeOpacity );
    Fill fill = createFill( fillColor, fillOpacity );
    Symbolizer symbolizer = createPolygonSymbolizer( stroke, fill, null, min, max );
    return createStyle( "defaultPolygon", symbolizer );
  }

  /**
   * creates a style with name 'defaultPoint' for rendering point geometries.
   * The style contains 1..n rules depending on the value range and the number
   * of steps within it. So it is possible to create a style that creates
   * different rendering depending on the value of one feature attribute.
   * <p>
   * there will be a linear interpolation between colors, size and width of the
   * first and the last rule considering the number of passed steps (rules)
   * 
   * @param wellKnownNames -
   *          list of well known names of the mark. the first field will be
   *          assigned to the starting rule the last to the ending rule.
   * @param startFillColor -
   *          the color of the mark of the first rule
   * @param endFillColor -
   *          the color of the mark of the last rule
   * @param startBorderColor -
   *          the outline color of the mark of the first rule
   * @param endBorderColor -
   *          the outline color of the mark of the last rule
   * @param startBorderWidth -
   *          the width of the outline of the first rule
   * @param endBorderWidth -
   *          the width of the outline of the last rule
   * @param opacity -
   *          the opacity of the graphic
   * @param startSize -
   *          the size of the graphic of the first rule
   * @param endSize -
   *          the size of the graphic of the last rule
   * @param rotation -
   *          the rotation from the top of the page of the graphic
   * @param min -
   *          the minimim scale to draw the feature at
   * @param max -
   *          the maximum scale to draw the feature at
   * @param featurePropertyName -
   *          name of the feature property that determines the selection of the
   *          rule for drawing
   * @param numberOfSteps -
   *          number of steps used for the interpolation between first and last
   *          value. It is identical with the number of rules that will be
   *          created.
   * 
   * @return the style created
   */
  public static Style createPointStyle( String[] wellKnownNames, Color startFillColor,
      Color endFillColor, Color startBorderColor, Color endBorderColor, double startBorderWidth,
      double endBorderWidth, double opacity, double startSize, double endSize, double rotation,
      double min, double max, String featurePropertyName, int numberOfSteps )
  {
    return null;
  }

  /**
   * creates a style with name 'defaultLine' for rendering line geometries. The
   * style contains 1..n rules depending on the value range and the number of
   * steps within it. So it is possible to create a style that creates different
   * rendering depending on the value of one feature attribute.
   * <p>
   * there will be a linear interpolation between colors, size and width of the
   * first and the last rule considering the number of passed steps (rules)
   * 
   * @param startColor -
   *          the color of the first rule
   * @param endColor -
   *          the color of the last rule
   * @param startWidth -
   *          the width of the line of the first rule
   * @param endWidth -
   *          the width of the line of the last rule
   * @param opacity -
   *          the opacity of the graphic
   * @param min -
   *          the minimim scale to draw the feature at
   * @param max -
   *          the maximum scale to draw the feature at
   * @param featurePropertyName -
   *          name of the feature property that determines the selection of the
   *          rule for drawing
   * @param numberOfSteps -
   *          number of steps used for the interpolation between first and last
   *          value. It is identical with the number of rules that will be
   *          created.
   * 
   * @return the style created
   */
  public static Style createLineStyle( Color startColor, Color endColor, double startWidth,
      double endWidth, double opacity, double min, double max, String featurePropertyName,
      int numberOfSteps )
  {
    return null;
  }

  /**
   * creates a style with name 'defaultPoint' for rendering point geometries.
   * The style contains 1..n rules depending on the value range and the number
   * of steps within it. So it is possible to create a style that creates
   * different rendering depending on the value of one feature attribute.
   * <p>
   * there will be a linear interpolation between colors, size and width of the
   * first and the last rule considering the number of passed steps (rules)
   * 
   * @param startFillColor -
   *          the fill color of the first rule
   * @param endFillColor -
   *          the fill color of the last rule
   * @param fillOpacity -
   *          the opacity of the fill
   * @param startStrokeColor -
   *          the line color of the first rule
   * @param endStrokeColor -
   *          the line color of the last rule
   * @param startStrokeWidth -
   *          the width of the outline of the first rule
   * @param endStrokeWidth -
   *          the width of the outline of the last rule
   * @param strokeOpacity -
   *          the opacity of the outline
   * @param min -
   *          the minimim scale to draw the feature at
   * @param max -
   *          the maximum scale to draw the feature at
   * @param featurePropertyName -
   *          name of the feature property that determines the selection of the
   *          rule for drawing
   * @param numberOfSteps -
   *          number of steps used for the interpolation between first and last
   *          value. It is identical with the number of rules that will be
   *          created.
   * 
   * @return the style created
   */
  public static Style createPolygonStyle( Color startFillColor, Color endFillColor,
      double fillOpacity, Color startStrokeColor, Color endStrokeColor, double startStrokeWidth,
      double endStrokeWidth, double strokeOpacity, double min, double max,
      String featurePropertyName, int numberOfSteps )
  {
    return null;
  }

  /**
   * create a new default style
   * 
   * @return the new style
   */
  public static Style createStyle()
  {
    return null;
  }

  /**
   * transforms the color of the request from java.awt.Color to the hexadecimal
   * representation as in an OGC conform WMS-GetMap request (e.g. white ==
   * "#ffffff").
   * 
   * @return the color as hexadecimal representation
   */
  public static String getColorAsHex( Color color )
  {
    String r = Integer.toHexString( color.getRed() );
    if( r.length() < 2 )
      r = "0" + r;
    String g = Integer.toHexString( color.getGreen() );
    if( g.length() < 2 )
      g = "0" + g;
    String b = Integer.toHexString( color.getBlue() );
    if( b.length() < 2 )
      b = "0" + b;
    return "#" + r + g + b;
  }
}