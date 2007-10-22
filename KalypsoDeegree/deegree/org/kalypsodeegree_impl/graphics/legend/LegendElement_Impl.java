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
package org.kalypsodeegree_impl.graphics.legend;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.util.ArrayList;

import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.graphics.displayelements.PolygonDisplayElement;
import org.kalypsodeegree.graphics.legend.LegendElement;
import org.kalypsodeegree.graphics.legend.LegendException;
import org.kalypsodeegree.graphics.sld.LineSymbolizer;
import org.kalypsodeegree.graphics.sld.PointSymbolizer;
import org.kalypsodeegree.graphics.sld.PolygonSymbolizer;
import org.kalypsodeegree.graphics.sld.RasterSymbolizer;
import org.kalypsodeegree.graphics.sld.Rule;
import org.kalypsodeegree.graphics.sld.Symbolizer;
import org.kalypsodeegree.graphics.sld.TextSymbolizer;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.graphics.displayelements.DisplayElementFactory;
import org.kalypsodeegree_impl.graphics.displayelements.PolygonDisplayElement_Impl;
import org.kalypsodeegree_impl.graphics.sld.Symbolizer_Impl.UOM;
import org.kalypsodeegree_impl.graphics.transformation.WorldToScreenTransform;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.tools.Debug;

/**
 * The implements the basic legend element. a legend element may has a label that can be set to eight positions relative
 * to the legend graphic. A <tt>LegendElement</tt> can be activated or deactivated. It depends on the using
 * application what effect this behavior will have.
 * <p>
 * <tt>LegendElement</tt> s can be collected in a <tt>LegendElementCollection</tt> which also is a
 * <tt>LegendElement</tt> to group elements or to create more complex elements.
 * <p>
 * Each <tt>LegendElement</tt> is able to paint itself as <tt>BufferedImage</tt>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public class LegendElement_Impl implements LegendElement
{
  protected ArrayList m_ruleslist = null;

  protected String m_label = "";

  protected double m_orientation = 0;

  protected int m_labelPosition = -1;

  protected boolean m_active = false;

  protected int m_width = 0;

  protected int m_height = 0;

  protected int m_bufferBetweenLegendAndLabel = 10;

  protected BufferedImage m_bi = null;

  /**
   * empty constructor
   */
  LegendElement_Impl( )
  {
    this.m_ruleslist = new ArrayList();
  }

  /**
   *
   *
   */
  LegendElement_Impl( final BufferedImage legendImage )
  {
    this();
    m_bi = legendImage;
  }

  /**
   * constructor
   * 
   * @param rules
   *            the different rules from the SLD
   * @param label
   *            the label beneath the legend symbol
   * @param orientation
   *            the rotation of the text in the legend
   * @param labelPosition
   *            the position of the text according to the symbol
   * @param active
   *            whether the legendsymbol is active or not
   * @param width
   *            the requested width of the legend symbol
   * @param height
   *            the requested height of the legend symbol
   */
  LegendElement_Impl( final Rule[] rules, final String label, final double orientation, final int labelPosition, final boolean active, final int width, final int height )
  {
    this();
    setRules( rules );
    setLabel( label );
    setLabelOrientation( orientation );
    setLabelPlacement( labelPosition );
    setActive( active );
    setWidth( width );
    setHeight( height );
  }

  /**
   * gets the Rules as an array
   * 
   * @return array of sld rules
   */
  public Rule[] getRules( )
  {
    if( m_ruleslist != null && m_ruleslist.size() > 0 )
    {
      return (Rule[]) m_ruleslist.toArray( new Rule[m_ruleslist.size()] );
    }

    return null;
  }

  /**
   * adds a rule to the ArrayList ruleslist
   * 
   * @param rule
   *            a sld rule
   */
  public void addRule( final Rule rule )
  {
    this.m_ruleslist.add( rule );
  }

  /**
   * sets the rules
   * 
   * @param rules
   *            an array of sld rules
   */
  public void setRules( final Rule[] rules )
  {
    this.m_ruleslist.clear();

    if( rules != null )
    {
      for( final Rule element : rules )
      {
        this.m_ruleslist.add( element );
      }
    }
  }

  /**
   * sets the label of the <tt>LegendElement</tt>
   * 
   * @param label
   *            label of the <tt>LegendElement</tt>
   */
  public void setLabel( final String label )
  {
    this.m_label = label;
  }

  /**
   * returns the label set to <tt>LegendElement</tt>. If no label is set, the method returns <tt>null</tt>
   * 
   * @return label of the <tt>LegendElement</tt> or <tt>null</tt>
   */
  public String getLabel( )
  {
    return this.m_label;
  }

  /**
   * sets the orientation of the label of the <tt>LegendElement</tt>. A label can have an orientation from -90° to
   * 90° expressed in radians, where 0° is horizontal
   * 
   * @param orientation
   */
  public void setLabelOrientation( final double orientation )
  {
    this.m_orientation = orientation;
  }

  /**
   * returns the current orientation of the label of the <tt>LegendElement</tt> in radians. If the element hasn't a
   * label <tt>Double.NEGATIVE_INFINITY</tt> will be returned.
   * 
   * @return orientation of the label of the <tt>LegendElement</tt> in radians
   */
  public double getLabelOrientation( )
  {
    return this.m_orientation;
  }

  /**
   * sets the placement of the label relative to the legend symbol. Possible values are:
   * <ul>
   * <li>LP_TOPCENTER
   * <li>LP_TOPLEFT
   * <li>LP_TOPRIGHT
   * <li>LP_RIGHT
   * <li>LP_LEFT
   * <li>LP_BOTTOMCENTER
   * <li>LP_BOTTOMRIGHT
   * <li>LP_BOTTOMLEFT
   * </ul>
   * 
   * <pre>
   * +---+---+---+
   * | 1 | 0 | 2 |
   * +---+---+---+
   * | 4 |LEG| 3 |
   * +---+---+---+
   * | 7 | 5 | 6 |
   * +---+---+---+
   * </pre>
   * 
   * An implementation of the interface may not supoort all positions.
   * 
   * @param labelPosition
   */
  public void setLabelPlacement( final int labelPosition )
  {
    this.m_labelPosition = labelPosition;
  }

  /**
   * returns the placement of the label relative to the legend symbol. If the element hasn't a label
   * <tt>LegendElement.LP_NOLABEL</tt> will be returned. Otherwise possible values are:
   * <ul>
   * <li>LP_TOPCENTER
   * <li>LP_TOPLEFT
   * <li>LP_TOPRIGHT
   * <li>LP_RIGHT
   * <li>LP_LEFT
   * <li>LP_BOTTOMCENTER
   * <li>LP_BOTTOMRIGHT
   * <li>LP_BOTTOMLEFT
   * </ul>
   * 
   * @return coded placement of the label relative to the legend symbol
   */
  public int getLabelPlacement( )
  {
    return this.m_labelPosition;
  }

  /**
   * activates or deactivates the label
   * 
   * @param active
   */
  public void setActive( final boolean active )
  {
    this.m_active = active;
  }

  /**
   * gets the activtion-status of the label
   */
  public boolean isActive( )
  {
    return this.m_active;
  }

  /**
   * sets the width of the LegendSymbol (in pixels)
   */
  public void setWidth( final int width )
  {
    this.m_width = width;
  }

  /**
   * gets the width of the LegendSymbol (in pixels)
   */
  public int getWidth( )
  {
    return this.m_width;
  }

  /**
   * sets the height of the LegendSymbol (in pixels)
   */
  public void setHeight( final int height )
  {
    this.m_height = height;
  }

  /**
   * gets the height of the LegendSymbol (in pixels)
   */
  public int getHeight( )
  {
    return this.m_height;
  }

  /**
   * returns the buffer place between the legend symbol and the legend label in pixels
   * 
   * @return the buffer as integer in pixels
   */
  public int getBufferBetweenLegendAndLabel( )
  {
    return this.m_bufferBetweenLegendAndLabel;
  }

  /**
   * @see org.kalypsodeegree_impl.graphics.legend.LegendElement_Impl#getBufferBetweenLegendAndLabel()
   * @param i
   *            the buffer as integer in pixels
   */
  public void setBufferBetweenLegendAndLabel( final int i )
  {
    this.m_bufferBetweenLegendAndLabel = i;
  }

  /**
   * draws a legendsymbol, if the SLD defines a point
   * 
   * @param g
   *            the graphics context
   * @param c
   *            the PointSymbolizer representing the drawable point
   * @param width
   *            the requested width of the symbol
   * @param height
   *            the requested height of the symbol
   * @throws LegendException
   *             is thrown, if the parsing of the sld failes.
   */
  protected void drawPointLegend( final Graphics g, final PointSymbolizer c, final int width, final int height ) throws LegendException
  {
    Debug.debugMethodBegin( "LegendElement_Impl", "drawPointLegend()" );
    final org.kalypsodeegree.graphics.sld.Graphic deegreegraphic = c.getGraphic();
    try
    {
      final BufferedImage buffi = ((org.kalypsodeegree_impl.graphics.sld.Graphic_Impl) deegreegraphic).getAsImage( null, UOM.pixel, null );
      final int w = buffi.getWidth();
      final int h = buffi.getHeight();
      g.drawImage( buffi, width / 2 - w / 2, height / 2 - h / 2, null );
    }
    catch( final FilterEvaluationException feex )
    {
      throw new LegendException( "FilterEvaluationException occured during the creation of the legend:\n" + "The legend for the PointSymbol can't be processed.\n" + feex.getMessage() );
    }
    Debug.debugMethodEnd();
  }

  /**
   * draws a legendsymbol, if the SLD defines a line
   * 
   * @param g
   *            the graphics context
   * @param ls
   *            the LineSymbolizer representing the drawable line
   * @param width
   *            the requested width of the symbol
   * @param height
   *            the requested height of the symbol
   * @throws LegendException
   *             is thrown, if the parsing of the sld failes.
   */
  protected void drawLineStringLegend( final Graphics2D g, final LineSymbolizer ls, final int width, final int height ) throws LegendException
  {
    Debug.debugMethodBegin();

    final org.kalypsodeegree.graphics.sld.Stroke sldstroke = ls.getStroke();
    try
    {
      // color, opacity
      setColor( g, sldstroke.getStroke( null ), sldstroke.getOpacity( null ) );
      g.setStroke( getBasicStroke( sldstroke ) );
    }
    catch( final FilterEvaluationException feex )
    {
      throw new LegendException( "FilterEvaluationException occured during the creation of the legend:\n" + "The legend for the LineSymbol can't be processed.\n" + feex.getMessage() );
    }

    // p1 = [0 | height]
    // p2 = [width / 3 | height / 3]
    // p3 = [width - width / 3 | height - height / 3]
    // p4 = [width | 0]
    final int[] xPoints = { 0, width / 3, width - width / 3, width };
    final int[] yPoints = { height, height / 3, height - height / 3, 0 };
    final int nPoints = 4;

    g.drawPolyline( xPoints, yPoints, nPoints );
    Debug.debugMethodEnd();
  }

  /**
   * draws a legendsymbol, if the SLD defines a polygon
   * 
   * @param g
   *            the graphics context
   * @param ps
   *            the PolygonSymbolizer representing the drawable polygon
   * @param width
   *            the requested width of the symbol
   * @param height
   *            the requested height of the symbol
   * @throws LegendException
   *             if the parsing of the sld failes.
   */
  protected void drawPolygonLegend( final Graphics2D g, final PolygonSymbolizer ps, final int width, final int height ) throws LegendException
  {
    Debug.debugMethodBegin();

    final GM_Position p1 = GeometryFactory.createGM_Position( 0, 0 );
    final GM_Position p2 = GeometryFactory.createGM_Position( 0, height - 1 );
    final GM_Position p3 = GeometryFactory.createGM_Position( width, height - 1 );
    final GM_Position p4 = GeometryFactory.createGM_Position( width, 0 );

    final GM_Position[] pos = { p1, p2, p3, p4, p1 };
    GM_Surface surface = null;
    try
    {
      surface = GeometryFactory.createGM_Surface( pos, null, null, null );
    }
    catch( final Exception ex )
    {
      throw new LegendException( "Exception occured during the creation of the legend:\n" + "The legendsymbol for the Polygon can't be processed.\n"
          + "Error in creating the Surface from the Positions.\n" + ex.getMessage() );
    }

    PolygonDisplayElement pde = null;
    pde = DisplayElementFactory.buildPolygonDisplayElement( null, surface, ps );
    if( pde == null )
      throw new LegendException( "IncompatibleGeometryTypeException occured during the creation of the legend:\n" + "The legendsymbol for the Polygon can't be processed.\n"
          + "Error in creating the PolygonDisplayElement.\n" );

    final GM_Envelope envelope = GeometryFactory.createGM_Envelope( p1, p3 );

    final WorldToScreenTransform wtst = new WorldToScreenTransform( envelope, envelope );
    ((PolygonDisplayElement_Impl) pde).paint( g, wtst, new NullProgressMonitor() );
    Debug.debugMethodEnd();
  }

  /**
   * sets the color including an opacity
   * 
   * @param g2
   *            the graphics contect as Graphics2D
   * @param color
   *            the requested color of the legend symbol
   * @param opacity
   *            the requested opacity of the legend symbol
   * @return the Graphics2D object containing color and opacity
   */
  private Graphics2D setColor( final Graphics2D g2, Color color, final double opacity )
  {
    Debug.debugMethodBegin();
    if( opacity < 0.999 )
    {
      final int alpha = (int) Math.round( opacity * 255 );
      final int red = color.getRed();
      final int green = color.getGreen();
      final int blue = color.getBlue();
      color = new Color( red, green, blue, alpha );
    }
    g2.setColor( color );
    Debug.debugMethodEnd();
    return g2;
  }

  /**
   * constructs a java.awt.BasicStroke for painting a LineString legend symbol.
   * 
   * @param sldstroke
   *            the deegree sld stroke
   * @return a java.awt.BasicStroke
   * @throws LegendException
   *             if the sld cannot be processed
   */
  private BasicStroke getBasicStroke( final org.kalypsodeegree.graphics.sld.Stroke sldstroke ) throws LegendException
  {
    Debug.debugMethodBegin();
    BasicStroke bs = null;
    try
    {
      final float width = (float) sldstroke.getWidth( null );
      final int cap = sldstroke.getLineCap( null );
      final int join = sldstroke.getLineJoin( null );
      final float miterlimit = 1f;
      final float[] dash = sldstroke.getDashArray( null );
      final float dash_phase = sldstroke.getDashOffset( null );

      bs = new BasicStroke( width, cap, join, miterlimit, dash, dash_phase );
      // return new BasicStroke((float)sldstroke.getWidth(null),
      // sldstroke.getLineCap(null), sldstroke.getLineJoin(null), 1f,
      // sldstroke.getDashArray(null), sldstroke.getDashOffset(null));

    }
    catch( final FilterEvaluationException ex )
    {
      throw new LegendException( "FilterEvaluationException occured during the creation of the legend:\n" + "The Stroke of the element can't be processed.\n" + ex.getMessage() );
    }
    Debug.debugMethodEnd();
    return bs;
  }

  /**
   * calculates the FontMetrics of the LegendLabel in pixels. It returns an 3-dimensional array containing [0] the
   * width, [1] the ascent and [2] the descent.
   * 
   * @param label
   *            the label of the LegendElement
   * @return the 3-dimensional INT-Array contains [0] the width of the string, [1] the ascent and [2] the descent.
   */
  protected int[] calculateFontMetrics( final String label )
  {
    Debug.debugMethodBegin( "LegendElement_Impl", "calculateFontMetrics" );
    final int[] fontmetrics = new int[3];

    final BufferedImage bi = new BufferedImage( 1, 1, BufferedImage.TYPE_INT_ARGB );
    final Graphics g = bi.getGraphics();

    final FontMetrics fm = g.getFontMetrics();

    if( label != null && label.length() > 0 )
    {
      fontmetrics[0] = fm.stringWidth( label );
      fontmetrics[1] = fm.getAscent();
      fontmetrics[2] = fm.getDescent();
    }
    else
    {
      fontmetrics[0] = 0;
      fontmetrics[1] = 0;
      fontmetrics[2] = 0;
      // value = 1, because of a bug, which doesn't paint the right border of
      // the polygon-symbol.
      setBufferBetweenLegendAndLabel( 1 );
    }
    g.dispose();

    Debug.debugMethodEnd();
    return fontmetrics;
  }

  /**
   * calculates the width and height of the resulting LegendSymbol depending on the LabelPlacement
   */
  private BufferedImage calculateImage( final int labelposition, final int labelwidth, final int ascent, final int descent, final int legendwidth, final int legendheight, final int buffer )
  {
    Debug.debugMethodBegin();
    // TODO labelposition
    switch( labelposition )
    {
      // LP_TOPCENTER
      case 0:
      {
        System.out.println( "The text-position LP_TOPCENTER in the legend is not implemented yet.\n " + "We put the text on the right side (EAST) of the legendsymbol." );
        final BufferedImage bi = new BufferedImage( (legendwidth + buffer + labelwidth), legendheight, BufferedImage.TYPE_INT_ARGB );
        final Graphics g = bi.getGraphics();
        g.setColor( Color.BLACK );
        g.drawString( getLabel(), m_width + 10, m_height / 2 + ((ascent - descent) / 2) );
        Debug.debugMethodEnd();
        return bi;
      }
        // LP_TOPLEFT
      case 1:
      {
        System.out.println( "The text-position LP_TOPLEFT in the legend is not implemented yet.\n" + "We put the text on the right side (EAST) of the legendsymbol." );
        final BufferedImage bi = new BufferedImage( (legendwidth + buffer + labelwidth), legendheight, BufferedImage.TYPE_INT_ARGB );
        final Graphics g = bi.getGraphics();
        g.setColor( Color.BLACK );
        g.drawString( getLabel(), m_width + 10, m_height / 2 + ((ascent - descent) / 2) );
        Debug.debugMethodEnd();
        return bi;
      }
        // LP_TOPRIGHT
      case 2:
      {
        System.out.println( "The text-position LP_TOPRIGHT in the legend is not implemented yet.\n We put the text on the right side (EAST) of the legendsymbol." );
        final BufferedImage bi = new BufferedImage( (legendwidth + buffer + labelwidth), legendheight, BufferedImage.TYPE_INT_ARGB );
        final Graphics g = bi.getGraphics();
        g.setColor( Color.BLACK );
        g.drawString( getLabel(), m_width + 10, m_height / 2 + ((ascent - descent) / 2) );
        Debug.debugMethodEnd();
        return bi;
      }
        // LP_RIGHT
      case 3:
      {
        System.out.println( "The text-position LP_RIGHT in the legend is not implemented yet.\n We put the text on the right side (EAST) of the legendsymbol." );
        final BufferedImage bi = new BufferedImage( (legendwidth + buffer + labelwidth), legendheight, BufferedImage.TYPE_INT_ARGB );
        final Graphics g = bi.getGraphics();
        g.setColor( Color.BLACK );
        g.drawString( getLabel(), m_width + 10, m_height / 2 + ((ascent - descent) / 2) );
        Debug.debugMethodEnd();
        return bi;

      }
        // LP_LEFT
      case 4:
      {
        final BufferedImage bi = new BufferedImage( (legendwidth + buffer + labelwidth), legendheight, BufferedImage.TYPE_INT_ARGB );
        final Graphics g = bi.getGraphics();
        g.setColor( Color.BLACK );
        g.drawString( getLabel(), m_width + 10, m_height / 2 + ((ascent - descent) / 2) );
        Debug.debugMethodEnd();
        return bi;
      }
        // LP_BOTTOMCENTER
      case 5:
      {
        System.out.println( "The text-position LP_BOTTOMCENTER in the legend is not implemented yet.\n We put the text on the right side (EAST) of the legendsymbol." );
        final BufferedImage bi = new BufferedImage( (legendwidth + buffer + labelwidth), legendheight, BufferedImage.TYPE_INT_ARGB );
        final Graphics g = bi.getGraphics();
        g.setColor( Color.BLACK );
        g.drawString( getLabel(), m_width + 10, m_height / 2 + ((ascent - descent) / 2) );
        Debug.debugMethodEnd();
        return bi;
      }
        // LP_BOTTOMRIGHT
      case 6:
      {
        System.out.println( "The text-position LP_BOTTOMRIGHT in the legend is not implemented yet.\n We put the text on the right side (EAST) of the legendsymbol." );
        final BufferedImage bi = new BufferedImage( (legendwidth + buffer + labelwidth), legendheight, BufferedImage.TYPE_INT_ARGB );
        final Graphics g = bi.getGraphics();
        g.setColor( Color.BLACK );
        g.drawString( getLabel(), m_width + 10, m_height / 2 + ((ascent - descent) / 2) );
        Debug.debugMethodEnd();
        return bi;
      }
        // LP_BOTTOMLEFT
      case 7:
      {
        System.out.println( "The text-position LP_BOTTOMLEFT in the legend is not implemented yet.\n We put the text on the right side (EAST) of the legendsymbol." );
        final BufferedImage bi = new BufferedImage( (legendwidth + buffer + labelwidth), legendheight, BufferedImage.TYPE_INT_ARGB );
        final Graphics g = bi.getGraphics();
        g.setColor( Color.BLACK );
        g.drawString( getLabel(), m_width + 10, m_height / 2 + ((ascent - descent) / 2) );
        Debug.debugMethodEnd();
        return bi;
      }
        // LP_NOLABEL
      default:
      {
        Debug.debugMethodEnd();
        return new BufferedImage( legendwidth, legendheight, BufferedImage.TYPE_INT_ARGB );
      }
    }
  }

  /**
   * exports the <tt>LegendElement</tt> as</tt> BufferedImage</tt>
   */
  public BufferedImage exportAsImage( ) throws LegendException
  {
    Debug.debugMethodBegin( "LegendElement_Impl", "exportAsImage" );
    if( m_bi == null )
    {
      int[] fontmetrics;

      Graphics g = null;

      // calculates the fontmetrics and creates the bufferedimage
      // if getLabel() is null is checked in calculateFontMetrics!
      fontmetrics = calculateFontMetrics( getLabel() );
      m_bi = calculateImage( getLabelPlacement(), fontmetrics[0], fontmetrics[1], fontmetrics[2], getWidth(), getHeight(), getBufferBetweenLegendAndLabel() );
      g = m_bi.getGraphics();
      g.setColor( Color.WHITE );
      final Rule[] myrules = getRules();
      Symbolizer[] symbolizer = null;

      // determines the legendsymbol and paints it
      for( final Rule element : myrules )
      {
        symbolizer = element.getSymbolizers();

        for( final Symbolizer element2 : symbolizer )
        {
          if( element2 instanceof PointSymbolizer )
          {
            drawPointLegend( g, (PointSymbolizer) element2, getWidth(), getHeight() );
          }
          if( element2 instanceof LineSymbolizer )
          {
            drawLineStringLegend( (Graphics2D) g, (LineSymbolizer) element2, m_width, m_height );
          }
          if( element2 instanceof PolygonSymbolizer )
          {
            drawPolygonLegend( (Graphics2D) g, (PolygonSymbolizer) element2, m_width, m_height );
          }
          if( element2 instanceof RasterSymbolizer )
          {
            // throw new LegendException("RasterSymbolizer is not implemented
            // yet!");
          }
          if( element2 instanceof TextSymbolizer )
          {
            // throw new LegendException("TextSymbolizer is not implemented
            // yet!");
          }
        }

        // g.setColor(Color.black);
        // g.drawString(getLabel(), width + 10, height / 2 + ((fontmetrics[1] -
        // fontmetrics[2]) / 2));

      }
    }
    Debug.debugMethodEnd();
    return m_bi;
  }
}

/***********************************************************************************************************************
 * **************************************************************************** Changes to this class. What the people
 * have been up to: $Log$
 * have been up to: Revision 1.15  2007/10/22 09:22:56  devgernot
 * have been up to: Kalypso-Grid support refactored and improved.
 * have been up to: have been up to: Revision 1.14 2007/07/04 21:17:20 devgernot have
 * been up to: Result map generation. have been up to: have been up to: Bugfix: default style not working any more due
 * to handling of gml:location. have been up to: have been up to: Revision 1.13 2007/05/03 07:07:47 devgernot have been
 * up to: SLD supports now SE 1.1.0 ish uom attribute for Symbolzers. have been up to: have been up to: Revision 1.12
 * 2005/06/29 10:41:17 belger have been up to: *** empty log message *** have been up to: have been up to: Revision 1.11
 * 2005/06/20 14:07:49 belger have been up to: Formatierung have been up to: Revision 1.12 2004/07/09 07:17:19 poth no
 * message Revision 1.11 2004/06/01 15:55:05 poth no message Revision 1.10 2004/04/07 10:58:46 axel_schaefer bugfix
 **********************************************************************************************************************/
