/*--------------- Kalypso-Header ------------------------------------------

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

 --------------------------------------------------------------------------*/

package org.kalypso.ui.editor.diagrameditor.actions;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Stroke;

/**
 * Stores the properties of a line
 * 
 * @author schlienger
 */
public final class LineProperties
{
  /** A color defining the undefined state. */
  public static final Color COLOR_UNDEF = new Color( 0, 0, 0 );

  /** A size defining the undefined state. */
  public static final Integer SIZE_UNDEF = new Integer( -999 );

  /** A dash defining the undefined state. */
  public static final DashType DASH_UNDEF = new DashType( "", "", null );

  /** A stroke defining the undefined state. */
  public static final Stroke STROKE_UNDEF = new BasicStroke();

  /** Some name of this line */
  private final String m_name;

  /** the color of the line */
  private final Color m_color;

  /** the size of the line */
  private final Integer m_size;

  /** The dash of the line. */
  private DashType m_dash = null;

  public LineProperties( final String name, final Color c )
  {
    this( name, c, new Integer( 1 ), null );
  }

  public LineProperties( final String name, final Color c, final Integer size )
  {
    this( name, c, size, null );
  }

  public LineProperties( final String name, final Color c, final Integer size, final DashType dash )
  {
    m_name = name;
    m_color = c;
    m_size = size;
    m_dash = dash;
  }

  public String getName()
  {
    return m_name;
  }

  public Color getColor()
  {
    return m_color;
  }

  public Integer getSize()
  {
    return m_size;
  }

  public DashType getDash()
  {
    return m_dash;
  }

  public Stroke getStroke()
  {
    if( m_size == LineProperties.SIZE_UNDEF && m_dash == LineProperties.DASH_UNDEF )
      return STROKE_UNDEF;

    final int size = m_size == LineProperties.SIZE_UNDEF ? 1 : m_size.intValue();
    if( m_dash == LineProperties.DASH_UNDEF || m_dash == DashType.NONE )
      return new BasicStroke( size, BasicStroke.CAP_ROUND, BasicStroke.JOIN_BEVEL );

    return new BasicStroke( size, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER, 1.0f, m_dash.getDashs(), 10.0f );
  }

  /**
   * Merge different line properties into one. Sets undefined constants for properties that differ.
   */
  public static LineProperties mergeProperties( final LineProperties[] items )
  {
    String name = null;
    Color color = null;
    Integer size = null;
    DashType dash = null;

    // Check 0-length, else we get a NPE later
    if( items.length == 0 )
      return null;

    for( int i = 0; i < items.length; i++ )
    {
      final LineProperties properties = items[i];

      final String curveName = properties.getName();
      final Color curveColor = properties.getColor();
      final Integer curveSize = properties.getSize();
      final DashType curveDash = properties.getDash();

      if( name == null )
        name = curveName;
      else if( name != EditDiagCurveDialog.NAME_UNDEF )
      {
        if( !name.equals( curveName ) )
          name = EditDiagCurveDialog.NAME_UNDEF;
      }

      if( color == null )
        color = curveColor;
      else if( color != LineProperties.COLOR_UNDEF )
      {
        if( !color.equals( curveColor ) )
          color = LineProperties.COLOR_UNDEF;
      }

      if( size == null )
        size = curveSize;
      else if( size != LineProperties.SIZE_UNDEF )
      {
        if( !size.equals( curveSize ) )
          size = LineProperties.SIZE_UNDEF;
      }

      if( dash == null )
        dash = curveDash;
      else if( dash != LineProperties.DASH_UNDEF )
      {
        if( !dash.equals( curveDash ) )
          dash = LineProperties.DASH_UNDEF;
      }
    }

    if( dash == null )
      dash = DashType.NONE;

    return new LineProperties( name, color, size, dash );
  }
}
