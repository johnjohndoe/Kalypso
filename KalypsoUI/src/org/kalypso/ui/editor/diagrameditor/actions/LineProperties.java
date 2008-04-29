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
  /** default: color black, size 1, dash null. */
  private static LineProperties m_default = new LineProperties( Color.BLACK );

  /** separator used between members in string representation */
  public static final String MEMBER_SEPARATOR = " ";

  /** string used to separate the dash elements */
  public static final String DASH_SEPARATOR = ",";

  /** the color of the line */
  private final Color m_color;

  /** the size of the line */
  private final float m_size;

  /** The dash of the line. */
  private float[] m_dash = null;

  public LineProperties( Color c )
  {
    this( c, 1, null );
  }

  public LineProperties( Color c, float size )
  {
    this( c, size, null );
  }

  public LineProperties( Color c, float size, float[] dash )
  {
    m_color = c;
    m_size = size;
    m_dash = dash;
  }

  public Color getColor()
  {
    return m_color;
  }

  public float getSize()
  {
    return m_size;
  }

  public float[] getDash()
  {
    return m_dash;
  }

  public Stroke getStroke()
  {
    return new BasicStroke( m_size, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER, 10.0f, m_dash, 0.0f );
  }


  /**
   * Factory method that returns the default LineProperties object.
   */
  public static LineProperties getDefault()
  {
    return m_default;
  }
}
