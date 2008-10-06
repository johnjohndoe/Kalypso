/*--------------- Kalypso-Header --------------------------------------------------------------------

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
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.sensor.tableview.rules;

import java.awt.Color;
import java.awt.Font;

import javax.swing.Icon;

/**
 * Stores information on how to render something. Its primary purpose is to be used within the context of table views.
 * <p>
 * Wrapper-class over the jaxb-generated class <code>TypeRenderingRule</code>
 * 
 * @author schlienger
 */
public class RenderingRule
{
  private final Color m_fg;

  private final Color m_bg;

  private final String m_tt;

  private final int m_mask;

  private final Font m_ft;

  private final Icon m_icon;

  public RenderingRule( final int mask, final Color fg, final Color bg, final Font font, final String tt, final Icon icon )
  {
    m_mask = mask;
    m_fg = fg;
    m_bg = bg;
    m_ft = font;
    m_tt = tt;
    m_icon = icon;
  }

  public Color getForegroundColor( )
  {
    return m_fg;
  }

  public Color getBackgroundColor( )
  {
    return m_bg;
  }

  public String getTooltipText( )
  {
    return m_tt;
  }

  public Font getFont( )
  {
    return m_ft;
  }

  public int getMask( )
  {
    return m_mask;
  }

  public Icon getIcon( )
  {
    return m_icon;
  }

  /**
   * Tests if the given mask is part of this.
   * 
   * @param mask
   *          some BitMask to test
   * @return true when mask is part of this
   */
  public boolean contains( final int mask )
  {
    if( m_mask == 0 || mask == 0 )
      return mask == m_mask;

    return (m_mask & mask) == mask;
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    return m_mask + " - " + m_tt == null ? "" : m_tt; //$NON-NLS-1$ //$NON-NLS-2$
  }

  /**
   * Create a deep copy of this rule.
   */
  public RenderingRule cloneRule()
  {
    return new RenderingRule( m_mask, m_fg, m_bg, m_ft, m_tt, m_icon );
  }
}