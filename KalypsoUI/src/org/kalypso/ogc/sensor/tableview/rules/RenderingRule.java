package org.kalypso.ogc.sensor.tableview.rules;

import java.awt.Color;
import java.awt.Font;

import javax.swing.Icon;

/**
 * Stores information on how to render something. Its primary purpose is to be
 * used within the context of table views.
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

  public RenderingRule( final int mask, final Color fg, final Color bg, final Font font,
      final String tt, final Icon icon )
  {
    m_mask = mask;
    m_fg = fg;
    m_bg = bg;
    m_ft = font;
    m_tt = tt;
    m_icon = icon;
  }

  public Color getForegroundColor()
  {
    return m_fg;
  }

  public Color getBackgroundColor()
  {
    return m_bg;
  }

  public String getTooltipText()
  {
    return m_tt;
  }

  public Font getFont()
  {
    return m_ft;
  }

  public int getMask()
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
    return ( m_mask & mask ) == mask;
  }
}