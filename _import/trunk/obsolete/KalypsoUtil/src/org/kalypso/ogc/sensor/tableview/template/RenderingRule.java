package org.kalypso.ogc.sensor.tableview.template;

import java.awt.Color;
import java.awt.Font;

import org.kalypso.java.util.StringUtilities;
import org.kalypso.template.obstableview.TypeRenderingRule;

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

  public RenderingRule( final int mask, final Color fg, final Color bg, final Font font,
      final String tt )
  {
    m_mask = mask;
    m_fg = fg;
    m_bg = bg;
    m_ft = font;
    m_tt = tt;
  }

  public static RenderingRule createRenderingRule( final TypeRenderingRule rr )
  {
    int mask = rr.getMask();
    String fg = rr.getForegroundcolor();
    String bg = rr.getBackgroundcolor();
    String font = rr.getFont();
    String tt = rr.getTooltip();

    return new RenderingRule( mask, fg == null ? null : StringUtilities.stringToColor( fg ),
        bg == null ? null : StringUtilities.stringToColor( bg ), font == null ? null
            : StringUtilities.stringToFont( font ), tt );
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

  /**
   * Tests if the given mask is part of this.
   * 
   * @param mask
   *          some BitMask to test
   */
  public boolean contains( final int mask )
  {
    return ( m_mask & mask ) == mask;
  }
}