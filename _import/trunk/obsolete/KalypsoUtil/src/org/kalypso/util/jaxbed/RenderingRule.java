package org.kalypso.util.jaxbed;

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
  private final TypeRenderingRule m_rr;

  public RenderingRule( final TypeRenderingRule rr )
  {
    m_rr = rr;
  }

  public Color getForegroundColor()
  {
    return StringUtilities.stringToColor( m_rr.getForegroundcolor() );
  }

  public Color getBackgroundColor()
  {
    return StringUtilities.stringToColor( m_rr.getBackgroundcolor() );
  }

  public String getTooltipText()
  {
    return m_rr.getTooltip();
  }

  public Font getFont()
  {
    return StringUtilities.stringToFont( m_rr.getFont() );
  }
  
  public int getMask()
  {
    // TODO take mask from xml
    return 0;//m_rr.
  }
}