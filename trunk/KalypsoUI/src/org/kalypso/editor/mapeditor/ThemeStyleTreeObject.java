/*
 * Created on 22.07.2004
 * 
 * TODO To change the template for this generated file go to Window -
 * Preferences - Java - Code Style - Code Templates
 */
package org.kalypso.editor.mapeditor;

import org.deegree.graphics.sld.UserStyle;
import org.kalypso.ogc.gml.KalypsoFeatureTheme;
import org.kalypso.ogc.gml.KalypsoUserStyle;

public class ThemeStyleTreeObject
{
  private final KalypsoUserStyle myStyle;

  private final KalypsoFeatureTheme myTheme;

  public ThemeStyleTreeObject( KalypsoFeatureTheme theme, UserStyle style )
  {
    myTheme = theme;
    myStyle = (KalypsoUserStyle)style;
  }

  /**
   *  
   */
  public ThemeStyleTreeObject( KalypsoFeatureTheme theme, KalypsoUserStyle style )
  {
    myTheme = theme;
    myStyle = style;
  }

  public KalypsoUserStyle getStyle()
  {
    return myStyle;
  }

  public KalypsoFeatureTheme getTheme()
  {
    return myTheme;
  }

  public String toString()
  {
    if( myStyle == null )
      return "<no styles set>";
    
    if( myStyle.getName() != null )
      return myStyle.getName();
    return myStyle.toString();
  }
}