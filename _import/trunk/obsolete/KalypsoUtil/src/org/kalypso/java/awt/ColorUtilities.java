package org.kalypso.java.awt;

import java.awt.Color;


/**
 * Some useful method dealing with java.awt.Color
 *
 * @author schlienger
 */
public class ColorUtilities
{
  /**
   * creates a returns the complementary Color of org. Each component of the color is substracted
   * to 255 and set as component of the new color.
   *
   * @param org
   *
   * @return new Color
   */
  public static Color createComplementary( final Color org )
  {
    final Color cmp = new Color( 255 - org.getRed(  ), 255 - org.getGreen(  ), 255 - org.getBlue(  ) );

    return cmp;
  }

  /**
   * @return a new Color which components have been given by Math.random().
   */
  public static Color random(  )
  {
    return new Color( (float)Math.random(  ), (float)Math.random(  ), (float)Math.random(  ) );
  }
}
