package org.kalypso.eclipse.swt.graphics;

import org.eclipse.swt.graphics.Device;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;

/**
 * @author belger
 */
public class FontUtilities
{
  private FontUtilities()
  {
    // do not instantiaite this class
  }

  public static Font createChangedFontData( final FontData[] fontData, final int heightOffset, final int styleOffset, final Device device )
  {
    for( int i = 0; i < fontData.length; i++ )
    {
      fontData[i].setHeight( fontData[i].getHeight() + heightOffset );
      fontData[i].setStyle( fontData[i].getStyle() | styleOffset );
    }
    return new Font( device, fontData );
  }
}
