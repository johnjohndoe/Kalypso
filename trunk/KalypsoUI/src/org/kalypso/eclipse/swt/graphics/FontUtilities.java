package org.kalypso.eclipse.swt.graphics;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;

import org.eclipse.swt.graphics.Device;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;

/**
 * <p>Helper class for deriving fonts from other fonts.</p>
 * <p>Keeps track of created fonts and disposes theses if this class is disposed</p>
 * 
 * @author belger
 */
public class FontUtilities
{
  private final Collection m_disposeFonts = new LinkedList();
  
  public void dispose()
  {
    for( final Iterator iter = m_disposeFonts.iterator(); iter.hasNext(); )
      ((Font)iter.next()).dispose();
  }
  
  public Font createChangedFontData( final FontData[] fontData, final int heightOffset, final int styleOffset, final Device device )
  {
    for( int i = 0; i < fontData.length; i++ )
    {
      fontData[i].setHeight( fontData[i].getHeight() + heightOffset );
      fontData[i].setStyle( fontData[i].getStyle() | styleOffset );
    }
    return new Font( device, fontData );
  }
}
