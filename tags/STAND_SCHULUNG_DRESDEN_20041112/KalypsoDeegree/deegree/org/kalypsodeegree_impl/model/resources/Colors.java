/*----------------    FILE HEADER  ------------------------------------------

 This file is part of deegree.
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon Fitzke/Fretter/Poth GbR
 http://www.lat-lon.de

 It has been implemented within SEAGIS - An OpenSource implementation of OpenGIS specification
 (C) 2001, Institut de Recherche pour le Développement (http://sourceforge.net/projects/seagis/)
 SEAGIS Contacts:  Surveillance de l'Environnement Assistée par Satellite
 Institut de Recherche pour le Développement / US-Espace
 mailto:seasnet@teledetection.fr


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

 Andreas Poth
 lat/lon Fitzke/Fretter/Poth GbR
 Meckenheimer Allee 176
 53115 Bonn
 Germany
 E-Mail: poth@lat-lon.de

 Jens Fitzke
 Department of Geography
 University of Bonn
 Meckenheimer Allee 166
 53115 Bonn
 Germany
 E-Mail: jens.fitzke@uni-bonn.de

 
 ---------------------------------------------------------------------------*/
package org.deegree_impl.model.resources;

// Miscellaneous
import java.awt.Color;
import java.awt.image.DataBuffer;
import java.awt.image.IndexColorModel;
import java.util.Arrays;

/**
 * Utilities methods for handling of colors informations.
 * 
 * @version 1.0
 * @author Martin Desruisseaux
 */
public final class Colors
{
  /**
   * Small number for rounding errors.
   */
  private static final double EPS = 1E-6;

  /**
   * Do not allow instantiation of this class.
   */
  private Colors()
  {}

  /**
   * Copy <code>colors</code> into array <code>ARGB</code> from index
   * <code>lower</code> inclusive to index <code>upper</code> exclusive. If
   * <code>upper-lower</code> is not equals to the length of
   * <code>colors</code> array, then colors will be interpolated.
   * 
   * @param colors
   *          Colors to copy into the <code>ARGB</code> array.
   * @param ARGB
   *          Array of integer to write ARGB values to.
   * @param lower
   *          Index (inclusive) of the first element of <code>ARGB</code> to
   *          change.
   * @param upper
   *          Index (exclusive) of the last element of <code>ARGB</code> to
   *          change.
   */
  public static void expand( final Color[] colors, final int[] ARGB, final int lower,
      final int upper )
  {
    switch( colors.length )
    {
    case 1:
      Arrays.fill( ARGB, lower, upper, colors[0].getRGB() ); // fall through
    case 0:
      return; // Note: getRGB() is really getARGB()
    }
    switch( upper - lower )
    {
    case 1:
      ARGB[lower] = colors[0].getRGB(); // fall through
    case 0:
      return; // Note: getRGB() is really getARGB()
    }
    final int maxBase = colors.length - 2;
    final double scale = (double)( colors.length - 1 ) / (double)( upper - 1 - lower );
    for( int i = lower; i < upper; i++ )
    {
      final double index = ( i - lower ) * scale;
      final int base = Math.min( maxBase, (int)( index + EPS ) ); // Round
      // toward 0,
      // which is
      // really what
      // we want.
      final double delta = index - base;
      final Color C0 = colors[base + 0];
      final Color C1 = colors[base + 1];
      int A = C0.getAlpha();
      int R = C0.getRed();
      int G = C0.getGreen();
      int B = C0.getBlue();
      ARGB[i] = ( round( A + delta * ( C1.getAlpha() - A ) ) << 24 )
          | ( round( R + delta * ( C1.getRed() - R ) ) << 16 )
          | ( round( G + delta * ( C1.getGreen() - G ) ) << 8 )
          | ( round( B + delta * ( C1.getBlue() - B ) ) << 0 );
    }
  }

  /**
   * Round a float value and clamp the result between 0 and 255 inclusive.
   */
  private static int round( final double value )
  {
    return Math.min( Math.max( (int)Math.round( value ), 0 ), 255 );
  }

  /**
   * Returns an index color model for specified ARGB codes. If the specified
   * array has not transparent color (i.e. all alpha values are 255), then the
   * returned color model will be opaque. Otherwise, if the specified array has
   * one and only one color with alpha value of 0, the returned color model will
   * have only this transparent color. Otherwise, the returned color model will
   * be translucide.
   * 
   * @param ARGB
   *          An array of ARGB values.
   * @return An index color model for the specified array.
   */
  public static IndexColorModel getIndexColorModel( final int[] ARGB )
  {
    boolean hasAlpha = false;
    int transparent = -1;
    for( int i = 0; i < ARGB.length; i++ )
    {
      final int alpha = ARGB[i] & 0xFF000000;
      if( alpha != 0xFF000000 )
      {
        if( alpha == 0x00000000 && transparent < 0 )
        {
          transparent = i;
          continue;
        }
        hasAlpha = true;
        break;
      }
    }
    return new IndexColorModel( getBitCount( ARGB.length ), ARGB.length, ARGB, 0, hasAlpha,
        transparent, getTransferType( ARGB.length ) );
  }

  /**
   * Returns a suggered bit count for an {@link IndexColorModel}of
   * <code>mapSize</code> colors. This method returns 1, 2, 4, 8 or 16
   * according the value of <code>mapSize</code>. It is guaranteed that the
   * following relation is hold:
   * 
   * <center>
   * 
   * <pre>(1 << getBitCount(mapSize)) >= mapSize</pre>
   * 
   * </center>
   */
  public static int getBitCount( final int mapSize )
  {
    if( mapSize <= 0x00002 )
      return 1;
    if( mapSize <= 0x00004 )
      return 2;
    if( mapSize <= 0x00010 )
      return 4;
    if( mapSize <= 0x00100 )
      return 8;
    if( mapSize <= 0x10000 )
      return 16;
    throw new IllegalArgumentException( Integer.toString( mapSize ) );
  }

  /**
   * Returns a suggered type for an {@link IndexColorModel}of
   * <code>mapSize</code> colors. This method returns
   * {@link DataBuffer#TYPE_BYTE}or {@link DataBuffer#TYPE_USHORT}.
   */
  private static int getTransferType( final int mapSize )
  {
    return ( mapSize <= 256 ) ? DataBuffer.TYPE_BYTE : DataBuffer.TYPE_SHORT;
  }
}