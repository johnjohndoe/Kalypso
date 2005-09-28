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
package org.kalypso.contribs.java.io;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

/**
 * @author belger
 */
public class StreamUtilities
{
  private StreamUtilities()
  {
  // diese Klasse wird nicht instantiiert
  }

  /**
   * Kopiert den Inhalt eines Streams in einen anderen. Die Streams werden nach Ende der Operation geschlossen.
   */
  public static final void streamCopy( final InputStream is, final OutputStream os ) throws IOException
  {
    final byte[] buffer = new byte[1024 * 16];
    while( true )
    {
      final int i = is.read( buffer );
      if( i == -1 )
        break;

      os.write( buffer, 0, i );
    }

    is.close();
    os.close();
  }

  public static void dumpAllAvailable( final InputStream is ) throws IOException
  {
    while( true )
    {
      final int avail = is.available();
      if( avail == 0 )
        return;

      final byte[] inBuffer = new byte[avail];
      is.read( inBuffer );
    }
  }

  /**
   * compares content of two inputstreams
   * 
   * @param isA
   * @param isB
   * @return true is content is equal
   * @throws IOException
   */
  public static final boolean isEqual( final InputStream isA, final InputStream isB ) throws IOException
  {
    try
    {
      final byte[] bufferA = new byte[1024 * 16];
      final byte[] bufferB = new byte[1024 * 16];
      byte[] restA = new byte[0];
      byte[] restB = new byte[0];
      int a = 0;
      int b = 0;
      while( true )
      {
        if( a > -1 )
          a = isA.read( bufferA );
        if( b > -1 )
          b = isB.read( bufferB );

        if( a == -1 && b == -1 )
          return restA.length == 0 && restB.length == 0;
        if( a > -1 && b > -1 )
        {
          byte[] mergeA = org.kalypso.contribs.java.util.Arrays.append( restA, bufferA, a );
          byte[] mergeB = org.kalypso.contribs.java.util.Arrays.append( restB, bufferB, b );
          if( !org.kalypso.contribs.java.util.Arrays.equals( mergeA, mergeB, Math.min( mergeA.length, mergeB.length ) ) )
            return false;
          if( mergeB.length > mergeA.length )
          {
            restA = new byte[0];
            restB = org.kalypso.contribs.java.util.Arrays.copyPart( mergeB, mergeA.length, mergeB.length );
          }
          else
          {
            restA = org.kalypso.contribs.java.util.Arrays.copyPart( mergeA, mergeB.length, mergeA.length );
            restB = new byte[0];
          }
        }
      }
    }
    finally
    {
      isA.close();
      isB.close();
    }
  }
}