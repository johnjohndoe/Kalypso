/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.test;

import java.io.InputStreamReader;
import java.net.URL;

import junit.framework.TestCase;

import org.kalypso.commons.java.io.ReaderUtilities;

/**
 * @author doemming
 */
public class TestUtilities
{

  public static void compare( final String description, final URL orgiginalResource, final String testText ) throws Exception
  {
    final InputStreamReader reader = new InputStreamReader( orgiginalResource.openStream(),"UTF-8");
    final String orgText = ReaderUtilities.readStringFromReader( reader );
    final String orgText2 = orgText.replaceAll( "\\r", "\n" );
    final String testText2 = testText.replaceAll( "\\r", "\n" );
    final String orgText3 = orgText2.replaceAll( "\\n+", "\n" );
    final String testText3 = testText2.replaceAll( "\\n+", "\n" );

    // detailed (line by line)
    final String[] orgLine = orgText3.split( "\n" );
    final String[] testLine = testText3.split( "\n" );
    final int radius = 5;
    int i = 0;
    int error = 0;
    while( i < orgLine.length && i < testLine.length )
    {
      if( !orgLine[i].equals( testLine[i] ) )
        error++;
      i++;
    }
    i = 0;
    System.out.println( "\nerrors/line:" + error + "/" + Math.max( orgLine.length, testLine.length ) + "\n" );
    int reported = 0;
    while( i < orgLine.length && i < testLine.length )
    {
      if( !orgLine[i].equals( testLine[i] ) )
      {
        System.out.println( "\n" + reported + ":\n\tfrom URL:" );
        for( int n = i - radius; n < i + radius; n++ )
        {
          if( n > -1 && n < orgLine.length )
            System.out.println( n + ": " + orgLine[n] );
        }
        System.out.println( "\n\tfrom current run:" );
        for( int n = i - radius; n < i + radius; n++ )
        {
          if( n > -1 && n < testLine.length )
            System.out.println( n + ": " + testLine[n] );
        }
        reported++;
        if( reported > 5 )
          TestCase.fail( "broken test: " + description );
      }
      i++;
    }
    if( orgLine.length != testLine.length )
      TestCase.fail( "broken test: " + description );
    if( error > 0 )
      TestCase.fail( "broken test: " + description );
  }
}
