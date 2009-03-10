/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.test.diff;

import java.io.File;
import java.io.InputStream;
import java.io.StringWriter;
import java.net.URL;
import java.util.logging.Level;

import junit.framework.TestCase;

import org.apache.commons.io.IOUtils;
import org.kalypso.commons.diff.DiffUtils;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.java.util.logging.ILogger;

/**
 * Test of the DiffUtilities
 * 
 * @author doemming
 */
public class DiffUtilitiesTest extends TestCase
{
  public void testZipDiff( ) throws Exception
  {
    try
    {
      final URL zipURL1 = getClass().getResource( "resources/test1.zip" );
      final File zipFile1 = FileUtilities.makeFileFromUrl( false, "test1", ".zip", zipURL1, true );
      final URL zipURL2 = getClass().getResource( "resources/test2.zip" );
      final File zipFile2 = FileUtilities.makeFileFromUrl( false, "test2", ".zip", zipURL2, true );
      final StringBuffer buffer = new StringBuffer();
      final ILogger logger = new ILogger()
      {
        /**
         * @see org.kalypso.contribs.java.util.logging.ILogger#log(java.util.logging.Level, int, java.lang.String)
         */
        public void log( final Level level, final int code, final String message )
        {
          System.out.println( message );
          buffer.append( message );
          buffer.append( "\n" );
        }
      };
      DiffUtils.diffZips( logger, zipFile1, zipFile2, new String[0] );
      final InputStream resourceAsStream = getClass().getResourceAsStream( "resources/difflog.txt" );
      final StringWriter writer = new StringWriter();
      IOUtils.copy( resourceAsStream, writer );
      final String test = buffer.toString().replaceAll( "\\s", "" );

      final String goal = writer.toString().replaceAll( "\\s", "" );
      assertEquals( test, goal );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw e;
    }
  }
}
