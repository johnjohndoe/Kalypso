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
package org.kalypso.java.net;

import junit.framework.TestCase;

/**
 * @author schlienger
 */
public class ContextMappingURLStreamHandlerTest extends TestCase
{
//  private ContextMappingURLStreamHandler handler;
//
//  /**
//   * @see junit.framework.TestCase#setUp()
//   */
//  protected void setUp() throws Exception
//  {
//  /*
//   * super.setUp();
//   * 
//   * handler = new ContextMappingURLStreamHandler();
//   * 
//   * final URL projectReplace = new URL(
//   * "file:C:/Programme/eclipse-3.0/workspace/KalypsoUtil/" );
//   * 
//   * final URL wkspReplace = new URL( "file:C:/Programme/eclipse-3.0/workspace/" );
//   * 
//   * handler.setStringReplacement( "project", projectReplace );
//   * handler.setStringReplacement( "workspace", wkspReplace );
//   * 
//   * final ConfigurableURLStreamHandlerFactory fact = new
//   * ConfigurableURLStreamHandlerFactory( ); fact.setHandler( "project", handler );
//   * fact.setHandler( "workspace", handler );
//   * 
//   * URL.setURLStreamHandlerFactory( fact );
//   */
//  }
//
//  public void testOpenConnection() throws IOException
//  {
//  /*
//   * final URL projUrl = new URL(
//   * "project:src/org/kalypso/java/package.html#testFragment#blabla#FOO=BAR" );
//   * final URLConnection conn = handler.openConnection( projUrl );
//   * assertNotNull(conn);
//   * 
//   * final InputStream ins = conn.getInputStream(); assertNotNull( ins );
//   * 
//   * ins.close();
//   * 
//   * final URL wkspUrl = new URL(
//   * "workspace:KalypsoUtil/src/org/kalypso/java/package.html#testFragment#blabla#FOO=BAR" );
//   * final URLConnection conn2 = handler.openConnection( wkspUrl );
//   * assertNotNull(conn2);
//   * 
//   * final InputStream ins2 = conn.getInputStream(); assertNotNull( ins2 );
//   * 
//   * ins2.close();
//   */
//  }
}