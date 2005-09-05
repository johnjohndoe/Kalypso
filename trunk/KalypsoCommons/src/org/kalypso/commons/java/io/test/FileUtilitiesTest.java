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
package org.kalypso.commons.java.io.test;

import java.io.File;

import junit.framework.TestCase;

import org.kalypso.commons.java.io.FileUtilities;

/**
 * @author schlienger
 */
public class FileUtilitiesTest extends TestCase
{
  public void testIsChildOf()
  {
    final File f = new File( "C:/temp" );
    final File c = new File( "C:/TEMP/grafik-kalypso/zml/Beispiel/beispiel.zml" );

    assertTrue( FileUtilities.isChildOf( f, c ) );

    final File c2 = new File( "C:/temp3/foo.txt" );

    assertFalse( FileUtilities.isChildOf( f, c2 ) );

    final File c3 = new File( "C:/temp" );

    assertTrue( FileUtilities.isChildOf( f, c3 ) );
  }
  
  public void testValidateName()
  {
    assertEquals( "foobar", FileUtilities.validateName( "\\foo*bar?", "" ) );
    assertEquals( "robertocarlos", FileUtilities.validateName( "/roberto/carlos", "" ) );
    assertEquals( "plusminus", FileUtilities.validateName( "plus:minus", "" ) );
    assertEquals( "xml", FileUtilities.validateName( "<xml>", "" ) );
    assertEquals( "strongweak", FileUtilities.validateName( "strong|weak", "" ) );
    assertEquals( "whatthehell", FileUtilities.validateName( "what\"the\"hell", "" ) );
  }
}
