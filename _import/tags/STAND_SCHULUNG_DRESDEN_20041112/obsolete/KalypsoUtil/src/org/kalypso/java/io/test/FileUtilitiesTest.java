package org.kalypso.java.io.test;

import java.io.File;

import junit.framework.TestCase;

import org.kalypso.java.io.FileUtilities;

/**
 * @author schlienger
 */
public class FileUtilitiesTest extends TestCase
{
  public void testIsChildOf( )
  {
    final File f = new File( "C:/temp" );
    final File c = new File( "C:/TEMP/grafik-kalypso/zml/Beispiel/beispiel.zml" );
    
    assertTrue( FileUtilities.isChildOf( f, c ) );
    
    final File c2 = new File( "C:/temp3/foo.txt" );
    
    assertFalse( FileUtilities.isChildOf( f, c2 ) );
    
    final File c3 = new File( "C:/temp" );
    
    assertTrue( FileUtilities.isChildOf( f, c3 ) );
  }
}
