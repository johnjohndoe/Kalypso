package org.kalypso.java.net.test;

import java.net.MalformedURLException;
import java.net.URL;

import junit.framework.TestCase;


/**
 * UrlUtilitiesTest
 * 
 * @author schlienger
 */
public class UrlUtilitiesTest extends TestCase
{

  public void testResolveURL( ) throws MalformedURLException
  {
    final URL base = new URL("file://c/temp/hello");
    
    final URL u1 = UrlUtilities.resolveURL( base, "" );
    System.out.println( u1 );
    assertEquals( base, u1 );

    final URL u2 = UrlUtilities.resolveURL( base, "world/toYou.txt" );
    System.out.println( u2 );
  }

}
