package org.kalypso.java.net.test;

import java.net.MalformedURLException;
import java.net.URL;

import org.kalypso.java.net.UrlUtilities;

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
    
    final UrlUtilities urlUtilities = new UrlUtilities();
    final URL u1 = urlUtilities.resolveURL( base, "" );
    System.out.println( u1 );
    assertEquals( base, u1 );

    final URL u2 = urlUtilities.resolveURL( base, "world/toYou.txt" );
    System.out.println( u2 );
  }

}
