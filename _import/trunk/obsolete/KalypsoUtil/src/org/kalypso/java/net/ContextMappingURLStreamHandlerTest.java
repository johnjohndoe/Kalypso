package org.kalypso.java.net;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.net.URLConnection;


import junit.framework.TestCase;

/**
 * @author schlienger
 */
public class ContextMappingURLStreamHandlerTest extends TestCase
{
  private ContextMappingURLStreamHandler handler;

  /**
   * @see junit.framework.TestCase#setUp()
   */
  protected void setUp() throws Exception
  {
    /*super.setUp();
   
    handler = new ContextMappingURLStreamHandler();

    final URL projectReplace = new URL( "file:C:/Programme/eclipse-3.0/workspace/KalypsoUtil/" );
    
    final URL wkspReplace = new URL( "file:C:/Programme/eclipse-3.0/workspace/" );
    
    handler.setStringReplacement( "project", projectReplace );
    handler.setStringReplacement( "workspace", wkspReplace );

    final ConfigurableURLStreamHandlerFactory fact = new ConfigurableURLStreamHandlerFactory(  );
    fact.setHandler( "project", handler );
    fact.setHandler( "workspace", handler );
    
    URL.setURLStreamHandlerFactory( fact );*/
  }

  public void testOpenConnection() throws IOException
  {
    /*final URL projUrl = new URL( "project:src/org/kalypso/java/package.html#testFragment#blabla#FOO=BAR" );
    final URLConnection conn = handler.openConnection( projUrl );
    assertNotNull(conn);
    
    final InputStream ins = conn.getInputStream();
    assertNotNull( ins );
    
    ins.close();

    final URL wkspUrl = new URL( "workspace:KalypsoUtil/src/org/kalypso/java/package.html#testFragment#blabla#FOO=BAR" );
    final URLConnection conn2 = handler.openConnection( wkspUrl );
    assertNotNull(conn2);
    
    final InputStream ins2 = conn.getInputStream();
    assertNotNull( ins2 );
    
    ins2.close();*/
  }
}
