package org.deegree_impl.services.wms.capabilities.test;

import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URL;
import java.net.URLConnection;

import junit.framework.TestCase;
import sun.misc.BASE64Encoder;

/**
 * @author sbad0205
 */
public class OGCWMSCapabilitiesFactoryTest extends TestCase
{

  /*
   * Class under test for WMSCapabilities createCapabilities(URL, String,
   * String, String, String)
   */
  public void testCreateCapabilitiesURLStringStringStringString()
  {
    try
    {

      System.setProperty( "proxySet", "true" );
      System.setProperty( "proxyHost", "172.16.0.1" );
      System.setProperty( "proxyPort", "8080" );
      //     URL url=new
      // URL("http","demo.deegree.org",8080,"/deegree/wms?SERVICE=WMS&VERSION=1.1.1&REQUEST=GetCapabilities");
      URL url = new URL( "http", "demo.deegree.org", 8080, "/deegree/wms" );

      URLConnection c = url.openConnection();
      c.addRequestProperty( "SERVICE", "WMS" );
      c.addRequestProperty( "VERSION", "1.1.1" );
      c.addRequestProperty( "REQUEST", "GetCapabilities" );
      String pw = "belger:LaufMensch";
      String epw = "Basic " + ( new BASE64Encoder() ).encode( pw.getBytes() );

      c.setRequestProperty( "Proxy-Authorization", epw );
      Reader reader = new InputStreamReader( c.getInputStream() );

      //     OGCWMSCapabilitiesFactory wmsCapFac=new OGCWMSCapabilitiesFactory();
      //      
      //   WMSCapabilities wmsCaps=wmsCapFac.createCapabilities(url, "172.16.0.1",
      // "8080", "belger", "LaufMensch");
      //   System.out.println(wmsCaps.exportAsXML()+"\n OK");
    }
    catch( Exception e )
    {
      e.printStackTrace();
      fail( e.getMessage() );
    }
  }

}