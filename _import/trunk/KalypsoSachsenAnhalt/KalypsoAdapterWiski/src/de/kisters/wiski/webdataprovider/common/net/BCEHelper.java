package de.kisters.wiski.webdataprovider.common.net;

import java.net.Authenticator;
import java.net.PasswordAuthentication;
import java.net.SocketPermission;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLConnection;
import java.security.AccessController;

/**
 * BCEHelper
 * 
 * @author schlienger
 */
public class BCEHelper
{
  private BCEHelper( )
  {
    // empty
  }

  public static void configureProxy( final String user, final String password )
  {
    final SocketPermission p = new SocketPermission( "10.123.123.66:10991", "connect" );
    
    try
    {
      AccessController.checkPermission( p );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }

    System.setProperty( "http.proxySet", "true" );
    System.setProperty( "http.proxyHost", "128.1.5.18" );
    System.setProperty( "http.proxyPort", "8080" );
    
//    Authenticator.setDefault( new Authenticator()
//    {
//      protected PasswordAuthentication getPasswordAuthentication( )
//      {
//        return new PasswordAuthentication( user, password.toCharArray() );
//      }
//    } );
  }
}