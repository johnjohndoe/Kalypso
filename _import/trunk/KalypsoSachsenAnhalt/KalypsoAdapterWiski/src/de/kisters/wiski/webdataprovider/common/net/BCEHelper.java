package de.kisters.wiski.webdataprovider.common.net;

import java.net.Authenticator;
import java.net.PasswordAuthentication;
import java.net.SocketPermission;
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

  public static void configureProxy( final String host, final String port,
      final String user, final String password )
  {
    final SocketPermission p = new SocketPermission( host + ":" + port,
        "connect" );

    try
    {
      AccessController.checkPermission( p );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }

    System.setProperty( "http.proxySet", "true" );
    System.setProperty( "http.proxyHost", host );
    System.setProperty( "http.proxyPort", port );

    Authenticator.setDefault( new Authenticator()
    {
      protected PasswordAuthentication getPasswordAuthentication( )
      {
        return new PasswordAuthentication( user, password.toCharArray() );
      }
    } );
  }
}