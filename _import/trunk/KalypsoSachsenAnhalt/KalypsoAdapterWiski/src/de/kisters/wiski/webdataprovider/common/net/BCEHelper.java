package de.kisters.wiski.webdataprovider.common.net;

import java.net.Authenticator;
import java.net.PasswordAuthentication;

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
    System.setProperty( "proxySet", "true" );
    System.setProperty( "proxyHost", "128.1.5.18" );
    System.setProperty( "proxyPort", "8080" );

    Authenticator.setDefault( new Authenticator()
    {
      protected PasswordAuthentication getPasswordAuthentication( )
      {
        return new PasswordAuthentication( user, password.toCharArray() );
      }
    } );
  }
}
