package org.kalypso.risk;

import java.util.MissingResourceException;
import java.util.ResourceBundle;

public class Messages
{
  private static final String BUNDLE_NAME = "org.kalypso.risk.messages"; //$NON-NLS-1$

  private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle( BUNDLE_NAME );

  private Messages( )
  {
  }

  public static String getString( final String key )
  {
    try
    {
      return RESOURCE_BUNDLE.getString( key );
    }
    catch( final MissingResourceException e )
    {
      return '!' + key + '!';
    }
  }
}
