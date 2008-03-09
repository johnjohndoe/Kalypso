package org.kalypso.risk.model.actions.dataImport.landuse;

import java.util.MissingResourceException;
import java.util.ResourceBundle;

public class Messages
{
  private static final String BUNDLE_NAME = "org.kalypso.risk.model.actions.dataImport.landuse.messages"; //$NON-NLS-1$

  private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle( BUNDLE_NAME );

  private Messages( )
  {
  }

  public static String getString( String key )
  {
    try
    {
      return RESOURCE_BUNDLE.getString( key );
    }
    catch( MissingResourceException e )
    {
      return '!' + key + '!';
    }
  }
}
