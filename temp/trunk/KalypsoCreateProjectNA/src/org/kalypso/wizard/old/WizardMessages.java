package org.kalypso.wizard.old;

import java.text.MessageFormat;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

/**
 * 
 * TODO: insert type comment here
 * 
 * @author huebsch
 */
public class WizardMessages
{
  private static final String BUNDLE_NAME = "org.kalypso.wizard.WizardMessages";//$NON-NLS-1$

  private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle( BUNDLE_NAME );

  private WizardMessages()
  {}

  /**
   * @param key -
   *          zu übersetzender Schlüssel
   * @return - die übersetzte Nachricht
   */
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

  /**
   * @param key -
   *          zu übersetzender Schlüssel
   * @param params -
   *          einzufügende Parameter
   * @return - die übersetzte Nachricht
   */
  public static String getString( String key, Object[] params )
  {
    if( params == null )
      return getString( key );
    try
    {
      return MessageFormat.format( getString( key ), params );
    }
    catch( Exception e )
    {
      return '!' + key + '!';
    }
  }
}
