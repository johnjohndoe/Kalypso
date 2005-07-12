package org.kalypso.auth;

import java.util.MissingResourceException;
import java.util.ResourceBundle;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.kalypso.auth.login.DefaultAuthenticator;
import org.kalypso.auth.login.IAuthenticator;
import org.kalypso.auth.user.IKalypsoUser;
import org.osgi.framework.BundleContext;

/**
 * The main plugin class to be used in the desktop.
 */
public class KalypsoAuthPlugin extends AbstractUIPlugin
{
  //The shared instance.
  private static KalypsoAuthPlugin plugin;

  //Resource bundle.
  private ResourceBundle resourceBundle;

  private IKalypsoUser m_user = null;

  /**
   * The constructor.
   */
  public KalypsoAuthPlugin()
  {
    super();
    plugin = this;
    try
    {
      resourceBundle = ResourceBundle.getBundle( "org.kalypso.auth.KalypsoAuthPluginResources" );
    }
    catch( MissingResourceException x )
    {
      resourceBundle = null;
    }
  }

  /**
   * This method is called upon plug-in activation
   */
  public void start( BundleContext context ) throws Exception
  {
    super.start( context );
  }

  /**
   * This method is called when the plug-in is stopped
   */
  public void stop( BundleContext context ) throws Exception
  {
    super.stop( context );
  }

  /**
   * Returns the shared instance.
   */
  public static KalypsoAuthPlugin getDefault()
  {
    return plugin;
  }

  /**
   * Returns the string from the plugin's resource bundle, or 'key' if not found.
   */
  public static String getResourceString( String key )
  {
    ResourceBundle bundle = KalypsoAuthPlugin.getDefault().getResourceBundle();
    try
    {
      return ( bundle != null ) ? bundle.getString( key ) : key;
    }
    catch( MissingResourceException e )
    {
      return key;
    }
  }

  /**
   * Returns the plugin's resource bundle,
   */
  public ResourceBundle getResourceBundle()
  {
    return resourceBundle;
  }

  /**
   * @return current user
   * @throws IllegalStateException
   *           if login procedure was not started
   */
  public IKalypsoUser getCurrentUser()
  {
    if( m_user == null )
      throw new IllegalStateException( "No user" );

    return m_user;
  }

  /**
   * Starts the login procedure. Goes through the extensions for IAuthenticator and lets the first available one
   * authenticate the user. As last resort the DefaultAuthenticator is used.
   * 
   * @throws InterruptedException
   *           means user cancels the login procedure
   * @throws CoreException
   *           means error while retrieving the extensions for org.kalypso.auth.login.IAuthenticator
   */
  public void startLoginProcedure( final Display display ) throws InterruptedException, CoreException
  {
    final Shell shell = new Shell( display, SWT.SYSTEM_MODAL );
    shell.setImage( ImageProvider.IMAGE_KALYPSO_ICON.createImage() );

    try
    {
      final IAuthenticator[] authenticators = AuthenticatorExtensions.retrieveExtensions();
      for( int i = 0; i < authenticators.length; i++ )
      {
        try
        {
          m_user = authenticators[i].authenticate( shell );
          return;
        }
        catch( final InterruptedException e )
        {
          // cancelled by user, so stop
          m_user = null;

          throw e;
        }
        catch( final Exception e )
        {
          // just stack trace
          e.printStackTrace();
        }
      }

      // no authentication succeeded till now, so let's give a last chance
      // using the default authenticator
      m_user = new DefaultAuthenticator().authenticate( shell );
    }
    finally
    {
      if( shell != null )
        shell.dispose();
    }
  }
}
