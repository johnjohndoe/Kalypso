package org.kalypso.auth;

import java.util.MissingResourceException;
import java.util.ResourceBundle;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.kalypso.auth.login.DefaultAuthenticator;
import org.kalypso.auth.login.IAuthenticator;
import org.kalypso.auth.scenario.IScenario;
import org.kalypso.auth.scenario.Scenario;
import org.kalypso.auth.user.IKalypsoUser;
import org.kalypso.auth.user.KalypsoUser;
import org.kalypso.auth.user.UserRights;
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

  /**
   * the one and only one kalypso user. By default it is set to a default one in order to allow developers to start
   * kalypso bypassing the login procedure. Once the login procedure is started, the user is set to null unless
   * authentication succeeded.
   */
  private IKalypsoUser m_user = new KalypsoUser( "default", UserRights.NO_RIGHTS, "", new IScenario[]
  { Scenario.DEFAULT_SCENARIO } );

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
    // important: once login procedure is started, directly set user to null.
    // Kalypso should be started using the product "org.kalypso.product.product"
    // which uses the KalypsoApplication which, in turn, calls this method.
    // During development, one might want to bypass the login stuff. So by default,
    // the user is set to some default one.
    m_user = null;

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
      
      // if user is still null, then inform user
      if( m_user == null )
        MessageDialog.openWarning( shell, "Kalypso-Login", "Login fehlgeschlagen." );
    }
    finally
    {
      if( shell != null )
        shell.dispose();
    }
  }

  /**
   * Returns a scenario from the available scenarios
   */
  public IScenario getScenario( final String scenarioId )
  {
    final IScenario[] scenarios = m_user.getAvailableScenarios();
    for( int i = 0; i < scenarios.length; i++ )
    {
      final IScenario scenario = scenarios[i];
      if( scenario.getId().equals( scenarioId ) )
        return scenario;
    }

    return null;
  }
  
  /**
   * This is a shorthand for calling
   * <code>
   * getScenario( getCurrentUser().getScenario() )
   * </code>
   * 
   * @return the scenario associated to the current user
   */
  public IScenario getScenarioForCurrentUser()
  {
    return getScenario( getCurrentUser().getScenario() );
  }
}
