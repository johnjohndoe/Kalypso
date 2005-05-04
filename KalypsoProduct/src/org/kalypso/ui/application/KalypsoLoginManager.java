package org.kalypso.ui.application;

import java.rmi.RemoteException;

import javax.xml.rpc.ServiceException;

import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.services.proxy.IUserService;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.users.User;
import org.kalypso.users.UserServiceConstants;

/**
 * KalypsoLoginManager
 * 
 * @author schlienger
 */
public final class KalypsoLoginManager
{
  private KalypsoLoginManager( )
  {
    // not intended to be instanciated
  }

  //  private foo( )
  //  {
  //    final String[] userRights = KalypsoGisPlugin.getDefault().getUserRights();
  //
  //    final StringBuffer userMsg = new StringBuffer( "Rights from server:" );
  //    for( int i = 0; i < userRights.length; i++ )
  //      userMsg.append( "\n'" + userRights[i] + "'" );
  //
  //    m_logger.info( userMsg.toString() );
  //
  //    final String[] rights = chooseRight( userRights );
  //
  //    if( rights.length == 0 )
  //      return IPlatformRunnable.EXIT_OK;
  //
  //    final StringBuffer chooseMsg = new StringBuffer( "Chosen rights:" );
  //    for( int i = 0; i < rights.length; i++ )
  //      chooseMsg.append( "\n'" + rights[i] + "'" );
  //    m_logger.info( chooseMsg.toString() );
  //  }

  public static User startLoginProcedure( )
  {
    IUserService srv = null;

    try
    {
      srv = KalypsoGisPlugin.getDefault().getUserServiceProxy();
    }
    catch( final ServiceException e )
    {
      e.printStackTrace();
    }

    final String title = "Kalypso - Login";
    String message = "Es konnten keine Benutzerrechte vom Server ermittelt werden. "
        + "Geben Sie das Administrator-Passwort ein, um im Administrator-Modus zu starten.";
    String userName = "Administrator";
    boolean useLogin = false;
    boolean useScenario = false;
    String[] scenarios = {};
    String[] scenarioDescs = {};

    ILoginValidator lv = null;

    if( srv != null )
    {
      message = "Melden Sie sich bitte an.";
      userName = System.getProperty( "user.name" );
      try
      {
        useLogin = srv.isAskForLogin();
        useScenario = srv.isAskForScenario();
        scenarios = srv.getScenarios();
        scenarioDescs = srv.getScenarioDescriptions();
      }
      catch( final RemoteException e )
      {
        e.printStackTrace();
      }

      if( useLogin )
        lv = new AuthLoginValidator( srv );
      else
        lv = new SingleLoginValidator( srv );
    }
    else
      lv = new AppLoginValidator();

    Display display = null;
    Shell shell = null;
    if( useLogin || useScenario )
    {
      display = new Display();
      shell = new Shell( display, SWT.SYSTEM_MODAL );
      shell.setImage( ImageProvider.IMAGE_KALYPSO_ICON.createImage() );
    }

    String password = null;
    User user = null;

    while( true )
    {
      if( useLogin || useScenario )
      {
        final KalypsoLoginDialog dlg = new KalypsoLoginDialog( shell, title,
            message, userName, useLogin, useLogin, useScenario, scenarios,
            scenarioDescs );

        if( dlg.open() == Window.OK )
        {
          userName = dlg.getUserName();
          password = dlg.getPassword();

          // TODO m_currentScenario = dlg.getSelectedScenario();
        }
        else
        {
          user = null;
          break;
        }
      }

      user = lv.validate( userName, password );

      if( user != null )
        break;
    }

    KalypsoGisPlugin.getDefault().setUser( user );

    if( shell != null )
      shell.dispose();
    if( display != null )
      display.dispose();

    return user;
  }

  public static interface ILoginValidator
  {
    public User validate( final String username, final String password );
  }

  private static class AppLoginValidator implements ILoginValidator
  {
    public User validate( final String username, final String password )
    {
      if( "hochwasser".equals( password ) )
        return new User( "Administrator",
            new String[] { UserServiceConstants.RIGHT_ADMIN } );

      return null;
    }
  }

  private static class AuthLoginValidator implements ILoginValidator
  {
    private final IUserService m_srv;

    public AuthLoginValidator( final IUserService srv )
    {
      m_srv = srv;
    }

    public User validate( final String username, final String password )
    {
      try
      {
        final String[] rights = m_srv.getRights2( username, password );

        if( rights != null )
          return new User( username, rights );
      }
      catch( final RemoteException e )
      {
        e.printStackTrace();
      }

      return null;
    }
  }

  private static class SingleLoginValidator implements ILoginValidator
  {
    private final IUserService m_srv;

    public SingleLoginValidator( final IUserService srv )
    {
      m_srv = srv;
    }

    public User validate( final String username, final String password )
    {
      try
      {
        final String[] rights = m_srv.getRights( username );

        if( rights != null )
          return new User( username, rights );
      }
      catch( final RemoteException e )
      {
        e.printStackTrace();
      }

      return null;
    }
  }
}
