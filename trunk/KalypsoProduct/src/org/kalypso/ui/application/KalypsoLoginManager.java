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
import org.kalypso.ui.application.login.AppLoginValidator;
import org.kalypso.ui.application.login.AuthLoginValidator;
import org.kalypso.ui.application.login.ILoginValidator;
import org.kalypso.ui.application.login.SingleLoginValidator;
import org.kalypso.users.User;

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

    ILoginValidator lv = null;

    boolean useLogin = false;
    boolean useScenario = false;
    String[] scenarios = {};
    String[] scenarioDescriptions = {};

    if( srv != null )
    {
      try
      {
        useLogin = srv.isAskForLogin();

        if( useLogin )
          lv = new AuthLoginValidator( srv );
        else
          lv = new SingleLoginValidator( srv );

        useScenario = srv.isAskForScenario();
        scenarios = srv.getScenarios();
        scenarioDescriptions = srv.getScenarioDescriptions();
      }
      catch( final RemoteException e )
      {
        e.printStackTrace();
        lv = new AppLoginValidator();
      }
    }
    else
    {
      useLogin = true;
      lv = new AppLoginValidator();
    }

    Display display = null;
    Shell shell = null;
    if( useLogin || useScenario )
    {
      display = new Display();
      shell = new Shell( display, SWT.SYSTEM_MODAL );
      shell.setImage( ImageProvider.IMAGE_KALYPSO_ICON.createImage() );
    }

    String username = lv.getDefaultUserName();
    User user = null;

    while( true )
    {
      String password = null;

      if( useLogin || useScenario )
      {
        final KalypsoLoginDialog dlg = new KalypsoLoginDialog( shell,
            "Kalypso - Login", lv.getMessage(), username, lv
                .userNameChangeable(), lv.passwordEnabled(), useScenario,
            scenarios, scenarioDescriptions );

        if( dlg.open() == Window.OK )
        {
          username = dlg.getUserName();
          password = dlg.getPassword();

          // TODO m_currentScenario = dlg.getSelectedScenario();
        }
        else
        {
          user = null;
          break;
        }
      }

      try
      {
        user = lv.validate( username, password );
        
        if( user != null )
          break;
      }
      catch( final Exception e )
      {
        e.printStackTrace();

        // if was using the server, still give a chance to
        // use kalypso app login
        lv = new AppLoginValidator( );
        username = lv.getDefaultUserName();
      }
    }

    KalypsoGisPlugin.getDefault().setUser( user );

    if( shell != null )
      shell.dispose();
    if( display != null )
      display.dispose();

    return user;
  }
}
