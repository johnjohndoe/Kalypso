/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ui.application;

import java.rmi.RemoteException;

import javax.xml.rpc.ServiceException;

import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.services.proxy.IUserService;
import org.kalypso.ui.KPImageProvider;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.application.login.AppLoginValidator;
import org.kalypso.ui.application.login.AuthLoginValidator;
import org.kalypso.ui.application.login.ILoginValidator;
import org.kalypso.ui.application.login.SingleLoginValidator;
import org.kalypso.users.User;

/**
 * Takes care of handling various login-possibilities. The various login possibilities are configured by the
 * KalypsoUserService. If the KalypsoUserService is not available (either down or not used at all) the login manager
 * uses the default AppLoginValidator which provides a static login facility.
 * 
 * @author schlienger
 */
public final class KalypsoLoginManager
{
  private KalypsoLoginManager()
  {
  // not intended to be instanciated
  }

  /**
   * The login procedure follows this path:
   * <ul>
   * <li>it tries to access the UserService
   * <li>if the UserService is up, then it asks it for its configuration (see UserService for more information)
   * <li>if the UserService is down, then it uses the default AppLoginValidator
   * </ul>
   */
  public static User startLoginProcedure( final Display display )
  {
    IUserService srv = null;

    try
    {
      srv = KalypsoGisPlugin.getDefault().getUserServiceProxy();
    }
    catch( final ServiceException e )
    {
      // for information
      e.printStackTrace();
    }

    ILoginValidator lv = null;

    boolean useLogin = false;
    boolean useScenario = false;
    String[] scenarios = {};
    String[] scenarioDescriptions = {};

    if( srv != null )
    {
      // the UserService is up so ask for its configuration. We basically have
      // two possibilities to log a user in Kalypso:
      // 1. using full authentication (user can change name and password)
      // 2. using pseudo-single-sign-on (user can only change password)
      try
      {
        useLogin = srv.isAskForLogin();

        if( useLogin )
          lv = new AuthLoginValidator( srv );
        else
          lv = new SingleLoginValidator( srv );

        // scenarios make only sense when the user service is
        // available, that's why these properties are only
        // initialised here
        useScenario = srv.isAskForScenario();
        scenarios = srv.getScenarios();
        scenarioDescriptions = srv.getScenarioDescriptions();
      }
      catch( final RemoteException e )
      {
        // shit happens, the user service seems to have difficulties
        // with our request for its configuration, so let the user
        // log in with the admin password
        e.printStackTrace();

        useLogin = true;
        lv = new AppLoginValidator();
      }
    }
    else
    {
      // there's no UserService so let the user log in with the
      // admin password
      useLogin = true;
      lv = new AppLoginValidator();
    }

    Shell shell = null;

    String username = lv.getDefaultUserName();
    User user = null;

    int trials = 5;

    while( true )
    {
      String password = null;

      if( useLogin || useScenario )
      {
        // make sure to have a display
        shell = new Shell( display, SWT.SYSTEM_MODAL );
        shell.setImage( KPImageProvider.IMAGE_KALYPSO_ICON.createImage() );

        final KalypsoLoginDialog dlg = new KalypsoLoginDialog( shell, "Kalypso - Login", lv.getMessage(), username, lv
            .userNameChangeable(), lv.passwordEnabled(), useScenario, scenarios, scenarioDescriptions );

        if( dlg.open() == Window.OK )
        {
          username = dlg.getUserName();
          password = dlg.getPassword();

          // TODO m_currentScenario = dlg.getSelectedScenario();
        }
        else
        {
          // cancelled by user, so stop
          user = null;
          break;
        }
      }

      try
      {
        trials--;
        user = lv.validate( username, password );

        if( user != null )
          break; // user successfully authenticated
        if( trials == 0 )
          throw new IllegalStateException( "Benutzer " + username
              + " kann sich nicht einloggen. Seine Berechtigung sollte " + "geprüft werden." );
      }
      catch( final Exception e )
      {
        e.printStackTrace();

        // if was using the server, still give a
        // chance to log in with admin password
        lv = new AppLoginValidator();
        username = lv.getDefaultUserName();
        useLogin = true;
      }
    }

    KalypsoGisPlugin.getDefault().setUser( user );

    if( shell != null )
      shell.dispose();

    return user;
  }
}