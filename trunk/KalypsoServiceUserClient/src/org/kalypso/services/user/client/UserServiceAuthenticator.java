/*--------------- Kalypso-Header ------------------------------------------

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

 --------------------------------------------------------------------------*/

package org.kalypso.services.user.client;

import java.rmi.RemoteException;
import java.util.logging.Logger;

import javax.xml.rpc.ServiceException;

import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.auth.login.IAuthenticator;
import org.kalypso.auth.scenario.IScenario;
import org.kalypso.auth.scenario.Scenario;
import org.kalypso.auth.ui.KalypsoLoginDialog;
import org.kalypso.auth.user.IKalypsoUser;
import org.kalypso.auth.user.KalypsoUser;
import org.kalypso.contribs.java.util.Arrays;
import org.kalypso.services.proxy.IUserService;

/**
 * @author schlienger
 */
public class UserServiceAuthenticator implements IAuthenticator
{
  private final static String MSG = "Melden Sie sich bitte an.";

  /**
   * @see org.kalypso.auth.login.IAuthenticator#authenticate(org.eclipse.swt.widgets.Shell)
   */
  public IKalypsoUser authenticate( final Shell shell ) throws ServiceException, RemoteException, InterruptedException
  {
    final IUserService srv = KalypsoServiceUserClientPlugin.getDefault().getUserServiceProxy();

    final ScenarioBean[] beans = srv.getScenarios();

    IScenario scenario = Scenario.DEFAULT_SCENARIO;
    final IScenario[] scenarios;
    if( beans.length > 0 )
    {
      scenarios = new IScenario[beans.length];
      for( int i = 0; i < scenarios.length; i++ )
        scenarios[i] = new Scenario( beans[i].getId(), beans[i].getProps() );
    }
    else
      scenarios = new IScenario[]
      { scenario };

    String username = System.getProperty( "user.name" );

    final boolean askForLogin = srv.isAskForLogin();
    final boolean askForScenario = srv.isAskForScenario();
    if( askForLogin || askForScenario )
    {
      final int iter = askForLogin ? 3 : 1;

      for( int i = 0; i < iter; i++ )
      {
        final KalypsoLoginDialog dlg = new KalypsoLoginDialog( shell, MSG, askForLogin, username, askForLogin,
            askForScenario, scenarios );
        if( dlg.open() == Window.OK )
        {
          username = dlg.getUserName();
          if( askForScenario )
            scenario = dlg.getSelectedScenario();

          if( askForLogin )
          {
            // using authentication
            final String[] rights = srv.getRights2( username, dlg.getPassword() );
            if( rights == null )
              Logger.getLogger( getClass().getName() ).info(
                  "Keine Nutzerrechte f�r Nutzer '" + username + "' erhalten." );
            else
              Logger.getLogger( getClass().getName() ).info(
                  "Folgende Nutzerrechte wurden f�r Nutzer '" + username + "' erhalten: "
                      + Arrays.toString( rights, "," ) );
            if( rights != null && rights.length > 0 )
              return new KalypsoUser( username, rights, scenario.getId(), scenarios );
          }
          else
          {
            // using single sign on
            final String[] rights = srv.getRights( username );
            if( rights == null )
              Logger.getLogger( getClass().getName() ).info(
                  "Keine Nutzerrechte f�r Nutzer '" + username + "' erhalten." );
            else
              Logger.getLogger( getClass().getName() ).info(
                  "Folgende Nutzerrechte wurden f�r Nutzer '" + username + "' erhalten: "
                      + Arrays.toString( rights, "," ) );
            if( rights != null && rights.length > 0 )
              return new KalypsoUser( username, rights, scenario.getId(), scenarios );
          }
        }
        else
          throw new InterruptedException();
      }
    }
    else
    {
      // using single sign on
      final String[] rights = srv.getRights( username );
      if( rights != null && rights.length > 0 )
        return new KalypsoUser( username, rights, scenario.getId(), scenarios );
    }

    // throw an exception to indicate error state
    throw new IllegalStateException( "Authentifizierung fehlgeschlagen" );
  }
}
