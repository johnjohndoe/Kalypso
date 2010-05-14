/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.test.bsu.wfs;

import java.io.File;
import java.io.FileWriter;
import java.net.URL;
import java.security.Principal;
import java.util.Iterator;
import java.util.Set;

import javax.security.auth.callback.Callback;
import javax.security.auth.callback.CallbackHandler;
import javax.security.auth.callback.NameCallback;
import javax.security.auth.callback.PasswordCallback;
import javax.security.auth.login.LoginContext;

import junit.framework.TestCase;

import org.apache.commons.io.CopyUtils;
import org.apache.commons.io.IOUtils;

/**
 * @author doemming
 */
public class SingleSignonTest extends TestCase
{

  public void testSigngleSignon( ) throws Exception
  {
    try
    {
      copy( new File( "D:/eclipse3.1/tmp/web_FlowsAStestLogin.html" ) );
      LoginContext loginContext = null;
      System.setProperty( "java.security.auth.login.config", "D:/eclipse3.1/tmp/jaasConf.txt" );
      // Login-Kontext für die Konfiguration "Demo" erzeugen
      // loginContext = new LoginContext( "Demo" );
      loginContext = new LoginContext( "Demo", new CallbackHandler()
      {

        public void handle( Callback[] callbacks )
        {
          for( int i = 0; i < callbacks.length; i++ )
          {
            Callback callback = callbacks[i];
            if( callback instanceof NameCallback )
            {
              final NameCallback nCall = (NameCallback) callback;
              System.out.println( nCall.getPrompt() );
              nCall.setName( "Flowsad" );
            }
            else if( callback instanceof PasswordCallback )
            {
              final PasswordCallback call = (PasswordCallback) callback;
              System.out.println( call.getPrompt() );
              call.setPassword( new char[] { ' ', ' ', } );
            }
            else
              System.out.println( "unknown Callback: " + callback.getClass().getName() );
          }
        }

      } );
      // Durchführung des Logins
      loginContext.login();
      System.out.println( "authentication succeeded" );

      // Die Principals ermitteln...
      Set principals = loginContext.getSubject().getPrincipals();
      // ...und in einer Iteration ausgeben
      Iterator it = principals.iterator();
      Principal p;
      while( it.hasNext() )
      {
        p = (Principal) it.next();
        System.out.println( p );
      }
      System.out.println( "logging out..." );
      copy( new File( "D:/eclipse3.1/tmp/web_FlowsAdmitLogin.html" ) );

      loginContext.logout();
    }
    catch( Exception e )
    {
      System.out.println( "authentication failed" );
      throw e;
    }
  }

  private void copy( File file ) throws Exception
  {
    URL url = new URL( "http://bsu-srv-u04242/gruenplan/control?action=init" );
    FileWriter writer = new FileWriter( file );
    CopyUtils.copy( url.openStream(), writer );
    IOUtils.closeQuietly( writer );
  }
}
