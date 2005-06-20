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
package org.kalypso.ui.application.login;

import org.kalypso.users.User;
import org.kalypso.users.UserServiceConstants;

/**
 * AppLoginValidator: last resort login without need for user service. Checks password again hardcoded one. Username is
 * not changeable.
 * 
 * @author schlienger
 */
public class AppLoginValidator implements ILoginValidator
{
  public User validate( final String username, final String password )
  {
    if( "hochwasser".equals( password ) )
      return new User( "Administrator", new String[]
      { UserServiceConstants.RIGHT_ADMIN } );

    return null;
  }

  public boolean userNameChangeable()
  {
    return false;
  }

  public boolean passwordEnabled()
  {
    return true;
  }

  public String getDefaultUserName()
  {
    return "Administrator";
  }

  public String getMessage()
  {
    return "Es konnten keine Benutzerrechte vom Server ermittelt werden.\n"
        + "Geben Sie das Administrator-Passwort ein, um im Administrator-Modus zu starten.";
  }
}