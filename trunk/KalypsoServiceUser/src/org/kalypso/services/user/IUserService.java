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
package org.kalypso.services.user;

import org.kalypso.services.IKalypsoService;

/**
 * IUserService
 * 
 * @author belger
 */
public interface IUserService extends IKalypsoService
{
  /**
   * Returns the rights of the given user
   * 
   * @param username
   *          name of the user who has already authenticated himself against the operating system
   * 
   * @return list of rights
   */
  public String[] getRights( final String username ) throws UserRightsException;

  /**
   * Returns the rights of the given user
   * 
   * @param username
   *          name of the user who is willing to authenticate himself. He might have logged in the operating system, but
   *          must still log into some other entity.
   * @param password
   *          TODO encode password, make this call secure
   * @return list of rights
   */
  public String[] getRightsWithAuth( final String username, final String password ) throws UserRightsException;

  /**
   * @return whether user should be asked to enter its login information or not
   */
  public boolean isAskForLogin() throws UserRightsException;

  /**
   * @return whether user should be asked for scenario or not
   */
  public boolean isAskForScenario() throws UserRightsException;

  /**
   * @return list of scenarios that must be managed by clients
   */
  public ScenarioBean[] getScenarios() throws UserRightsException;
}
