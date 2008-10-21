/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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
package org.kalypso.project.database.sei;

import java.io.IOException;

import javax.jws.WebService;

import org.kalypso.project.database.common.interfaces.implementation.KalypsoProjectBeanCreationDelegate;
import org.kalypso.project.database.sei.beans.KalypsoProjectBean;

/**
 * Facade of the ProjectDatabase
 * 
 * @author Dirk Kuch
 */
@WebService
public interface IProjectDatabase
{

  /**
   * @param projectType
   *          of {@link KalypsoProjectBean}
   * @return list of existing projects (head versions of projects - contains a list of subprojects (versions of one
   *         project)
   */
  KalypsoProjectBean[] getProjectHeads( String projectType );

  /**
   * @param url
   *          location of incoming project zip
   * @param name
   *          of project
   * @return newly created project
   */
  KalypsoProjectBean createProject( KalypsoProjectBeanCreationDelegate delegate ) throws IOException;

  String testMethod( );
}
