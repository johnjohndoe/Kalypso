/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.project.database.client.test;

import org.junit.Assert;
import org.junit.Test;
import org.kalypso.project.database.client.KalypsoProjectDatabaseClient;
import org.kalypso.project.database.sei.IProjectDatabase;
import org.kalypso.project.database.sei.beans.KalypsoProjectBean;

/**
 * UnitTest for acquiring and releasing a project edit lock
 * 
 * @author Dirk Kuch
 */
public class TestAcquireReleaseEditLock
{
  private static final String PROJECT_TYPE = "PlanerClientProject";

  private static String TICKET = null;

  private static String PROJECT_NAME = null;

  @Test
  public void testAcquireLock( )
  {
    final IProjectDatabase service = KalypsoProjectDatabaseClient.getService();
    final KalypsoProjectBean[] projects = service.getProjectHeads( PROJECT_TYPE );
    Assert.assertTrue( projects.length > 0 );

    PROJECT_NAME = projects[0].getUnixName();
    Assert.assertNotNull( PROJECT_NAME );
    TICKET = service.acquireProjectEditLock( projects[0].getUnixName() );
    Assert.assertNotNull( TICKET );
  }

  @Test
  public void testReleaseLock( )
  {
    Assert.assertNotNull( PROJECT_NAME );
    Assert.assertNotNull( TICKET );
    final IProjectDatabase service = KalypsoProjectDatabaseClient.getService();

    final Boolean released = service.releaseProjectEditLock( PROJECT_NAME, TICKET );
    Assert.assertTrue( released );
  }

}
