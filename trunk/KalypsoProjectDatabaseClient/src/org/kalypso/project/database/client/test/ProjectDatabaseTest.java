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
package org.kalypso.project.database.client.test;

import java.net.URL;

import org.apache.commons.vfs.FileObject;
import org.apache.commons.vfs.FileSystemManager;
import org.junit.Assert;
import org.junit.Test;
import org.kalypso.commons.io.VFSUtilities;
import org.kalypso.project.database.client.KalypsoProjectDatabaseClient;
import org.kalypso.project.database.common.interfaces.IProjectDatabaseAccess;
import org.kalypso.project.database.common.interfaces.implementation.KalypsoProjectBeanCreationDelegate;
import org.kalypso.project.database.sei.IProjectDatabase;
import org.kalypso.project.database.sei.beans.KalypsoProjectBean;

/**
 * @author kuch
 */
public class ProjectDatabaseTest
{
  private static final String PROJECT_TYPE = "PlanerClientProject";

  @Test
  public void testMethod( )
  {
    final IProjectDatabase service = KalypsoProjectDatabaseClient.getService();
    final String result = service.testMethod();
    System.out.println( result );
  }

  @Test
  public void testCreateProjects( )
  {
    for( int i = 0; i < 10; i++ )
    {
      createProject( "project_one", i );
      createProject( "project_two", i );
    }
  }

  public void createProject( final String name, final int version )
  {
    try
    {
      // copy project.zip to server incoming directory
      final URL project = ProjectDatabaseTest.class.getResource( "data/project.zip" );
      final FileSystemManager manager = VFSUtilities.getManager();
      final FileObject src = manager.resolveFile( project.toExternalForm() );

      final IProjectDatabaseAccess access = KalypsoProjectDatabaseClient.getDefault().getIncomingAccessData();
      final String url = access.getUrl( "test.zip" );

      final FileObject destination = manager.resolveFile( url );
      VFSUtilities.copy( src, destination );

      /* create project */
      final IProjectDatabase service = KalypsoProjectDatabaseClient.getService();

      final KalypsoProjectBeanCreationDelegate delegate = new KalypsoProjectBeanCreationDelegate( name, name, version, PROJECT_TYPE, url );
      final KalypsoProjectBean bean = service.createProject( delegate );
      Assert.assertNotNull( bean );

// KalypsoProjectBeanWrapper wrapper = new KalypsoProjectBeanWrapper( bean );
// FileObject dest = wrapper.getFileObject( access );
// Assert.assertTrue( dest.exists() );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  @Test
  public void testGetProjects( )
  {
    final IProjectDatabase service = KalypsoProjectDatabaseClient.getService();
    final KalypsoProjectBean[] projects = service.getProjectHeads( PROJECT_TYPE );

    for( final KalypsoProjectBean project : projects )
    {
      System.out.println( String.format( "Project: %s ", project.getName() ) );
    }
  }

}
