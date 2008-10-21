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
package org.kalypso.project.database.server;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TreeMap;

import javax.jws.WebService;

import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.vfs.FileObject;
import org.apache.commons.vfs.FileSystemManager;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.Transaction;
import org.hibernate.cfg.AnnotationConfiguration;
import org.kalypso.commons.io.VFSUtilities;
import org.kalypso.project.database.common.interfaces.implementation.KalypsoProjectBeanCreationDelegate;
import org.kalypso.project.database.sei.IProjectDatabase;
import org.kalypso.project.database.sei.beans.KalypsoProjectBean;

/**
 * @author kuch
 */
@WebService(endpointInterface = "org.kalypso.project.database.sei.IProjectDatabase")
public class ProjectDatabase implements IProjectDatabase
{
  private SessionFactory FACTORY = null;

  private final String BASE_PROJECT_URL = "webdav://planer:client@localhost:8888/webdav/projects/";

  public ProjectDatabase( )
  {
    final URL url = this.getClass().getResource( "conf/hibernate.cfg.xml" );
    final AnnotationConfiguration configure = new AnnotationConfiguration().configure( url );

    configure.addAnnotatedClass( KalypsoProjectBean.class );
    FACTORY = configure.buildSessionFactory();
  }

  public void dispose( )
  {
    if( FACTORY != null )
    {
      FACTORY.close();
    }

  }

  /**
   * @see org.kalypso.project.database.sei.IProjectDatabase#testMethod()
   */
  @Override
  public String testMethod( )
  {
// /* test storing and reading of projects */
// /** Getting the Session Factory and session */
//
// Session session = FACTORY.getCurrentSession();
//
// /** Starting the Transaction */
// Transaction tx = session.beginTransaction();
//
// KalypsoProjectBean project = new KalypsoProjectBean( "TestProject" );
//
// /** Saving POJO */
// session.save( project );
//
// /** Commiting the changes */
// tx.commit();

    return "blub";
  }

  /**
   * @see org.kalypso.project.database.sei.IProjectDatabase#getProjects()
   */
  @Override
  public KalypsoProjectBean[] getProjectHeads( final String projectType )
  {
    /** Getting the Session Factory and session */
    final Session session = FACTORY.getCurrentSession();

    /** Starting the Transaction */
    final Transaction tx = session.beginTransaction();

    /* names of exsting projects */
    final List< ? > names = session.createQuery( String.format( "select m_name from KalypsoProjectBean where m_projectType = '%s'", projectType ) ).list();
    tx.commit();

    final Set<String> projects = new HashSet<String>();

    for( final Object object : names )
    {
      if( !(object instanceof String) )
        continue;

      final String name = object.toString();
      projects.add( name );
    }

    final List<KalypsoProjectBean> projectBeans = new ArrayList<KalypsoProjectBean>();

    for( final String project : projects )
    {
      final TreeMap<Integer, KalypsoProjectBean> myBeans = new TreeMap<Integer, KalypsoProjectBean>();

      final Session mySession = FACTORY.getCurrentSession();
      final Transaction myTx = mySession.beginTransaction();
      final List< ? > beans = mySession.createQuery( String.format( "from KalypsoProjectBean where m_name = '%s'", project ) ).list();
      myTx.commit();

      for( final Object object : beans )
      {
        if( !(object instanceof KalypsoProjectBean) )
          continue;

        final KalypsoProjectBean b = (KalypsoProjectBean) object;
        myBeans.put( b.getProjectVersion(), b );
      }

      final Integer[] keys = myBeans.keySet().toArray( new Integer[] {} );

      /* determine head */
      final KalypsoProjectBean head = myBeans.get( keys[keys.length - 1] );

      KalypsoProjectBean[] values = myBeans.values().toArray( new KalypsoProjectBean[] {} );
      values = (KalypsoProjectBean[]) ArrayUtils.remove( values, values.length - 1 ); // remove last entry -> cycle!

      Arrays.sort( values, new Comparator<KalypsoProjectBean>()
      {
        @Override
        public int compare( final KalypsoProjectBean o1, final KalypsoProjectBean o2 )
        {
          return o1.getProjectVersion().compareTo( o2.getProjectVersion() );
        }
      } );

      head.setChildren( values );
      projectBeans.add( head );
    }

    return projectBeans.toArray( new KalypsoProjectBean[] {} );
  }

  /**
   * @see org.kalypso.project.database.sei.IProjectDatabase#createProject(java.lang.String)
   */
  @Override
  public KalypsoProjectBean createProject( final KalypsoProjectBeanCreationDelegate delegate ) throws IOException
  {
    final FileSystemManager manager = VFSUtilities.getManager();
    final FileObject src = manager.resolveFile( delegate.getIncomingUrl() );

    try
    {
      if( !src.exists() )
        throw new FileNotFoundException( String.format( "Incoming file not exists: %s", delegate.getIncomingUrl() ) );

      /* destination of incoming file */
      final String urlDestination = String.format( "%s%s/%d/project.zip", BASE_PROJECT_URL, delegate.getUnixName(), delegate.getVersion() );
      final FileObject destination = manager.resolveFile( urlDestination );

      VFSUtilities.copyFileTo( src, destination );

      final KalypsoProjectBean bean = new KalypsoProjectBean( delegate );

      /* store project bean in database */
      final Session session = FACTORY.getCurrentSession();
      final Transaction tx = session.beginTransaction();
      session.save( bean );

      tx.commit();

      return bean;
    }
    catch( final Exception e )
    {
      throw new IOException( e.getMessage() );
    }
    finally
    {
      src.delete();
    }
  }
}
