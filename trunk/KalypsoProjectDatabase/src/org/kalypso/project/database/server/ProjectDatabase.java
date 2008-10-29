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
import java.util.Calendar;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import javax.jws.WebService;

import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.vfs.FileObject;
import org.apache.commons.vfs.FileSystemManager;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.Transaction;
import org.hibernate.cfg.AnnotationConfiguration;
import org.kalypso.commons.io.VFSUtilities;
import org.kalypso.project.database.IProjectDataBaseServerConstant;
import org.kalypso.project.database.common.utils.ProjectModelUrlResolver;
import org.kalypso.project.database.sei.IProjectDatabase;
import org.kalypso.project.database.sei.beans.KalypsoProjectBean;

/**
 * @author kuch
 */
@WebService(endpointInterface = "org.kalypso.project.database.sei.IProjectDatabase")
public class ProjectDatabase implements IProjectDatabase
{
  private SessionFactory FACTORY = null;

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
   * @see org.kalypso.project.database.sei.IProjectDatabase#getProjects()
   */
  @Override
  public KalypsoProjectBean[] getProjectHeads( final String projectType )
  {
    /** Getting the Session Factory and session */
    final Session session = FACTORY.getCurrentSession();

    /** Starting the Transaction */
    final Transaction tx = session.beginTransaction();

    /* names of existing projects */
    final List< ? > names = session.createQuery( String.format( "select m_unixName from KalypsoProjectBean where m_projectType = '%s' ORDER by m_name", projectType ) ).list();
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
      final List< ? > beans = mySession.createQuery( String.format( "from KalypsoProjectBean where m_unixName = '%s'  ORDER by m_projectVersion", project ) ).list();
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

      // TODO check needed? - order by clauses
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
   * @see org.kalypso.project.database.sei.IProjectDatabase#getProject()
   */
  @Override
  public KalypsoProjectBean getProject( final String projectUnixName )
  {
    /** Getting the Session Factory and session */
    final Session session = FACTORY.getCurrentSession();

    /** Starting the Transaction */
    final Transaction tx = session.beginTransaction();

    /* names of exsting projects */
    final List< ? > projects = session.createQuery( String.format( "from KalypsoProjectBean where m_unixName = '%s' ORDER by m_projectVersion desc", projectUnixName ) ).list();
    tx.commit();

    if( projects.size() <= 0 )
      return null;

    /* determine head */
    final KalypsoProjectBean head = (KalypsoProjectBean) projects.get( 0 );

    final List<KalypsoProjectBean> beans = new ArrayList<KalypsoProjectBean>();
    for( int i = 1; i < projects.size(); i++ )
    {
      beans.add( (KalypsoProjectBean) projects.get( i ) );
    }

    head.setChildren( beans.toArray( new KalypsoProjectBean[] {} ) );

    return head;
  }

  /**
   * @see org.kalypso.project.database.sei.IProjectDatabase#createProject(java.lang.String)
   */
  @Override
  public KalypsoProjectBean createProject( final KalypsoProjectBean bean, final URL incoming ) throws IOException
  {
    final FileSystemManager manager = VFSUtilities.getManager();
    final FileObject src = manager.resolveFile( incoming.toExternalForm() );

    try
    {
      if( !src.exists() )
        throw new FileNotFoundException( String.format( "Incoming file not exists: %s", incoming.toExternalForm() ) );

      /* destination of incoming file */
      final String urlDestination = ProjectModelUrlResolver.getUrlAsWebdav( new ProjectModelUrlResolver.IResolverInterface()
      {
        @Override
        public String getPath( )
        {
          return System.getProperty( IProjectDataBaseServerConstant.SERVER_WRITEABLE_PATH );
        }
      }, String.format( "%s/%d/project.zip", bean.getUnixName(), bean.getProjectVersion() ) );

      final FileObject destination = manager.resolveFile( urlDestination );

      VFSUtilities.copyFileTo( src, destination );

      /* store project bean in database */
      bean.setCreationDate( Calendar.getInstance().getTime() );

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
  }

  /**
   * @see org.kalypso.project.database.sei.IProjectDatabase#updateProject(java.lang.String)
   */
  @Override
  public KalypsoProjectBean udpateProject( final KalypsoProjectBean bean, final URL incoming ) throws IOException
  {
    /* get head */
    final KalypsoProjectBean head = getProject( bean.getUnixName() );
    bean.setProjectVersion( head.getProjectVersion() + 1 );

    return createProject( bean, incoming );
  }

  /**
   * @see org.kalypso.project.database.sei.IProjectDatabase#acquireProjectEditLock(org.kalypso.project.database.sei.beans.KalypsoProjectBean)
   */
  @Override
  public String acquireProjectEditLock( final String projectUnixName )
  {
    // TODO lock already acquired

    final Session mySession = FACTORY.getCurrentSession();
    final Transaction myTx = mySession.beginTransaction();

    final String ticket = String.format( "Ticket%d", Calendar.getInstance().getTime().hashCode() );
    final int updated = mySession.createQuery( String.format( "update KalypsoProjectBean set m_editLockTicket = '%s' where m_unixName = '%s'", ticket, projectUnixName ) ).executeUpdate();
    myTx.commit();

    if( updated == 0 )
      return null;

    final KalypsoProjectBean project = getProject( projectUnixName );
    if( !project.isProjectLockedForEditing() )
      throw new IllegalStateException( "Updating edit lock of projects failed." );

    final KalypsoProjectBean[] children = project.getChildren();
    for( final KalypsoProjectBean child : children )
    {
      if( !child.isProjectLockedForEditing() )
        throw new IllegalStateException( "Updating edit lock of projects failed." );
    }

    return ticket;
  }

  /**
   * @see org.kalypso.project.database.sei.IProjectDatabase#releaseProjectEditLock(java.lang.String, java.lang.String)
   */
  @Override
  public Boolean releaseProjectEditLock( final String projectUnixName, final String ticketId )
  {
    // TODO lock already released

    final Session mySession = FACTORY.getCurrentSession();
    final Transaction myTx = mySession.beginTransaction();

    mySession.createQuery( String.format( "update KalypsoProjectBean set m_editLockTicket = '' where m_unixName = '%s' and m_editLockTicket = '%s'", projectUnixName, ticketId ) ).executeUpdate();
    myTx.commit();

    final KalypsoProjectBean project = getProject( projectUnixName );
    if( project.isProjectLockedForEditing() )
      return false;

    final KalypsoProjectBean[] children = project.getChildren();
    for( final KalypsoProjectBean child : children )
    {
      if( child.isProjectLockedForEditing() )
        return false;
    }

    return true;
  }

  /**
   * @see org.kalypso.project.database.sei.IProjectDatabase#getProjectTypes()
   */
  @SuppressWarnings("unchecked")
  @Override
  public String[] getProjectTypes( )
  {
    /** Getting the Session Factory and session */
    final Session session = FACTORY.getCurrentSession();

    /** Starting the Transaction */
    final Transaction tx = session.beginTransaction();

    /* list of project types */
    final List<String> projects = session.createQuery( "Select distinct m_projectType from KalypsoProjectBean ORDER by m_projectType" ).list();
    tx.commit();

    return projects.toArray( new String[] {} );
  }

  /**
   * @see org.kalypso.project.database.sei.IProjectDatabase#getProjectHeads()
   */
  @Override
  public KalypsoProjectBean[] getAllProjectHeads( )
  {
    final Set<KalypsoProjectBean> myBeans = new TreeSet<KalypsoProjectBean>();

    final String[] types = getProjectTypes();
    for( final String type : types )
    {
      final KalypsoProjectBean[] beans = getProjectHeads( type );
      for( final KalypsoProjectBean bean : beans )
      {
        myBeans.add( bean );
      }
    }

    return myBeans.toArray( new KalypsoProjectBean[] {} );
  }

  /**
   * @see org.kalypso.project.database.sei.IProjectDatabase#ping()
   */
  @Override
  public Boolean ping( )
  {
    return Boolean.TRUE;
  }
}
