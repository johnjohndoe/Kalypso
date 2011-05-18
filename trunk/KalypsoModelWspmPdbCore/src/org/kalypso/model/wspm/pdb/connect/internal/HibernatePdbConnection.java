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
package org.kalypso.model.wspm.pdb.connect.internal;

import java.util.List;

import org.hibernate.HibernateException;
import org.hibernate.SessionFactory;
import org.hibernate.Transaction;
import org.hibernate.cfg.Configuration;
import org.hibernate.classic.Session;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.db.PdbInfo;
import org.kalypso.model.wspm.pdb.db.PdbPoint;
import org.kalypso.model.wspm.pdb.db.PdbProperty;

/**
 * @author Gernot Belger
 */
public abstract class HibernatePdbConnection<SETTINGS extends HibernateSettings> implements IPdbConnection
{
  private final SETTINGS m_settings;

  private Configuration m_config;

  private Session m_session;

  public HibernatePdbConnection( final SETTINGS connectInfo )
  {
    m_settings = connectInfo;
  }

  @Override
  public String getLabel( )
  {
    return m_settings.getName();
  }

  protected SETTINGS getSettings( )
  {
    return m_settings;
  }

  private synchronized Configuration getConfiguration( )
  {
    if( m_config == null )
      m_config = createConfiguration();

    return m_config;
  }

  private Configuration createConfiguration( )
  {
    final Configuration configuration = new Configuration();

    doConfiguration( configuration );

    configure( configuration );

    configureMappings( configuration );

// final org.hibernate.dialect.PostgreSQLDialect dialect = new PostgisDialect();
// final String[] creationScripts = configuration.generateSchemaCreationScript( dialect );
// for( final String creationScript : creationScripts )
// System.out.println( creationScript );

    return configuration;
  }

  protected abstract void doConfiguration( Configuration configuration );

  private void configure( final Configuration configuration )
  {
    configuration.setProperty( "hibernate.order_updates", "true" );

    // FIXME: why does this not work???
    // configuration.setProperty( "hibernate.hbm2dll.auto", "create" );
    // configuration.setProperty( "org.hibernate.tool.hbm2ddl", "debug" );

    configuration.setProperty( "connection.pool_size", "1" );
    configuration.setProperty( "current_session_context_class", "thread" );
    configuration.setProperty( "cache.provider_class", "org.hibernate.cache.NoCacheProvider" );

    // TODO: via tracing
    configuration.setProperty( "show_sql", "true" );

// configuration.setProperty( "hibernate.c3p0.min_size", "5" );
// configuration.setProperty( "hibernate.c3p0.max_size", "20" );
// configuration.setProperty( "hibernate.c3p0.timeout", "1800" );
// configuration.setProperty( "hibernate.c3p0.max_statements", "50" );
  }

  private void configureMappings( final Configuration configuration )
  {
    configuration.addAnnotatedClass( PdbProperty.class );
// configuration.addAnnotatedClass( PdbPoint.class );
    configuration.addResource( "/org/kalypso/model/wspm/pdb/db/pdbpoint.xml" );
    // TODO: add all binding classes
  }

  @Override
  public void connect( ) throws PdbConnectException
  {
    if( isConnected() )
      return;

    // FIXME: use helper stuff here
    final ClassLoader classLoader = getClass().getClassLoader();
    final ClassLoader oldContextClassLoader = Thread.currentThread().getContextClassLoader();
    Thread.currentThread().setContextClassLoader( classLoader );
    try
    {
      final Configuration configuration = getConfiguration();

      final SessionFactory sessionFactory = configuration.buildSessionFactory();

      m_session = sessionFactory.openSession();
    }
    catch( final HibernateException e )
    {
      e.printStackTrace();
      throw new PdbConnectException( "Failed to open connection to PDB", e );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new PdbConnectException( "Failed to open connection to PDB", e );
    }
    finally
    {
// Thread.currentThread().setContextClassLoader( oldContextClassLoader );
    }
  }

  @Override
  public boolean isConnected( )
  {
    return m_session != null;
  }

  @Override
  public void close( ) throws PdbConnectException
  {
    try
    {
      m_session.close();
    }
    catch( final HibernateException e )
    {
      e.printStackTrace();
      throw new PdbConnectException( "Failed to close connection to PDB", e );
    }
    finally
    {
      m_session = null;
    }
  }

  private void checkConnection( ) throws PdbConnectException
  {
    if( !isConnected() || !m_session.isConnected() )
      throw new PdbConnectException( "PDB connection is not open" );
  }

  @Override
  public PdbInfo getInfo( ) throws PdbConnectException
  {
    checkConnection();

    // query all info's
    final Transaction transaction = m_session.beginTransaction();
    final String query = String.format( "from %s", PdbProperty.class.getName() );
    final List< ? > allInfo = m_session.createQuery( query ).list();
    transaction.commit();

    return new PdbInfo( allInfo );
  }

  @Override
  public void addPoint( final PdbPoint onePoint ) throws PdbConnectException
  {
    checkConnection();

    try
    {
      final Transaction transaction = m_session.beginTransaction();
      // TODO: transaction?
      m_session.save( onePoint );

      transaction.commit();
    }
    catch( final HibernateException e )
    {
      e.printStackTrace();
      throw new PdbConnectException( "Failed to write point to pdb.", e );
    }
  }
}