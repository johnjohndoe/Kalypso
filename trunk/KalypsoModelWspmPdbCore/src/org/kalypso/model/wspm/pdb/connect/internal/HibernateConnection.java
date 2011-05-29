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
package org.kalypso.model.wspm.pdb.connect.internal;

import java.util.List;

import org.hibernate.HibernateException;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.cfg.Configuration;
import org.hibernate.cfg.Environment;
import org.hibernatespatial.GeometryUserType2;
import org.hibernatespatial.HBSpatialExtension;
import org.hibernatespatial.SpatialDialect;
import org.hibernatespatial.postgis.PostgisDialect;
import org.kalypso.contribs.eclipse.core.runtime.ThreadContextClassLoaderRunnable;
import org.kalypso.model.wspm.pdb.PdbUtils;
import org.kalypso.model.wspm.pdb.connect.Executor;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.connect.command.ListOperation;
import org.kalypso.model.wspm.pdb.db.PdbInfo;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSection;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSectionPart;
import org.kalypso.model.wspm.pdb.db.mapping.Event;
import org.kalypso.model.wspm.pdb.db.mapping.Info;
import org.kalypso.model.wspm.pdb.db.mapping.Point;
import org.kalypso.model.wspm.pdb.db.mapping.PointKind;
import org.kalypso.model.wspm.pdb.db.mapping.Roughness;
import org.kalypso.model.wspm.pdb.db.mapping.RoughnessId;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.db.mapping.Vegetation;
import org.kalypso.model.wspm.pdb.db.mapping.VegetationId;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.db.mapping.WaterlevelFixation;

/**
 * @author Gernot Belger
 */
public abstract class HibernateConnection<SETTINGS extends HibernateSettings> implements IPdbConnection
{
  protected static final String SPATIAL_DIALECT = "hibernate.spatial.dialect"; //$NON-NLS-1$

  private final SETTINGS m_settings;

  private Configuration m_config;

  private SessionFactory m_sessionFactory;

  public HibernateConnection( final SETTINGS connectInfo )
  {
    m_settings = connectInfo;
  }

  @Override
  public String getLabel( )
  {
    return m_settings.getName();
  }

  @Override
  public SETTINGS getSettings( )
  {
    return m_settings;
  }

  synchronized Configuration getConfiguration( )
  {
    if( m_config == null )
      m_config = createConfiguration();

    return m_config;
  }

  private Configuration createConfiguration( )
  {
    final Configuration configuration = new Configuration();

    doConfiguration( configuration );

    configureSpatial( configuration );

    configure( configuration );

    configureMappings( configuration );

    return configuration;
  }

  protected abstract void doConfiguration( Configuration configuration );

  protected abstract SpatialDialect createSpatialDialect( );

  private void configureSpatial( final Configuration configuration )
  {
    /**
     * IMPORTANT: statically initialize HBSpatialExtension at this place (with the right context class loader active, so
     * the pseudo DialectProvider will be found; we will get a NPE else later.
     */
    HBSpatialExtension.getDefaultGeomFactory();

    /**
     * Important: we need to specify the spatial dialect ourself, else hibernatespatial will fall back to a default
     * spatial dialect.
     */
    final SpatialDialect spatialDialect = createSpatialDialect();
    final GeometryUserType2 geometryUserType = new GeometryUserType2( spatialDialect );
    configuration.registerTypeOverride( geometryUserType, new String[] { geometryUserType.getClass().getName() } );

    configuration.setProperty( Environment.DIALECT, spatialDialect.getClass().getName() );
    configuration.setProperty( SPATIAL_DIALECT, spatialDialect.getClass().getName() );
  }

  private void configure( final Configuration configuration )
  {
    configuration.setProperty( Environment.ORDER_UPDATES, Boolean.TRUE.toString() );

    // FIXME: why does this not work???
    // configuration.setProperty( "hibernate.hbm2dll.auto", "create" );
    // configuration.setProperty( "org.hibernate.tool.hbm2ddl", "debug" );

    configuration.setProperty( Environment.POOL_SIZE, "1" );
    configuration.setProperty( Environment.CURRENT_SESSION_CONTEXT_CLASS, "thread" );
    configuration.setProperty( Environment.CACHE_PROVIDER, "org.hibernate.cache.NoCacheProvider" );

    // TODO: via tracing
    configuration.setProperty( Environment.SHOW_SQL, Boolean.FALSE.toString() );
    configuration.setProperty( Environment.FORMAT_SQL, Boolean.FALSE.toString() );

// configuration.setProperty( "hibernate.c3p0.min_size", "5" );
// configuration.setProperty( "hibernate.c3p0.max_size", "20" );
// configuration.setProperty( "hibernate.c3p0.timeout", "1800" );
// configuration.setProperty( "hibernate.c3p0.max_statements", "50" );
  }

  private void configureMappings( final Configuration configuration )
  {
    configuration.addPackage( Info.class.getPackage().getName() );

    configuration.addAnnotatedClass( Info.class );
    configuration.addAnnotatedClass( WaterBody.class );
    configuration.addAnnotatedClass( State.class );
    configuration.addAnnotatedClass( Event.class );
    configuration.addAnnotatedClass( CrossSection.class );
    configuration.addAnnotatedClass( CrossSectionPart.class );
    configuration.addAnnotatedClass( Point.class );
    configuration.addAnnotatedClass( PointKind.class );
    configuration.addAnnotatedClass( Roughness.class );
    configuration.addAnnotatedClass( RoughnessId.class );
    configuration.addAnnotatedClass( Vegetation.class );
    configuration.addAnnotatedClass( VegetationId.class );
    configuration.addAnnotatedClass( WaterlevelFixation.class );
  }

  @Override
  public void connect( ) throws PdbConnectException
  {
    if( isConnected() )
      return;

    final ClassLoader classLoader = getClass().getClassLoader();
    final ThreadContextClassLoaderRunnable operation = new ThreadContextClassLoaderRunnable( classLoader )
    {
      @Override
      protected void runWithContextClassLoader( ) throws Exception
      {
        final Configuration configuration = getConfiguration();

        final org.hibernate.dialect.PostgreSQLDialect dialect = new PostgisDialect();
        final String[] creationScript = configuration.generateSchemaCreationScript( dialect );
        for( final String sql : creationScript )
          System.out.println( sql );

        final SessionFactory sessionFactory = configuration.buildSessionFactory();
        setSessionFactory( sessionFactory );
      }
    };

    try
    {
      operation.run();
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
  }

  protected void setSessionFactory( final SessionFactory sessionFactory )
  {
    m_sessionFactory = sessionFactory;
  }

  @Override
  public boolean isConnected( )
  {
    return m_sessionFactory != null;
  }

  @Override
  public void close( ) throws PdbConnectException
  {
    try
    {
      if( m_sessionFactory != null )
        m_sessionFactory.close();
    }
    catch( final HibernateException e )
    {
      e.printStackTrace();
      throw new PdbConnectException( "Failed to close connection to PDB", e );
    }
    finally
    {
      m_sessionFactory = null;
    }
  }

// private void checkConnection( ) throws PdbConnectException
// {
// if( !isConnected() )
// throw new PdbConnectException( "PDB connection is not open" );
// }

  @Override
  public PdbInfo getInfo( ) throws PdbConnectException
  {
    final Session session = openSession();
    try
    {
      final ListOperation<Info> operation = new ListOperation<Info>( Info.class );
      new Executor( session, operation ).execute();
      final List<Info> properties = operation.getList();
      return new PdbInfo( properties );
    }
    finally
    {
      PdbUtils.closeSessionQuietly( session );
    }
  }

// private void addObject( final Object object ) throws PdbConnectException
// {
// final IPdbOperation operation = new AddObjectOperation( object );
// executeCommand( operation );
// }

// @Override
// public void addPoint( final Points onePoint ) throws PdbConnectException
// {
// addObject( onePoint );
// }

// @Override
// public List<WaterBodies> getWaterBodies( ) throws PdbConnectException
// {
// return getList( WaterBodies.class );
// }

// @Override
// public void addWaterBody( final WaterBodies waterBody ) throws PdbConnectException
// {
// checkConnection();
//
// addObject( waterBody );
// }

// @Override
// public void addState( final States state ) throws PdbConnectException
// {
// final Date now = new Date();
// state.setCreationDate( now );
// state.setEditingDate( now );
// state.setEditingUser( getSettings().getUsername() );
//
// checkConnection();
//
// addObject( state );
// }

// @Override
// public void addCrossSection( final CrossSections crossSection ) throws PdbConnectException
// {
// checkConnection();
//
// addObject( crossSection );
// }

// @Override
// public List<States> getStates( ) throws PdbConnectException
// {
// return getList( States.class );
// }

// @Override
// public void addCrossSectionPart( final CrossSectionParts csPart ) throws PdbConnectException
// {
// addObject( csPart );
// }

  @Override
  public Session openSession( ) throws PdbConnectException
  {
    if( !isConnected() )
      throw new PdbConnectException( "PDB not connected" );

    try
    {
      return m_sessionFactory.openSession();
    }
    catch( final HibernateException e )
    {
      e.printStackTrace();
      throw new PdbConnectException( "Failed to open db session", e );
    }
  }
}