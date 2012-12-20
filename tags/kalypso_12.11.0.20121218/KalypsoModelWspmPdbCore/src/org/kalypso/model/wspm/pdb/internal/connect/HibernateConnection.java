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
package org.kalypso.model.wspm.pdb.internal.connect;

import org.hibernate.HibernateException;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.cfg.Configuration;
import org.hibernate.cfg.Environment;
import org.hibernatespatial.GeometryUserType2;
import org.hibernatespatial.HBSpatialExtension;
import org.hibernatespatial.SpatialDialect;
import org.kalypso.contribs.eclipse.core.runtime.ThreadContextClassLoaderRunnable;
import org.kalypso.model.wspm.pdb.PdbUtils;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.PDBRole;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.db.PdbInfo;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSection;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSectionPart;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSectionPartParameter;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSectionPartType;
import org.kalypso.model.wspm.pdb.db.mapping.DhmIndex;
import org.kalypso.model.wspm.pdb.db.mapping.Document;
import org.kalypso.model.wspm.pdb.db.mapping.Event;
import org.kalypso.model.wspm.pdb.db.mapping.Info;
import org.kalypso.model.wspm.pdb.db.mapping.Point;
import org.kalypso.model.wspm.pdb.db.mapping.PointKind;
import org.kalypso.model.wspm.pdb.db.mapping.Roughness;
import org.kalypso.model.wspm.pdb.db.mapping.RoughnessId;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.db.mapping.Style;
import org.kalypso.model.wspm.pdb.db.mapping.StyleArray;
import org.kalypso.model.wspm.pdb.db.mapping.StyleParameter;
import org.kalypso.model.wspm.pdb.db.mapping.Vegetation;
import org.kalypso.model.wspm.pdb.db.mapping.VegetationId;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.db.mapping.WaterlevelFixation;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCoreDebug;
import org.kalypso.model.wspm.pdb.internal.i18n.Messages;

/**
 * @author Gernot Belger
 */
public abstract class HibernateConnection<SETTINGS extends HibernateSettings> implements IPdbConnection
{
  private static final String SPATIAL_DIALECT = "hibernate.spatial.dialect"; //$NON-NLS-1$

  private PdbInfo m_info = null;

  private final SETTINGS m_settings;

  private Configuration m_config;

  private SessionFactory m_sessionFactory;

  private PDBRole m_role;

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
    // cfg.setProperty( org.hibernate.cfg.Environment.HBM2DDL_AUTO, "create" );
    // configuration.setProperty( "org.hibernate.tool.hbm2ddl", "debug" );

    configuration.setProperty( Environment.POOL_SIZE, "1" ); //$NON-NLS-1$
    configuration.setProperty( Environment.CURRENT_SESSION_CONTEXT_CLASS, "thread" ); //$NON-NLS-1$
    configuration.setProperty( Environment.CACHE_PROVIDER, "org.hibernate.cache.NoCacheProvider" ); //$NON-NLS-1$

    if( WspmPdbCoreDebug.SHOW_SQL_STATEMENTS.isEnabled() )
    {
      configuration.setProperty( Environment.SHOW_SQL, Boolean.TRUE.toString() );
      configuration.setProperty( Environment.FORMAT_SQL, Boolean.TRUE.toString() );
    }
    else
    {
      configuration.setProperty( Environment.SHOW_SQL, Boolean.FALSE.toString() );
      configuration.setProperty( Environment.FORMAT_SQL, Boolean.FALSE.toString() );
    }

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
    configuration.addAnnotatedClass( StyleArray.class );
    configuration.addAnnotatedClass( Style.class );
    configuration.addAnnotatedClass( StyleParameter.class );
    configuration.addAnnotatedClass( CrossSectionPartType.class );
    configuration.addAnnotatedClass( CrossSectionPart.class );
    configuration.addAnnotatedClass( CrossSectionPartParameter.class );
    configuration.addAnnotatedClass( Point.class );
    configuration.addAnnotatedClass( PointKind.class );
    configuration.addAnnotatedClass( Roughness.class );
    configuration.addAnnotatedClass( RoughnessId.class );
    configuration.addAnnotatedClass( Vegetation.class );
    configuration.addAnnotatedClass( VegetationId.class );
    configuration.addAnnotatedClass( WaterlevelFixation.class );
    configuration.addAnnotatedClass( Document.class );
    configuration.addAnnotatedClass( DhmIndex.class );
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

        // final org.hibernate.dialect.PostgreSQLDialect dialect = new PostgisDialect();
        // final String[] creationScript = configuration.generateSchemaCreationScript( dialect );
        // for( final String sql : creationScript )
        // System.out.println( sql );

        final SessionFactory sessionFactory = configuration.buildSessionFactory();
        setSessionFactory( sessionFactory );
      }
    };

    try
    {
      operation.run();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new PdbConnectException( Messages.getString( "HibernateConnection.0" ), e ); //$NON-NLS-1$
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
      throw new PdbConnectException( Messages.getString( "HibernateConnection.1" ), e ); //$NON-NLS-1$
    }
    finally
    {
      m_sessionFactory = null;
    }
  }

  @Override
  public Session openSession( ) throws PdbConnectException
  {
    if( !isConnected() )
      throw new PdbConnectException( Messages.getString( "HibernateConnection.2" ) ); //$NON-NLS-1$

    try
    {
      return m_sessionFactory.openSession();
    }
    catch( final HibernateException e )
    {
      e.printStackTrace();
      throw new PdbConnectException( Messages.getString( "HibernateConnection.3" ), e ); //$NON-NLS-1$
    }
  }

  @Override
  public PdbInfo getInfo( )
  {
    if( m_info == null )
      loadInfo();

    return m_info;
  }

  @Override
  public PDBRole getRole( )
  {
    if( m_role == null )
      loadInfo();

    return m_role;
  }

  @Override
  public void updateInfo( )
  {
    loadInfo();
  }

  private void loadInfo( )
  {
    Session session = null;
    try
    {
      session = openSession();
      m_info = new PdbInfo( session );
      m_role = readRole( session );
    }
    catch( final PdbConnectException e )
    {
      e.printStackTrace();
    }
    finally
    {
      PdbUtils.closeSessionQuietly( session );
    }
  }

  protected abstract PDBRole readRole( final Session session );
}