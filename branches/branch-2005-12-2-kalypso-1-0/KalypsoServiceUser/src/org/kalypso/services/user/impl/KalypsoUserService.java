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
package org.kalypso.services.user.impl;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.rmi.RemoteException;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.commons.io.IOUtils;
import org.kalypso.commons.java.net.UrlResolverSingleton;
import org.kalypso.contribs.java.lang.reflect.ClassUtilities;
import org.kalypso.services.common.ServiceConfig;
import org.kalypso.services.user.IUserRightsProvider;
import org.kalypso.services.user.IUserService;
import org.kalypso.services.user.ScenarioBean;
import org.kalypso.services.user.UserRightsException;

/**
 * User Rights Service.
 * <p>
 * 17.12.2004 - schlienger - removed the dependency to PSICompact by using the concept of a configurable
 * IUserRightsProvider. It is configured in the service properties.
 * </p>
 * 
 * @author belger
 */
public class KalypsoUserService implements IUserService
{
  private IUserRightsProvider m_rightsProvider = null;

  private final Logger m_logger = Logger.getLogger( KalypsoUserService.class.getName() );

  /**
   * name of the property that contains the classname of the instance of IUserRigthsProvider
   */
  private static final String PROP_PROVIDER = "PROVIDER";

  private static final String PROP_IMPERSONATE_USER = "IMPERSONATE_USER";

  private static final String PROP_SCENARIO_ID_LIST = "SCENARIO_ID_LIST";
  private static final String PROP_SCENARIO_INI_LIST = "SCENARIO_INI_LIST";

  private static final String PROP_CHOOSE_SCENARIO = "CHOOSE_SCENARIO";

  private boolean m_impersonateUser;

  private boolean m_chooseScenario;

  private ScenarioBean[] m_scenarios;

  public KalypsoUserService() throws RemoteException
  {
    m_logger.info( "Initialisiere UserService" );

    init();
  }

  /**
   * initialize this service
   * 
   * @throws RemoteException
   */
  private final void init() throws RemoteException
  {
    InputStream stream = null;
    try
    {
      final URL confLocation = ServiceConfig.getConfLocation();
      final URL confUrl = UrlResolverSingleton.resolveUrl( confLocation, "IUserService/userService.properties" );
      stream = confUrl.openStream();

      final Properties props = new Properties();
      props.load( stream );

      // step through properties and
      UrlResolverSingleton.resolveUrlProperties( confUrl, props );

      // try to instanciate our commiter
      final String className = props.getProperty( PROP_PROVIDER );
      m_rightsProvider = (IUserRightsProvider)ClassUtilities.newInstance( className, IUserRightsProvider.class,
          getClass().getClassLoader() );
      m_rightsProvider.init( props );

      final String iu = props.getProperty( PROP_IMPERSONATE_USER, "false" );
      m_impersonateUser = Boolean.valueOf( iu ).booleanValue();
      final String cs = props.getProperty( PROP_CHOOSE_SCENARIO, "false" );
      m_chooseScenario = Boolean.valueOf( cs ).booleanValue();

      final String sid = props.getProperty( PROP_SCENARIO_ID_LIST, "" );
      final String[] scenarioIds = sid.split( ";" );
      final String sini = props.getProperty( PROP_SCENARIO_INI_LIST, "" );
      final String[] scenarioInis = sini.split( ";" );

      if( scenarioIds.length == scenarioInis.length )
      {
        m_scenarios = new ScenarioBean[scenarioIds.length];

        for( int i = 0; i < m_scenarios.length; i++ )
        {
          final String id = scenarioIds[i];
          final String ini = scenarioInis[i];

          final Properties iniProperties = new Properties();
          if( ini != "" )
          {
            InputStream iniStream = null;
            try
            {
              final URL iniUrl = new URL( confUrl, ini );
              iniStream = iniUrl.openStream();
              iniProperties.load( iniStream );
              iniStream.close();

              UrlResolverSingleton.resolveUrlProperties( iniUrl, iniProperties );
            }
            catch( final IOException e )
            {
              m_logger.log( Level.WARNING, "Konnte ini Datei für Szenario nicht laden: " + id, e );
              IOUtils.closeQuietly( iniStream );
            }
          }

          m_scenarios[i] = new ScenarioBean( id, iniProperties );
        }
      }
      else
        throw new IllegalArgumentException( "Szenario Listen unterschiedlich lang" );
    }
    catch( final Exception e ) // generic exception caught for simplicity
    {
      m_logger.throwing( "KalypsoUserService", "init", e );

      throw new RemoteException( "Fehler bei der Initialisierung", e );
    }
    finally
    {
      IOUtils.closeQuietly( stream );
    }
  }

  /**
   * @see java.lang.Object#finalize()
   */
  protected void finalize() throws Throwable
  {
    if( m_rightsProvider != null )
      m_rightsProvider.dispose();
  }

  /**
   * @see org.kalypso.services.user.IUserService#getRights(java.lang.String, java.lang.String)
   */
  public String[] getRights( final String username, final String currentScenarioId ) throws RemoteException
  {
    if( m_rightsProvider == null )
      return null;

    try
    {
      return m_rightsProvider.getRights( username, currentScenarioId );
    }
    catch( final UserRightsException e )
    {
      m_logger.info( e.getLocalizedMessage() );

      throw new RemoteException( "Exception in getRights()", e );
    }
  }

  /**
   * @see org.kalypso.services.IKalypsoService#getServiceVersion()
   */
  public int getServiceVersion()
  {
    return 0;
  }

  /**
   * @see org.kalypso.services.user.IUserService#getRights(java.lang.String, java.lang.String, java.lang.String)
   */
  public String[] getRights( final String username, final String password, final String currentScenarioId ) throws RemoteException
  {
    if( m_rightsProvider == null )
      return null;

    try
    {
      return m_rightsProvider.getRights( username, password, currentScenarioId );
    }
    catch( final UserRightsException e )
    {
      m_logger.info( e.getLocalizedMessage() );

      throw new RemoteException( "Exception in getRights()", e );
    }

  }

  /**
   * @see org.kalypso.services.user.IUserService#isAskForLogin()
   */
  public boolean isAskForLogin()
  {
    return m_impersonateUser;
  }

  /**
   * @see org.kalypso.services.user.IUserService#isAskForScenario()
   */
  public boolean isAskForScenario()
  {
    return m_chooseScenario;
  }

  /**
   * @see org.kalypso.services.user.IUserService#getScenarios()
   */
  public ScenarioBean[] getScenarios()
  {
    return m_scenarios;
  }
}