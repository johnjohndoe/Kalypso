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

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.net.URL;
import java.rmi.RemoteException;
import java.util.Iterator;
import java.util.Properties;
import java.util.Set;
import java.util.logging.FileHandler;
import java.util.logging.Logger;

import org.apache.commons.io.IOUtils;
import org.kalypso.commons.java.net.UrlResolverSingleton;
import org.kalypso.contribs.java.lang.reflect.ClassUtilities;
import org.kalypso.services.common.ServiceConfig;
import org.kalypso.services.user.IUserService;
import org.kalypso.users.IUserRightsProvider;
import org.kalypso.users.UserRightsException;

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

  private static final String PROP_SCENARIO_LIST = "SCENARIO_LIST";

  private static final String PROP_SCENARIO_DESC_LIST = "SCENARIO_DESC_LIST";

  private static final String PROP_CHOOSE_SCENARIO = "CHOOSE_SCENARIO";

  private boolean m_impersonateUser;

  private boolean m_chooseScenario;

  private String[] m_scenarios;

  private String[] m_scenarioDescriptions;

  public KalypsoUserService() throws RemoteException
  {
    try
    {
      m_logger.addHandler( new FileHandler( ServiceConfig.getTempDir() + "/IUserService%g.log", 10000000, 1, true ) );
    }
    catch( Exception e ) // generic Exception caught for simplicity
    {
      e.printStackTrace();
      System.out.println( "Logger für User-Service konnte nicht erzeugt werden" );
    }

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
    final File conf = new File( ServiceConfig.getConfDir(), "IUserService/userService.properties" );

    InputStream stream = null;
    try
    {
      stream = new FileInputStream( conf );

      final Properties props = new Properties();
      props.load( stream );

      // step through properties and 
      final Set keys = props.keySet();
      for( final Iterator it = keys.iterator(); it.hasNext(); )
      {
        final String key = (String)it.next();
        if( key.endsWith( "URL" ) )
        {
          final String path = props.getProperty( key );
          final URL resolved = UrlResolverSingleton.resolveUrl( conf.toURL() , path);
          final String fullPath = resolved.toExternalForm();

          props.setProperty( key, fullPath );
        }
      }

      // try to instanciate our commiter
      final String className = props.getProperty( PROP_PROVIDER );
      m_rightsProvider = (IUserRightsProvider)ClassUtilities.newInstance( className, IUserRightsProvider.class,
          getClass().getClassLoader() );
      m_rightsProvider.init( props );

      final String iu = props.getProperty( PROP_IMPERSONATE_USER, "false" );
      m_impersonateUser = Boolean.valueOf( iu ).booleanValue();
      final String cs = props.getProperty( PROP_CHOOSE_SCENARIO, "false" );
      m_chooseScenario = Boolean.valueOf( cs ).booleanValue();
      final String sl = props.getProperty( PROP_SCENARIO_LIST, "" );
      m_scenarios = sl.split( ";" );
      final String sdl = props.getProperty( PROP_SCENARIO_DESC_LIST, "" );
      m_scenarioDescriptions = sdl.split( ";" );
    }
    catch( final Exception e ) // generic exception caught for simplicity
    {
      m_logger.throwing( "KalypsoUserService", "init", e );

      throw new RemoteException( "Exception in init()", e );
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
   * @see org.kalypso.services.user.IUserService#getRights(java.lang.String)
   */
  public String[] getRights( final String username ) throws RemoteException
  {
    if( m_rightsProvider == null )
      return null;

    try
    {
      return m_rightsProvider.getRights( username );
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
   * @see org.kalypso.services.user.IUserService#getRights(java.lang.String, java.lang.String)
   */
  public String[] getRights( String username, String password ) throws RemoteException
  {
    if( m_rightsProvider == null )
      return null;

    try
    {
      return m_rightsProvider.getRights( username, password );
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
  public String[] getScenarios()
  {
    return m_scenarios;
  }

  /**
   * @see org.kalypso.services.user.IUserService#getScenarioDescriptions()
   */
  public String[] getScenarioDescriptions()
  {
    return m_scenarioDescriptions;
  }
}