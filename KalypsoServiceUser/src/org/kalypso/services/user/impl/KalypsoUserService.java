package org.kalypso.services.user.impl;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.rmi.RemoteException;
import java.util.Properties;
import java.util.logging.FileHandler;
import java.util.logging.Logger;

import org.apache.commons.io.IOUtils;
import org.kalypso.java.lang.reflect.ClassUtilities;
import org.kalypso.services.common.ServiceConfig;
import org.kalypso.services.user.IUserService;
import org.kalypso.users.IUserRightsProvider;
import org.kalypso.users.UserRightsException;

/**
 * User Rights Service.
 * <p>
 * 17.12.2004 - schlienger - removed the dependency to PSICompact by using the
 * concept of a configurable IUserRightsProvider. It is configured in the
 * service properties.
 * </p>
 * 
 * @author belger
 */
public class KalypsoUserService implements IUserService
{
  private IUserRightsProvider m_rightsProvider = null;

  private final Logger m_logger = Logger.getLogger( KalypsoUserService.class
      .getName() );

  /**
   * name of the property that contains the classname of the instance of
   * IUserRigthsProvider
   */
  private static final String PROP_PROVIDER = "PROVIDER";

  public KalypsoUserService( ) throws RemoteException
  {
    try
    {
      m_logger.addHandler( new FileHandler( ServiceConfig.getTempDir()
          + "/IUserService%g.log", 10000000, 1, true ) );
    }
    catch( Exception e ) // generic Exception caught for simplicity
    {
      e.printStackTrace();
      System.out
          .println( "Logger für User-Service konnte nicht erzeugt werden" );
    }

    m_logger.info( "Initialisiere UserService" );

    init();
  }

  /**
   * initialize this service
   * 
   * @throws RemoteException
   */
  private void init( ) throws RemoteException
  {
    final File conf = new File( ServiceConfig.getConfDir(),
        "IUserService/userService.properties" );

    InputStream stream = null;
    try
    {
      stream = new FileInputStream( conf );

      final Properties props = new Properties();
      props.load( stream );

      // try to instanciate our commiter
      final String className = props.getProperty( PROP_PROVIDER );
      m_rightsProvider = (IUserRightsProvider) ClassUtilities.newInstance(
          className, IUserRightsProvider.class, getClass().getClassLoader() );
    }
    catch( Exception e ) // generic exception caught for simplicity
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
    catch( UserRightsException e )
    {
      e.printStackTrace();

      throw new RemoteException( "Exception in getRights()", e );
    }
  }

  /**
   * @see org.kalypso.services.IKalypsoService#getServiceVersion()
   */
  public int getServiceVersion( )
  {
    return 0;
  }
}