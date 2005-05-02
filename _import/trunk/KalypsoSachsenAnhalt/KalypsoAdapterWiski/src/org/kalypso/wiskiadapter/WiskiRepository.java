package org.kalypso.wiskiadapter;

import java.rmi.Naming;
import java.rmi.NoSuchObjectException;
import java.rmi.RemoteException;
import java.util.HashMap;
import java.util.logging.Logger;

import org.kalypso.repository.AbstractRepository;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.repository.RepositoryException;
import org.kalypso.wiskiadapter.wiskicall.IWiskiCall;

import de.kisters.wiski.webdataprovider.common.net.KiWWDataProviderInterface;
import de.kisters.wiski.webdataprovider.common.util.KiWWException;
import de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf;

/**
 * WiskiRepository over the Wiski Data Provider (WDP).
 * 
 * @author schlienger
 */
public class WiskiRepository extends AbstractRepository
{
  /** expected number of items in the configuration string */
  private final static int CONF_NB_ITEMS = 5;

  /** separator of the configuration string */
  private final static String CONF_SEP = "#";

  private final static Logger LOG = Logger.getLogger( WiskiRepository.class
      .getName() );

  private KiWWDataProviderRMIf m_wiski = null;

  private final HashMap m_userData;

  private final String m_url;

  private final String m_domain;

  private final String m_logonName;

  private final String m_password;

  /**
   * Constructor
   * 
   * @param name
   * @param factory
   * @param conf
   *          the configuration should be build the followin way: URL # DOMAIN #
   *          LOGIN-NAME # PASSWORD # LANGUAGE
   * 
   * @param readOnly
   * @throws RepositoryException
   */
  public WiskiRepository( String name, String factory, String conf,
      boolean readOnly ) throws RepositoryException
  {
    super( name, factory, conf, readOnly );

    final String[] items = conf.split( CONF_SEP );

    if( items.length != CONF_NB_ITEMS )
      throw new RepositoryException( "Configuration should have "
          + CONF_NB_ITEMS + " items separated by " + CONF_SEP );

    m_url = items[0];
    m_domain = items[1];
    m_logonName = items[2];
    m_password = items[3];
    final String language = items[4];

    m_userData = new HashMap();
    m_userData.put( "domain", m_domain );
    m_userData.put( "logonName", m_logonName );
    m_userData.put( "password", m_password );
    m_userData.put( "language", language );

    m_wiski = wiskiInit();
  }

  /**
   * Perform initialisation of WISKI RMI proxy. Declared final since called from
   * constructor (good java practice)
   * 
   * @throws RepositoryException
   */
  private final KiWWDataProviderRMIf wiskiInit( ) throws RepositoryException
  {
    try
    {
      //create a server object
      final KiWWDataProviderRMIf myServerObject = (KiWWDataProviderRMIf) Naming
          .lookup( m_url );
      LOG.info( "Wiski About()=" + myServerObject.about() );

      // optional params (used for timeout, entry in seconds)
      // 28800 = 8 hours
      // 604800 = 1 week
      // 125798400 = 1 year
      final HashMap optParam = new HashMap();
      optParam.put(
          KiWWDataProviderInterface.OPT_GETUSERAUTHORISATION_MAXINACTIVE,
          Integer.toString( 28800 ) );

      final HashMap auth = myServerObject.getUserAuthorisation( m_domain,
          m_logonName, m_password, "myhost.kisters.de", optParam );
      LOG.info( "Wiski login=" + auth );

      if( auth == null
          || !"1"
              .equals( auth.get( KiWWDataProviderInterface.AUTHKEY_ALLOWED ) ) )
        throw new RepositoryException( "Login not allowed" );

      return myServerObject;
    }
    catch( Exception e )
    {
      throw new RepositoryException( e );
    }
  }

  /**
   * @see org.kalypso.repository.IRepository#dispose()
   */
  public void dispose( )
  {
    super.dispose();

    wiskiLogout();
  }

  private final void wiskiLogout( )
  {
    try
    {
      LOG.info( "Logging out from WISKI-WDP" );
      m_wiski.logout( m_userData, null );
    }
    catch( final Exception e ) // KiWWException, RemoteException
    {
      e.printStackTrace();
      LOG.info( e.getLocalizedMessage() );
    }
  }

  /**
   * @see org.kalypso.repository.IRepository#findItem(java.lang.String)
   */
  public IRepositoryItem findItem( String id ) throws RepositoryException
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.repository.IRepository#reload()
   */
  public void reload( ) throws RepositoryException
  {
    WiskiUtils.forcePropertiesReload();
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getIdentifier()
   */
  public String getIdentifier( )
  {
    return "wiski://";
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#hasChildren()
   */
  public boolean hasChildren( ) throws RepositoryException
  {
    return true;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getChildren()
   */
  public IRepositoryItem[] getChildren( ) throws RepositoryException
  {
    final String prop = WiskiUtils.getProperties().getProperty(
        WiskiUtils.PROP_SUPERGROUPNAMES );
    if( prop == null )
      throw new RepositoryException(
          "Gruppenliste in die Einstellungen (config.ini) nicht definiert" );

    final String[] superGroupNames = prop.split( "," );
    final IRepositoryItem[] supergroups = new IRepositoryItem[superGroupNames.length];
    for( int i = 0; i < superGroupNames.length; i++ )
      supergroups[i] = new SuperGroupItem( this, superGroupNames[i] );

    return supergroups;
  }

  public HashMap getUserData( )
  {
    return m_userData;
  }

  /**
   * Performs a call on the wiski remote object. This should be used in order to
   * allow automatic reloging in if the session has timed out.
   * 
   * @throws RemoteException
   * @throws KiWWException
   * @throws RepositoryException
   */
  public void executeWiskiCall( final IWiskiCall call ) throws RemoteException,
      KiWWException, RepositoryException
  {
    try
    {
      call.execute( m_wiski, m_userData );
    }
    catch( final NoSuchObjectException e )
    {
      // normally, if we get this exception, that means wiski has logged us
      // out. So we try here to reconnect and to perform the call again.
      wiskiLogout();

      wiskiInit();

      call.execute( m_wiski, m_userData );
    }
  }
}