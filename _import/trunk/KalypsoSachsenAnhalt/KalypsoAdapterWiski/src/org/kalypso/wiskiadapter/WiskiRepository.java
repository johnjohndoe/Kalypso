package org.kalypso.wiskiadapter;

import java.rmi.Naming;
import java.util.HashMap;
import java.util.logging.Logger;

import org.kalypso.repository.AbstractRepository;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.repository.RepositoryException;

import de.kisters.tsmsystem.common.data.SimpleRequestSortTerm;
import de.kisters.wiski.webdataprovider.common.net.KiWWDataProviderInterface;
import de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf;

/**
 * WiskiRepository
 * 
 * @author schlienger
 */
public class WiskiRepository extends AbstractRepository
{
  private final static int CONF_NB_ITEMS = 5;

  private final static String CONF_SEP = "#";

  private String m_url;

  private String m_domain;

  private String m_logonName;

  private String m_password;

  private String m_language;

  private final static Logger LOG = Logger.getLogger( WiskiRepository.class
      .getName() );

  private KiWWDataProviderRMIf m_wiski;

  private HashMap m_userData;

  /**
   * @param name
   * @param factory
   * @param conf
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
    m_language = items[4];

    m_wiski = init();
  }

  /**
   * Perform initialisation of WISKI RMI proxy
   * 
   * @return
   * 
   * @throws RepositoryException
   */
  private KiWWDataProviderRMIf init( ) throws RepositoryException
  {
    try
    {
//      BCEHelper.configureProxy( "172.16.0.253", "8080", "schlienger",
//          "lba7cfi8" );

      //create a server object
      final KiWWDataProviderRMIf myServerObject = (KiWWDataProviderRMIf) Naming
          .lookup( m_url );
      LOG.info( "Wiski About()=" + myServerObject.about() );

      m_userData = new HashMap();
      m_userData.put( "domain", m_domain );
      m_userData.put( "logonName", m_logonName );
      m_userData.put( "password", m_password );
      m_userData.put( "language", m_language );

      final HashMap auth = myServerObject.getUserAuthorisation( m_domain,
          m_logonName, m_password, "myhost.kisters.de", null );
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
   * @see java.lang.Object#finalize()
   */
  protected void finalize( ) throws Throwable
  {
    m_wiski.logout( m_userData, null );

    super.finalize();
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
    // TODO Auto-generated method stub

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
    // TODO Auto-generated method stub
    return false;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getChildren()
   */
  public IRepositoryItem[] getChildren( ) throws RepositoryException
  {
    SimpleRequestSortTerm sort = new SimpleRequestSortTerm();
    sort.addColumnAscent( "station_name" );

    //m_wiski.getRiverList( )
    //m_wiski.getStationList( m_userData, new String[] { "station_no",
      //  "station_name", "station_id" }, new String[] );

    // TODO Auto-generated method stub
    return null;
  }
}