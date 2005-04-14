package org.kalypso.wiskiadapter;

import java.rmi.Naming;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Logger;

import org.kalypso.repository.AbstractRepository;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.repository.RepositoryException;

import de.kisters.tsmsystem.common.data.SimpleRequestFilterTerm;
import de.kisters.tsmsystem.common.data.SimpleRequestSortTerm;
import de.kisters.wiski.webdataprovider.common.net.KiWWDataProviderInterface;
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

  /** interesting column names for querying groups */
  private static final String[] COLUMNS_GROUP = { "group_id", "group_name" };

  private final static Logger LOG = Logger.getLogger( WiskiRepository.class
      .getName() );

  private String m_url;

  private String m_domain;

  private String m_logonName;

  private String m_password;

  private String m_language;

  private KiWWDataProviderRMIf m_wiski;

  private HashMap m_userData;

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
    m_language = items[4];

    m_wiski = init();
  }

  /**
   * Perform initialisation of WISKI RMI proxy. Declared final since called from
   * constructor (good java practice)
   * 
   * @throws RepositoryException
   */
  private final KiWWDataProviderRMIf init( ) throws RepositoryException
  {
    try
    {
      //      BCEHelper.configureProxy( "172.16.0.253", "8080", "schlienger",
      //          "..." );

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
        WiskiUtils.PROP_GROUP_NAMES );
    if( prop == null )
      throw new RepositoryException(
          "Gruppenliste in die Einstellungen (config.ini) nicht definiert" );

    final String[] groupNames = prop.split( "," );

    final SimpleRequestFilterTerm filtergroup = new SimpleRequestFilterTerm();
    for( int i = 0; i < groupNames.length; i++ )
    {
      if( i > 0 )
        filtergroup.addOperator( "and" );

      filtergroup.addColumnReference( "group_name" );
      filtergroup.addOperator( "like" );
      filtergroup.addValue( groupNames[i] + "%" );
    }

    final SimpleRequestSortTerm sort = new SimpleRequestSortTerm();
    sort.addColumnAscent( "group_name" );

    try
    {
      final HashMap grouplist = m_wiski.getGroupList( m_userData,
          COLUMNS_GROUP, KiWWDataProviderInterface.TIMESERIES_GROUP, sort,
          filtergroup, 15, 0, false, null );

      final List resultList = (List) grouplist
          .get( KiWWDataProviderInterface.KEY_RESULT_LIST );

      final GroupItem[] groups = new GroupItem[resultList.size()];
      int i = 0;
      for( final Iterator it = resultList.iterator(); it.hasNext(); )
      {
        final HashMap map = (HashMap) it.next();
        groups[i++] = new GroupItem( this, (String) map.get( "group_id" ),
            (String) map.get( "group_name" ) );
      }

      return groups;
    }
    catch( Exception e ) // RemoteException or KiWWException
    {
      e.printStackTrace();
      throw new RepositoryException( e );
    }
  }

  public HashMap getUserData( )
  {
    return m_userData;
  }

  public KiWWDataProviderRMIf getWiski( )
  {
    return m_wiski;
  }
}