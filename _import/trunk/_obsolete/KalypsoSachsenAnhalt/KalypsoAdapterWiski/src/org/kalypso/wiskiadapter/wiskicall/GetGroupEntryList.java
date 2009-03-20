package org.kalypso.wiskiadapter.wiskicall;

import java.rmi.NoSuchObjectException;
import java.rmi.RemoteException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import de.kisters.wiski.webdataprovider.common.net.KiWWDataProviderInterface;
import de.kisters.wiski.webdataprovider.common.util.KiWWException;
import de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf;

/**
 * getGroupEntryList Wiski Call
 * <p>
 * Liefert Metainformation über Gruppenarten Einträge
 * <p>
 * 
 * @author Gernot Belger
 */
public class GetGroupEntryList implements IWiskiCall
{
  private final long m_groupId;

  /** id:String -> active:Boolean */
  private final Map m_activeMap = new HashMap();

  /**
   * Constructor with groupId. All tsInfoList objects in the group will be fetched in the wiski database.
   */
  public GetGroupEntryList( final String groupId )
  {
    m_groupId = Long.valueOf( groupId ).longValue();
  }

  public void execute( final KiWWDataProviderRMIf wiski, final HashMap userData ) throws NoSuchObjectException,
      KiWWException, RemoteException
  {
    final List bl = wiski.getGroupEntryList( userData, KiWWDataProviderInterface.TIMESERIES_GROUP, m_groupId, null );
    for( final Iterator iter = bl.iterator(); iter.hasNext(); )
    {
      final HashMap element = (HashMap)iter.next();
      final String idStr = (String)element.get( "id" );
      final String activeStr = (String)element.get( "isactive" );
      final boolean active = activeStr.length() > 0 && activeStr.charAt( 0 ) == '1';
      m_activeMap.put( idStr, Boolean.valueOf( active ) );
    }
  }

  public boolean isActive( final String id )
  {
    if( m_activeMap.containsKey( id ) )
      return ( (Boolean)m_activeMap.get( id ) ).booleanValue();

    /* In doubt, return true. */
    return true;
  }
}
