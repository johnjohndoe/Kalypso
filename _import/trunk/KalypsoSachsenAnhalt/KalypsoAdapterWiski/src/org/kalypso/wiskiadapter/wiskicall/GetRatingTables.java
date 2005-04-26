package org.kalypso.wiskiadapter.wiskicall;

import java.rmi.NoSuchObjectException;
import java.rmi.RemoteException;
import java.sql.Timestamp;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;

import de.kisters.wiski.webdataprovider.common.net.KiWWDataProviderInterface;
import de.kisters.wiski.webdataprovider.common.util.KiWWException;
import de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf;

/**
 * GetRatingTables
 * 
 * @author schlienger
 */
public class GetRatingTables implements IWiskiCall
{
  private final Long m_id;

  private final Date m_validity;

  private Number[] W;

  private Number[] Q;

  public GetRatingTables( final Long id, final Date validity )
  {
    m_id = id;
    m_validity = validity;
  }

  public void execute( KiWWDataProviderRMIf wiski, HashMap userData )
      throws NoSuchObjectException, KiWWException, RemoteException
  {
    final HashMap tables = wiski.getRatingTables( userData,
        KiWWDataProviderInterface.OBJECT_TIMESERIES, new Long[] { m_id },
        new Timestamp( m_validity.getTime() ) );

    final LinkedList list = (LinkedList) tables.get( m_id );
    System.out.println( "ID:" + m_id + " - " + list );
    if( list.size() > 0 )
    {
      final HashMap table = (HashMap) list.getFirst();

      W = (Number[])((List) table.get( "curve_table_stage" )).toArray( new Number[0]);
      Q = (Number[]) ((List) table.get( "curve_table_flow" )).toArray( new Number[0]);

      if( W.length != Q.length )
        throw new IllegalArgumentException(
            "Anzahl von W-Werte und Q-Werte ist nicht gleich" );
    }
  }

  public Number[] getW( )
  {
    return W;
  }

  public Number[] getQ( )
  {
    return Q;
  }
  
  public boolean hasTable()
  {
    return W != null;
  }
}
