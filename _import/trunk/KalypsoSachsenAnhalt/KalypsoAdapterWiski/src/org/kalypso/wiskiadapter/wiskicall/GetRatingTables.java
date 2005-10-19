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

  private Number[] m_stage;

  private Number[] m_flow;

  public GetRatingTables( final Long id, final Date validity )
  {
    m_id = id;
    m_validity = validity;
  }

  public void execute( KiWWDataProviderRMIf wiski, HashMap userData ) throws NoSuchObjectException, KiWWException,
      RemoteException
  {
    final HashMap tables = wiski.getRatingTables( userData, KiWWDataProviderInterface.OBJECT_TIMESERIES, new Long[]
    { m_id }, new Timestamp( m_validity.getTime() ) );

    final LinkedList list = (LinkedList)tables.get( m_id );
    if( list.size() > 0 )
    {
      final HashMap table = (HashMap)list.getFirst();

      m_stage = (Number[])( (List)table.get( "curve_table_stage" ) ).toArray( new Number[0] );
      m_flow = (Number[])( (List)table.get( "curve_table_flow" ) ).toArray( new Number[0] );

      if( m_stage.length != m_flow.length )
        throw new IllegalArgumentException( "Anzahl von W-Werte und Q-Werte ist nicht gleich" );
    }
  }

  public Number[] getStage()
  {
    return m_stage;
  }

  public Number[] getFlow()
  {
    return m_flow;
  }

  public boolean hasTable()
  {
    return m_stage != null;
  }
}
