package org.kalypso.wiskiadapter.wiskicall;

import java.rmi.NoSuchObjectException;
import java.rmi.RemoteException;
import java.sql.Timestamp;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedList;

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

  private double[] W;

  private double[] Q;

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
    System.out.println(list);
    final HashMap table = (HashMap) list.getFirst();

    final String[] strW = ((String) table.get( "curve_table_stage" ))
        .split( "," );
    final String[] strQ = ((String) table.get( "curve_table_flow" ))
        .split( "," );

    if( strW.length != strQ.length )
      throw new IllegalArgumentException(
          "Anzahl von W-Werte und Q-Werte ist nicht gleich" );

    W = new double[strW.length];
    Q = new double[strW.length];
    for( int i = 0; i < strW.length; i++ )
    {
      W[i] = Double.parseDouble( strW[i] );
      Q[i] = Double.parseDouble( strQ[i] );
    }
  }

  public double[] getW( )
  {
    return W;
  }

  public double[] getQ( )
  {
    return Q;
  }
}
