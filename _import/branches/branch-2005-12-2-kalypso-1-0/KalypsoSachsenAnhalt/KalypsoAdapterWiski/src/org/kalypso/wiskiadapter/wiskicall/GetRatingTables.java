package org.kalypso.wiskiadapter.wiskicall;

import java.rmi.NoSuchObjectException;
import java.rmi.RemoteException;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;

import org.kalypso.contribs.java.util.DoubleComparator;

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

  private final String m_wiskiObjectType;

  /**
   * Constructor
   * 
   * @param id
   *          either id of timeserie, parameter or station, depending on the value of wiskiObjectType
   * @param validity
   * @param wiskiObjectType
   *          one of KiWWDataProviderInterface.OBJECT_*
   */
  public GetRatingTables( final Long id, final Date validity, final String wiskiObjectType )
  {
    m_id = id;
    m_validity = validity;
    m_wiskiObjectType = wiskiObjectType;
  }

  public void execute( KiWWDataProviderRMIf wiski, HashMap userData ) throws NoSuchObjectException, KiWWException,
      RemoteException
  {
    final HashMap tables = wiski.getRatingTables( userData, m_wiskiObjectType, new Long[]
    { m_id }, new Timestamp( m_validity.getTime() ) );

    final LinkedList list = (LinkedList)tables.get( m_id );
    if( list.size() > 0 )
    {
      final HashMap table = (HashMap)list.getFirst();

      final Number[] stage = (Number[])( (List)table.get( "curve_table_stage" ) ).toArray( new Number[0] );
      final Number[] flow = (Number[])( (List)table.get( "curve_table_flow" ) ).toArray( new Number[0] );

      if( stage.length != flow.length )
        throw new IllegalStateException( "stage and flow arrays are not of the same length" );

      // jetzt schlechte Werte (-777) herausfiltern
      final List goodStage = new ArrayList( stage.length );
      final List goodFlow = new ArrayList( stage.length );

      final DoubleComparator dc = new DoubleComparator( 0.001 );
      final Double errValue = new Double( -777 );

      for( int i = 0; i < flow.length; i++ )
      {
        if( dc.compare( errValue, stage[i] ) == 0 || dc.compare( errValue, flow[i] ) == 0 )
          continue;
        
        goodStage.add( stage[i] );
        goodFlow.add( flow[i] );
      }

      m_stage = (Number[])goodStage.toArray( new Number[goodStage.size()] );
      m_flow = (Number[])goodFlow.toArray( new Number[goodFlow.size()] );

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
