package org.kalypso.wiskiadapter.wiskicall;

import java.rmi.NoSuchObjectException;
import java.rmi.RemoteException;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Logger;

import org.kalypso.contribs.java.util.DoubleComparator;
import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQTable;

import de.kisters.wiski.webdataprovider.common.util.KiWWException;
import de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf;

/**
 * GetRatingTables
 * <p>
 * the rating-table is cleaned once the values were retrieved from the WDP:
 * <ul>
 * <li>all invalid values are removed (-777)
 * <li>the flow must be increasing continuously (flyback values are ignored)
 * </ul>
 * 
 * @author schlienger
 */
public class GetRatingTables implements IWiskiCall
{
  private final static Logger LOG = Logger.getLogger( GetRatingTables.class.getName() );

  private final Long m_id;

  private final Date m_validity;

  private final String m_wiskiObjectType;

  private final Date m_wqTableValidity;

  private LinkedList m_tables;

  /**
   * Constructor
   * 
   * @param id
   *          either id of timeserie, parameter or station, depending on the value of wiskiObjectType
   * @param validity
   * @param wiskiObjectType
   *          one of KiWWDataProviderInterface.OBJECT_*
   * @param wqTableValidity
   *          The validity for which the wq-Tabel will be created
   */
  public GetRatingTables( final Long id, final Date validity, final String wiskiObjectType, final Date wqTableValidity )
  {
    m_id = id;
    m_validity = validity;
    m_wiskiObjectType = wiskiObjectType;
    m_wqTableValidity = wqTableValidity;
  }

  public void execute( final KiWWDataProviderRMIf wiski, final HashMap userData ) throws NoSuchObjectException, KiWWException,
      RemoteException
  {
    final HashMap tables = wiski.getRatingTables( userData, m_wiskiObjectType, new Long[]
    { m_id }, new Timestamp( m_validity.getTime() ) );

    m_tables = (LinkedList) tables.get( m_id );
  }

  /**
   * Converts the wiski call into a wq-table
   * 
   * @throws IllegalStateException
   * @throws IllegalArgumentException
   */
  private static WQTable convertTable( final HashMap table, final Date wqTableValidity ) throws IllegalStateException, IllegalArgumentException
  {
    final Number[] stage = (Number[])( (List)table.get( "curve_table_stage" ) ).toArray( new Number[0] );
    final Number[] flow = (Number[])( (List)table.get( "curve_table_flow" ) ).toArray( new Number[0] );

    if( stage.length != flow.length )
      throw new IllegalStateException( "stage and flow arrays are not of the same length" );

    // jetzt schlechte Werte (-777) herausfiltern
    // wir testen auch ob flow immer stetig steigend ist
    final List goodStage = new ArrayList( stage.length );
    final List goodFlow = new ArrayList( stage.length );

    final DoubleComparator dc = new DoubleComparator( 0.000001 );
    final Double errValue = new Double( -777 );

    Number stetigFlow = null;

    final StringBuffer buf = new StringBuffer();

    for( int i = 0; i < flow.length; i++ )
    {
      if( dc.compare( errValue, stage[i] ) == 0 || dc.compare( errValue, flow[i] ) == 0 )
      {
        buf.append( "Ignoring Rating-Table Tuple #" + i + " [" + stage[i] + ", " + flow[i] + "]\n" );
        continue;
      }

      if( stetigFlow != null && dc.compare( stetigFlow, flow[i] ) >= 0 )
      {
        buf.append( "Ignoring Rating-Table Tuple #" + i + " [" + stage[i] + ", " + flow[i]
            + "] due to flyback (Rücksprung)\n" );
        continue;
      }

      stetigFlow = flow[i];

      goodStage.add( stage[i] );
      goodFlow.add( flow[i] );
    }

    if( buf.length() > 0 )
      LOG.info( "Rating-Table analysis found following warnings:\n" + buf.toString() );

    final Number[] theStage = (Number[])goodStage.toArray( new Number[goodStage.size()] );
    final Number[] theFlow = (Number[])goodFlow.toArray( new Number[goodFlow.size()] );

    if( theStage.length != theFlow.length )
      throw new IllegalArgumentException( "Anzahl von W-Werte und Q-Werte ist nicht gleich" );

    return new WQTable( wqTableValidity, theStage, theFlow );
  }

  /** Returns the fetched wqTable, or null if none was found. */
  public WQTable getTable()
  {
    if( m_tables.size() == 0 )
      return null;

    final HashMap table = (HashMap) m_tables.getFirst();
    return convertTable( table, m_wqTableValidity );
  }
}
