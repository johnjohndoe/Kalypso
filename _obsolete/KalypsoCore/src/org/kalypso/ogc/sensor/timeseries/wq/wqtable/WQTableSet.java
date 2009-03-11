package org.kalypso.ogc.sensor.timeseries.wq.wqtable;

import java.util.Date;
import java.util.SortedMap;
import java.util.TreeMap;

import org.kalypso.core.i18n.Messages;
import org.kalypso.ogc.sensor.timeseries.wq.IWQConverter;
import org.kalypso.ogc.sensor.timeseries.wq.WQException;

/**
 * WQTableSet
 * 
 * @author schlienger
 */
public class WQTableSet implements IWQConverter
{
  private final SortedMap<Date, WQTable> m_tables = new TreeMap<Date, WQTable>();

  private final String m_fromType;

  private final String m_toType;

  /**
   * Constructor
   * 
   * @param tables the WQ-Tables depending with validity information
   * @param fromType just used for information purposes
   * @param toType just used for information purposes
   */
  public WQTableSet( final WQTable[] tables, final String fromType, final String toType )
  {
    m_fromType = fromType;
    m_toType = toType;

    for( int i = 0; i < tables.length; i++ )
      m_tables.put( tables[i].getValidity(), tables[i] );
  }

  /**
   * @return a WQ-Table that is valid for the given date. This is the table such as: Validity( Table ) &lt;= date
   */
  public WQTable getFor( final Date date )
  {
    if( m_tables.size() == 0 )
      throw new IllegalStateException( Messages.getString("org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQTableSet.0") ); //$NON-NLS-1$

    final SortedMap<Date,WQTable> headSet = m_tables.headMap( date );

    final Date key;
    if( headSet.isEmpty() )
      key = m_tables.firstKey();
    else
      key = headSet.lastKey();

    return m_tables.get( key );
  }

  /**
   * @return Returns the fromType.
   */
  public String getFromType()
  {
    return m_fromType;
  }

  /**
   * @return Returns the toType.
   */
  public String getToType()
  {
    return m_toType;
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString()
  {
    return m_tables.toString();
  }

  /**
   * @return list of tables backed by this set; sorted by their validity
   */
  public WQTable[] getTables()
  {
    return m_tables.values().toArray( new WQTable[m_tables.size()] );
  }

  /**
   * @see org.kalypso.ogc.sensor.timeseries.wq.IWQConverter#computeW(java.util.Date, double)
   */
  public double computeW( final Date date, final double Q ) throws WQException
  {
    return getFor( date ).getWFor( Q );
  }

  /**
   * @see org.kalypso.ogc.sensor.timeseries.wq.IWQConverter#computeQ(java.util.Date, double)
   */
  public double computeQ( final Date date, final double W ) throws WQException
  {
    return getFor( date ).getQFor( W );
  }
}
