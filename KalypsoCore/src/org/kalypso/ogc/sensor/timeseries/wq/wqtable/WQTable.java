package org.kalypso.ogc.sensor.timeseries.wq.wqtable;

import java.text.DateFormat;
import java.util.Date;
import java.util.SortedSet;
import java.util.TreeSet;

import org.kalypso.commons.math.LinearEquation;
import org.kalypso.commons.math.LinearEquation.SameXValuesException;
import org.kalypso.ogc.sensor.timeseries.wq.WQException;

/**
 * WQTable
 * 
 * @author schlienger
 */
public class WQTable
{
  private final TreeSet m_qSortedPairs;

  private final TreeSet m_wSortedPairs;

  private final static WQException CANNOT_INTERPOLATE_EXCEPTION = new WQException( "Kann nicht interpolieren" );

  private final LinearEquation EQ = new LinearEquation();

  private final Date m_validity;

  private int m_offset;

  /**
   * Creates a WQTable with a default offset of 0
   * 
   * @param validity
   *          date up from which this table is valid
   * @param table
   */
  public WQTable( final Date validity, final double[][] table )
  {
    this( validity, 0, table );
  }

  /**
   * Creates a WQTable
   * 
   * @param validity
   *          date up from which this table is valid
   * @param offset
   *          offset used for W, before conversion W = W + offset
   * @param table
   */
  public WQTable( final Date validity, final int offset, final double[][] table )
  {
    this( validity, offset, WQPair.convert2pairs( table ) );
  }

  /**
   * Creates a WQTable with a default offset of 0
   * 
   * @param validity
   *          date up from which this table is valid
   */
  public WQTable( final Date validity, final double[] W, final double[] Q )
  {
    this( validity, 0, W, Q );
  }

  /**
   * Creates a WQTable
   * 
   * @param validity
   *          date up from which this table is valid
   * @param offset
   *          offset used for W, before conversion W = W + offset
   * @param W
   * @param Q
   */
  public WQTable( final Date validity, final int offset, final double[] W, final double[] Q )
  {
    this( validity, offset, WQPair.convert2pairs( W, Q ) );
  }

  /**
   * Creates a WQTable with a default offset of 0
   * 
   * @param validity
   *          date up from which this table is valid
   */
  public WQTable( final Date validity, final Number[] W, final Number[] Q )
  {
    this( validity, 0, W, Q );
  }

  /**
   * Creates a WQTable
   * 
   * @param validity
   *          date up from which this table is valid
   * @param offset
   *          offset used for W, before conversion W = W + offset
   * @param W
   * @param Q
   */
  public WQTable( final Date validity, final int offset, final Number[] W, final Number[] Q )
  {
    this( validity, offset, WQPair.convert2pairs( W, Q ) );
  }

  /**
   * Creates a WQTable
   * 
   * @param validity
   *          date up from which this table is valid
   * @param offset
   *          offset used for W, before conversion W = W + offset
   * @param wqpairs
   */
  public WQTable( final Date validity, final int offset, final WQPair[] wqpairs )
  {
    m_validity = validity;
    m_offset = offset;

    m_qSortedPairs = new TreeSet( WQPairComparator.Q_COMPARATOR );
    m_wSortedPairs = new TreeSet( WQPairComparator.W_COMPARATOR );

    for( int i = 0; i < wqpairs.length; i++ )
    {
      m_qSortedPairs.add( wqpairs[i] );
      m_wSortedPairs.add( wqpairs[i] );
    }
  }

  public double getWFor( double q ) throws WQException
  {
    final WQPair p = new WQPair( 0, q );
    final SortedSet headSet = m_qSortedPairs.headSet( p );
    final SortedSet tailSet = m_qSortedPairs.tailSet( p );

    if( headSet.isEmpty() || tailSet.isEmpty() )
      throw CANNOT_INTERPOLATE_EXCEPTION; // TODO check if exception should be
    // thrown or a value be returned

    final WQPair p1 = (WQPair)headSet.last();
    final WQPair p2 = (WQPair)tailSet.first();

    try
    {
      EQ.setPoints( p1.getW(), p1.getQ(), p2.getW(), p2.getQ() );
    }
    catch( SameXValuesException e )
    {
      throw new WQException( "Kann nicht interpolieren für Q= " + q, e );
    }

    return EQ.computeX( q );
  }

  public double getQFor( double w ) throws WQException
  {
    final WQPair p = new WQPair( w, 0 );
    final SortedSet headSet = m_wSortedPairs.headSet( p );
    final SortedSet tailSet = m_wSortedPairs.tailSet( p );

    if( headSet.isEmpty() || tailSet.isEmpty() )
      throw CANNOT_INTERPOLATE_EXCEPTION; // TODO check if exception should be
    // thrown or a value be returned

    final WQPair p1 = (WQPair)headSet.last();
    final WQPair p2 = (WQPair)tailSet.first();

    try
    {
      EQ.setPoints( p1.getW(), p1.getQ(), p2.getW(), p2.getQ() );
    }
    catch( SameXValuesException e )
    {
      throw new WQException( "Kann nicht interpolieren für W= " + w, e );
    }

    return EQ.computeY( w );
  }

  public Date getValidity()
  {
    return m_validity;
  }

  public int getOffset()
  {
    return m_offset;
  }

  public void setOffset( int offset )
  {
    m_offset = offset;
  }

  /**
   * @see java.lang.Object#toString()
   */
  public String toString()
  {
    final StringBuffer sb = new StringBuffer();

    final DateFormat df = DateFormat.getDateTimeInstance();
    sb.append( "Gültigkeit: " ).append( df.format( m_validity ) ).append( " Offset: " ).append( m_offset );//.append( "\n" ).append(
    //    m_wSortedPairs ).append( "\n" );

    return sb.toString();
  }

  public WQPair[] getPairs()
  {
    return (WQPair[])m_wSortedPairs.toArray( new WQPair[m_wSortedPairs.size()] );
  }
}
