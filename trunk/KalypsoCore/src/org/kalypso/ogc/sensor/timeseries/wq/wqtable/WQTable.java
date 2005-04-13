package org.kalypso.ogc.sensor.timeseries.wq.wqtable;

import java.util.SortedSet;
import java.util.TreeSet;

import org.kalypso.util.math.LinearEquation;
import org.kalypso.util.math.LinearEquation.SameXValuesException;

/**
 * WQTable
 * 
 * @author schlienger
 */
public class WQTable
{
  private final TreeSet m_qSortedPairs;
  private final TreeSet m_wSortedPairs;

  //private final static WQTableException CANNOT_INTERPOLATE_EXCEPTION = new WQTableException( "Kann nicht interpolieren" );
  
  private final LinearEquation EQ = new LinearEquation();
  
  public WQTable( final double[][] table )
  {
    this( convert2pairs(table) );
  }
  
  public WQTable( final WQPair[] wqpairs )
  {
    m_qSortedPairs = new TreeSet( WQPairComparator.Q_COMPARATOR );
    m_wSortedPairs = new TreeSet( WQPairComparator.W_COMPARATOR );
    
    for( int i = 0; i < wqpairs.length; i++ )
    {
      m_qSortedPairs.add( wqpairs[i] );
      m_wSortedPairs.add( wqpairs[i] );
    }
  }

  public double getWFor( double q ) throws WQTableException
  {
    final WQPair p = new WQPair( 0, q );
    final SortedSet headSet = m_qSortedPairs.headSet( p );
    final SortedSet tailSet = m_qSortedPairs.tailSet( p );
    
    if( headSet.isEmpty() || tailSet.isEmpty() )
      throw new WQTableException();//CANNOT_INTERPOLATE_EXCEPTION; // TODO check if exception should be thrown or a value be returned
      
    final WQPair p1 = (WQPair) headSet.last();
    final WQPair p2 = (WQPair) tailSet.first();

    try
    {
      EQ.setPoints( p1.getW(), p1.getQ(), p2.getW(), p2.getQ() );
    }
    catch( SameXValuesException e )
    {
      throw new WQTableException( "Kann nicht interpolieren für Q= " + q, e );
    }
    
    return EQ.computeX( q );
  }

  public double getQFor( double w ) throws WQTableException
  {
    final WQPair p = new WQPair( w, 0 );
    final SortedSet headSet = m_wSortedPairs.headSet( p );
    final SortedSet tailSet = m_wSortedPairs.tailSet( p );
    
    if( headSet.isEmpty() || tailSet.isEmpty() )
      throw new WQTableException(); //CANNOT_INTERPOLATE_EXCEPTION; // TODO check if exception should be thrown or a value be returned

    final WQPair p1 = (WQPair) headSet.last();
    final WQPair p2 = (WQPair) tailSet.first();

    try
    {
      EQ.setPoints( p1.getW(), p1.getQ(), p2.getW(), p2.getQ() );
    }
    catch( SameXValuesException e )
    {
      throw new WQTableException( "Kann nicht interpolieren für W= " + w, e );
    }
    
    return EQ.computeY( w );
  }
  
  public static WQPair[] convert2pairs( final double[][] table )
  {
    final WQPair[] pairs = new WQPair[ table.length ];
    for( int i = 0; i < table.length; i++ )
      pairs[i] = new WQPair( table[i][0], table[i][1] );
    
    return pairs;
  }
}
