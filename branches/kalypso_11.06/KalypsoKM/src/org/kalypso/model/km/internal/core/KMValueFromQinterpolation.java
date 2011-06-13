package org.kalypso.model.km.internal.core;

import java.util.SortedSet;

import org.kalypso.commons.math.LinearEquation;
import org.kalypso.commons.math.LinearEquation.SameXValuesException;

/**
 * {@link IKMValue} derived by linear interpolation over q between two adjacent {@link IKMValue}.
 */
class KMValueFromQinterpolation extends AbstractKMValue
{
  private final double m_qLowerChannel;

  private final double m_qUpperChannel;

  private final double m_qLowerForeland;

  private final double m_qUpperForeland;

  private final double m_aplha;

  private final double m_length;

  private final NKValue m_nkValue;

  private final NKValue m_nkValueForeland;

  private KMValueFromQinterpolation( final double lowerQ, final IKMValue km1, final IKMValue km2 ) throws SameXValuesException
  {
    m_length = km1.getLength();

    final double lowerQ1 = km1.getLowerQ();
    final double lowerQ2 = km2.getLowerQ();

    final LinearEquation qLowerChannel = new LinearEquation( lowerQ1, km1.getLowerQchannel(), lowerQ2, km2.getLowerQchannel() );
    m_qLowerChannel = qLowerChannel.computeY( lowerQ );

    final LinearEquation qUpperChannel = new LinearEquation( lowerQ1, km1.getUpperQchannel(), lowerQ2, km2.getUpperQchannel() );
    m_qUpperChannel = qUpperChannel.computeY( lowerQ );

    final LinearEquation qLowerForeland = new LinearEquation( lowerQ1, km1.getLowerQforeland(), lowerQ2, km2.getLowerQforeland() );
    m_qLowerForeland = qLowerForeland.computeY( lowerQ );

    final LinearEquation qUpperForeland = new LinearEquation( lowerQ1, km1.getUpperQforeland(), lowerQ2, km2.getUpperQforeland() );
    m_qUpperForeland = qUpperForeland.computeY( lowerQ );

    final LinearEquation alpha = new LinearEquation( lowerQ1, km1.getAlpha(), lowerQ2, km2.getAlpha() );
    m_aplha = alpha.computeY( lowerQ );

    final LinearEquation kEq = new LinearEquation( lowerQ1, km1.getK(), lowerQ2, km2.getK() );
    final LinearEquation nEq = new LinearEquation( lowerQ1, km1.getN(), lowerQ2, km2.getN() );

    final double k = kEq.computeY( lowerQ );
    final double n = nEq.computeY( lowerQ );

    final LinearEquation kfEQ = new LinearEquation( lowerQ1, km1.getKForeland(), lowerQ2, km2.getKForeland() );
    final LinearEquation nfEQ = new LinearEquation( lowerQ1, km1.getNForeland(), lowerQ2, km2.getNForeland() );

    final double nf = nfEQ.computeY( lowerQ );
    final double kf = kfEQ.computeY( lowerQ );

    final NKValue nkValue = new NKValue( n, k );
    final NKValue nkValueForeland = new NKValue( nf, kf );

    m_nkValue = nkValue.adjust();
    m_nkValueForeland = nkValueForeland.adjust();
  }

  @Override
  public double getLength( )
  {
    return m_length;
  }

  @Override
  public double getAlpha( )
  {
    return m_aplha;
  }

  @Override
  public double getK( )
  {
    return m_nkValue.getK();
  }

  @Override
  public double getN( )
  {
    return m_nkValue.getN();
  }

  @Override
  public double getKForeland( )
  {
    return m_nkValueForeland.getK();
  }

  @Override
  public double getNForeland( )
  {
    return m_nkValueForeland.getN();
  }

  /**
   * Gets the km value for a certain discharge by interpolating them from the existing km-values
   */
  public static IKMValue interpolateKM( final SortedSet<IKMValue> sort, final double q ) throws SameXValuesException
  {
    final DummyKMValue value = new DummyKMValue( q );
    final SortedSet<IKMValue> headSet = sort.headSet( value );
    if( headSet.isEmpty() )
      return sort.first();

    final IKMValue km1 = headSet.last();
    final IKMValue km2 = sort.tailSet( value ).first();
    return new KMValueFromQinterpolation( q, km1, km2 );
  }

  @Override
  public double getLowerQchannel( )
  {
    return m_qLowerChannel;
  }

  @Override
  public double getUpperQchannel( )
  {
    return m_qUpperChannel;
  }

  @Override
  public double getLowerQforeland( )
  {
    return m_qLowerForeland;
  }

  @Override
  public double getUpperQforeland( )
  {
    return m_qUpperForeland;
  }

}
