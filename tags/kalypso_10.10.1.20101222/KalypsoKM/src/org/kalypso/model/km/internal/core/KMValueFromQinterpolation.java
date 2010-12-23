package org.kalypso.model.km.internal.core;

import java.util.SortedSet;

import org.kalypso.commons.math.LinearEquation;
import org.kalypso.commons.math.LinearEquation.SameXValuesException;

/**
 * {@link IKMValue} derived by linear interpolation over q between two adjacent {@link IKMValue}.
 */
class KMValueFromQinterpolation extends AbstractKMValue
{
  private final double m_q;

  private final double m_aplha;

  private double m_k;

  private double m_kf;

  private double m_n;

  private double m_nf;

  private final double m_qf;

  private final double m_length;

  private KMValueFromQinterpolation( final double q, final IKMValue km1, final IKMValue km2 ) throws SameXValuesException
  {
    m_length = km1.getLength();

    final LinearEquation alpha = new LinearEquation( km1.getQSum(), km1.getAlpha(), km2.getQSum(), km2.getAlpha() );
    m_aplha = alpha.computeY( q );

    final LinearEquation k = new LinearEquation( km1.getQSum(), km1.getK(), km2.getQSum(), km2.getK() );
    m_k = k.computeY( q );

    final LinearEquation kf = new LinearEquation( km1.getQSum(), km1.getKForeland(), km2.getQSum(), km2.getKForeland() );
    m_kf = kf.computeY( q );

    final LinearEquation n = new LinearEquation( km1.getQSum(), km1.getN(), km2.getQSum(), km2.getN() );
    m_n = n.computeY( q );

    final LinearEquation nf = new LinearEquation( km1.getQSum(), km1.getNForeland(), km2.getQSum(), km2.getNForeland() );
    m_nf = nf.computeY( q );

    final LinearEquation qeq = new LinearEquation( km1.getQSum(), km1.getQ(), km2.getQSum(), km2.getQ() );
    m_q = qeq.computeY( q );

    final LinearEquation qf = new LinearEquation( km1.getQSum(), km1.getQForeland(), km2.getQSum(), km2.getQForeland() );
    m_qf = qf.computeY( q );

//    System.out.println( Messages.getString( "org.kalypso.model.km.KMValueFromQinterpolation.0" ) + q ); //$NON-NLS-1$
//    System.out.println( Messages.getString( "org.kalypso.model.km.KMValueFromQinterpolation.1" ) + km1 ); //$NON-NLS-1$
//    System.out.println( Messages.getString( "org.kalypso.model.km.KMValueFromQinterpolation.2" ) + km2 ); //$NON-NLS-1$
//    System.out.println( Messages.getString( "org.kalypso.model.km.KMValueFromQinterpolation.3" ) + this ); //$NON-NLS-1$
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
    return m_k;
  }

  @Override
  public double getN( )
  {

    return m_n;
  }

  @Override
  public double getKForeland( )
  {

    return m_kf;
  }

  @Override
  public double getNForeland( )
  {

    return m_nf;
  }

  @Override
  public double getQ( )
  {

    return m_q;
  }

  @Override
  public double getQForeland( )
  {
    return m_qf;
  }

  public void setK( final double k )
  {
    m_k = k;
  }

  public void setKf( final double kf )
  {
    m_kf = kf;
  }

  public void setN( final double n )
  {
    m_n = n;
  }

  public void setNf( final double nf )
  {
    m_nf = nf;
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
    final KMValueFromQinterpolation strandKMValue = new KMValueFromQinterpolation( q, km1, km2 );

    // TODO: check if this is right: Setting n to maximum 30 (Prof.Pasche)
    if( strandKMValue.getN() > 30d )
    {
      final double prod = strandKMValue.getN() * strandKMValue.getK();
      strandKMValue.setN( 30d );
      strandKMValue.setK( prod / 30d );
    }
    if( strandKMValue.getNForeland() > 30d )
    {
      final double prod = strandKMValue.getNForeland() * strandKMValue.getKForeland();
      strandKMValue.setNf( 30d );
      strandKMValue.setKf( prod / 30d );
    }
    return strandKMValue;
  }
}
