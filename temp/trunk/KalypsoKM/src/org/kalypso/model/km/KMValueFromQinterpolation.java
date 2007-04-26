package org.kalypso.model.km;

import org.kalypso.commons.math.LinearEquation;
import org.kalypso.commons.math.LinearEquation.SameXValuesException;

public class KMValueFromQinterpolation extends AbstractKMValue
{

  private final double m_q;

  private double m_aplha;

  private double m_k;

  private double m_kf;

  private double m_n;

  private double m_nf;

  private double m_qf;

  private final AbstractKMValue m_km1;

//  private boolean m_changed;

  public KMValueFromQinterpolation( double q, AbstractKMValue km1, AbstractKMValue km2 ) throws SameXValuesException
  {
    m_km1 = km1;
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

    System.out.println( "\n Lineare Interpolation zweier KM-Datensätze für q= " + q );
    System.out.println( "KM1: " + km1 );
    System.out.println( "KM2: " + km2 );
    System.out.println( "Ergebnis:" + this );
  }

  @Override
  public double getLength( )
  {
    return m_km1.getLength();
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

  public double getN( )
  {

    return m_n;
  }

  public double getKForeland( )
  {

    return m_kf;
  }

  public double getNForeland( )
  {

    return m_nf;
  }

  public double getQ( )
  {

    return m_q;
  }

  public double getQForeland( )
  {
    return m_qf;
  }

  public void setK( double k )
  {
    m_k = k;
  }

  public void setKf( double kf )
  {
    m_kf = kf;
  }

  public void setN( double n )
  {
    m_n = n;
  }

  public void setNf( double nf )
  {
    m_nf = nf;
  }

//  public void setMaxNReached( boolean isChanged )
//  {
//    m_changed = isChanged;
//  }

}
