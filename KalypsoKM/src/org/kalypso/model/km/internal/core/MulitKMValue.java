package org.kalypso.model.km.internal.core;

import org.kalypso.model.km.internal.i18n.Messages;

/**
 * interpolates KM for some profiles at same discharge
 * 
 * @author doemming
 */
class MulitKMValue extends AbstractKMValue
{
  private final double m_alpha;

  private final double m_length;

  private final double m_k;

  private final double m_kForeland;

  private final double m_n;

  private final double m_nForeland;

  private final IKMValue m_innerKM;

  /*
   * Aggregation of Kalinin-Miljukov parameters for one river strand.
   */
  public MulitKMValue( final IKMValue[] values )
  {
    m_innerKM = values[0];
    double length = 0;
    double alpha = 0;
    double k = 0;
    double kForeland = 0;
    double n = 0;
    double nForeland = 0;
    for( final IKMValue km : values )
    {
      length += km.getLength();
      alpha += km.getAlpha() * km.getLength();
      k += km.getK() * km.getLength();
      kForeland += km.getKForeland() * km.getLength();
      n += km.getN();
      nForeland += km.getNForeland();
    }
    // calculate mean values for the strand by the merged profiles
    m_alpha = alpha / length;
    m_length = length;
    m_k = k / length;
    m_kForeland = kForeland / length;
    m_n = n;
    m_nForeland = nForeland;
    // TODO: Put in logger, because of tooooo many values!!!
    System.out.println( Messages.getString( "org.kalypso.model.km.MulitKMValue.0" ) + values.length + Messages.getString( "org.kalypso.model.km.MulitKMValue.1" ) ); //$NON-NLS-1$ //$NON-NLS-2$
    for( int i = 0; i < values.length; i++ )
    {
      System.out.println( Messages.getString( "org.kalypso.model.km.MulitKMValue.2" ) + (i + 1) + ":" ); //$NON-NLS-1$ //$NON-NLS-2$
      System.out.println( "-------------" ); //$NON-NLS-1$
      final IKMValue value = values[i];
      System.out.println( value );
    }
    System.out.println( Messages.getString( "org.kalypso.model.km.MulitKMValue.5" ) + this ); //$NON-NLS-1$
  }

  @Override
  public double getAlpha( )
  {
    return m_alpha;
  }

  @Override
  public double getKForeland( )
  {

    return m_kForeland;
  }

  @Override
  public double getNForeland( )
  {
    return m_nForeland;
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
  public double getLength( )
  {
    return m_length;
  }

  @Override
  public double getQ( )
  {
    return m_innerKM.getQ();
  }

  @Override
  public double getQForeland( )
  {
    return m_innerKM.getQForeland();
  }

}
