package org.kalypso.psiadapter.repository.conversion;

/**
 * SIConverter
 * 
 * @author schlienger
 */
public class SIConverter implements IValueConverter
{
  private final double m_psi2kal;
  private final double m_kal2psi;

  public SIConverter( final String psiUnit, final String kalypsoUnit )
  {
    final double psif = factor( psiUnit );
    final double kalf = factor( kalypsoUnit );
    
    m_psi2kal = psif / kalf;
    m_kal2psi = kalf / psif;
  }
  
  /**
   * @see org.kalypso.psiadapter.repository.conversion.IValueConverter#psi2kalypso(double)
   */
  public double psi2kalypso( double value )
  {
    return value * m_psi2kal;
  }

  /**
   * @see org.kalypso.psiadapter.repository.conversion.IValueConverter#kalypso2psi(double)
   */
  public double kalypso2psi( double value )
  {
    return value * m_kal2psi;
  }

  /**
   * TODO: complete the list!
   * 
   * Returns the factor associated to the given unit
   * 
   * @param unit
   * @return factor
   */
  private final static double factor( final String unit )
  {
    // BEWARE: 10^-x does not give the right value 
    if( unit.equals( "m") )
      return 1;
    if( unit.equals( "dm") )
      return 0.1;
    if( unit.equals( "cm") )
      return 0.01;
    if( unit.equals( "mm") )
      return 0.001;
    
    if( unit.equals( "m³") )
      return 1;

    return 1;
  }
}
