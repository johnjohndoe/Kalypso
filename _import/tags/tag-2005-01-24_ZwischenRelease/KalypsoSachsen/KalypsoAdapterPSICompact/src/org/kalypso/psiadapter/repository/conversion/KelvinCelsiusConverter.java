package org.kalypso.psiadapter.repository.conversion;

/**
 * KelvinCelsiusConverter
 * 
 * @author schlienger
 */
public class KelvinCelsiusConverter implements IValueConverter
{
  private static KelvinCelsiusConverter m_instance = null;
  
  private KelvinCelsiusConverter()
  {
    // empty
  }

  public final static KelvinCelsiusConverter getInstance()
  {
    if( m_instance == null )
      m_instance = new KelvinCelsiusConverter();
    
    return m_instance;
  }
  
  /**
   * @see org.kalypso.psiadapter.repository.conversion.IValueConverter#psi2kalypso(double)
   */
  public double psi2kalypso( double value )
  {
    return value - 273.15;
  }

  /**
   * @see org.kalypso.psiadapter.repository.conversion.IValueConverter#kalypso2psi(double)
   */
  public double kalypso2psi( double value )
  {
    return value + 273.15;
  }
}
