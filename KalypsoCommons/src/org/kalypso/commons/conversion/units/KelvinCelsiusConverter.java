package org.kalypso.commons.conversion.units;

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
   * @see org.kalypso.commons.conversion.units.IValueConverter#convert(double)
   */
  public double convert( final double value )
  {
    return value - 273.15;
  }

  /**
   * @see org.kalypso.commons.conversion.units.IValueConverter#reverse(double)
   */
  public double reverse( final double value )
  {
    return value + 273.15;
  }
}
