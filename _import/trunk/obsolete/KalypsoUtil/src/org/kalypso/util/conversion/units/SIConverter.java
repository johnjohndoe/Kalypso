package org.kalypso.util.conversion.units;

/**
 * SIConverter
 * 
 * @author schlienger
 */
public class SIConverter implements IValueConverter
{
  private final double m_factor;

  public SIConverter( final String orgUnit, final String destUnit )
  {
    final double orgf = factor( orgUnit );
    final double destf = factor( destUnit );
    
    m_factor = orgf / destf;
  }
  
  /**
   * @see org.kalypso.util.conversion.units.IValueConverter#convert(double)
   */
  public double convert( double value )
  {
    return value * m_factor;
  }

  /**
   * @see org.kalypso.util.conversion.units.IValueConverter#reverse(double)
   */
  public double reverse( double value )
  {
    return value / m_factor;
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
