package org.kalypso.util.conversion.units;

/**
 * Performs no conversion, return the values 'as is'.
 * 
 * @author schlienger
 */
public final class NoConverter implements IValueConverter
{
  private static NoConverter instance;

  private NoConverter()
  {
  // no instanciation
  }

  public static NoConverter getInstance()
  {
    if( instance == null )
      instance = new NoConverter();

    return instance;
  }

  /**
   * @see org.kalypso.util.conversion.units.IValueConverter#convert(double)
   */
  public double convert( double value )
  {
    return value;
  }

  /**
   * @see org.kalypso.util.conversion.units.IValueConverter#reverse(double)
   */
  public double reverse( double value )
  {
    return value;
  }
}
