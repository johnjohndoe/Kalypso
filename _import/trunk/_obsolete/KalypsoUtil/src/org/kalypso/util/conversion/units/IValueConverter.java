package org.kalypso.util.conversion.units;

/**
 * IValueConversion for handling unit clashes between systems
 * 
 * @author schlienger
 */
public interface IValueConverter
{
  /**
   * Converts the value according to the settings of the instance. For instance, the KelvinCelsiusConverter converts
   * from Kelvin to Celsius.
   * 
   * @param value
   *          in original unit
   * @return converted value in destination unit
   */
  public double convert( final double value );

  /**
   * Reverses the conversion. For instance, the KelvinCelsiusConverter reverse from Celsius to Kelvin.
   * 
   * @param value
   *          in destination unit
   * @return value in original unit
   */
  public double reverse( final double value );
}
