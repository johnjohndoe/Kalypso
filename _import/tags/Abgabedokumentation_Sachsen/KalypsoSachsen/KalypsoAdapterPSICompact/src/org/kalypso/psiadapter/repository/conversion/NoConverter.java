package org.kalypso.psiadapter.repository.conversion;

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
   * @see org.kalypso.psiadapter.repository.conversion.IValueConverter#psi2kalypso(double)
   */
  public double psi2kalypso( double value )
  {
    return value;
  }

  /**
   * @see org.kalypso.psiadapter.repository.conversion.IValueConverter#kalypso2psi(double)
   */
  public double kalypso2psi( double value )
  {
    return value;
  }
}
