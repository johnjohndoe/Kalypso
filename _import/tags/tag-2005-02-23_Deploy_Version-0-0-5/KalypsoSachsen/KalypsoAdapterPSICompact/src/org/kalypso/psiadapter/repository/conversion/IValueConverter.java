package org.kalypso.psiadapter.repository.conversion;

/**
 * IValueConversion for handling unit clashes between kalypso and psicompact.
 * 
 * @author schlienger
 */
public interface IValueConverter
{
  /**
   * Converts the psi value to a kalypso one
   * 
   * @param value
   * @return kalypso value
   */
  public double psi2kalypso( final double value );
  
  /**
   * Converts the kalypso value to a psi one
   * 
   * @param value
   * @return psi value
   */
  public double kalypso2psi( final double value );
}
