package org.kalypso.contribs.java.lang;

/**
 * Utility class for Number parsing etc.
 * 
 * @author gernot
 */
public final class NumberUtils
{
  private NumberUtils()
  {
  // do not instantiate
  }

  public static final boolean isDouble( final String string )
  {
    return !Double.isNaN( parseQuiet( string ) );
  }

  /** Tries to parse a double, if fails, returns {@link java.lang.Double#NaN} */
  public static final double parseQuiet( final String string )
  {
    try
    {
      return Double.parseDouble( string.replace( ',', '.' ) );
    }
    catch( final Exception e )
    {
      return Double.NaN;
    }
  }

  /**
   * Tries to parse an integer, if fails retruns null
   */
  public static final Integer parseQuietInteger( final String string )
  {
    try
    {
      return new Integer( Integer.parseInt( string ) );
    }
    catch( final NumberFormatException e )
    {
      return null;
    }
  }
}
