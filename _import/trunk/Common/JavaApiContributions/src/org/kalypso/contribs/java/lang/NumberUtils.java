package org.kalypso.contribs.java.lang;

import java.text.NumberFormat;
import java.text.ParseException;

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
    return !Double.isNaN( parseQuietDouble( string ) );
  }

  /**
   * Parses a string as double.
   * <p>
   * The double can contain '.' or ','.
   * </p>
   */
  public static final double parseDouble( final String string ) throws NumberFormatException
  {
    return Double.parseDouble( string.replace( ',', '.' ) );
  }

  /**
   * Tries to parse a double, if fails, returns {@link java.lang.Double#NaN}.
   * <p>
   * The double can contain '.' or ','.
   * </p>
   */
  public static final double parseQuietDouble( final String string )
  {
    try
    {
      return parseDouble( string );
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

  /** What is the difference to: <code>Integer.parseInt()</code>? */
  public static int toInteger( final String value ) throws ParseException
  {
    final NumberFormat instance = NumberFormat.getInstance();
    final Number number = instance.parse( value );
    return number.intValue();
  }
}
