package org.kalypso.util.xml;

import java.util.Date;

/**
 * @author schlienger
 */
public class SchemaUtils
{
  /**
   * Gibt die passende Java-Klasse zum Datentyp aus Schema Sicht.
   * <p>
   * Z.B:
   * <p>
   * xs:double gibt Double.class zurück
   * <p>
   * xs:string gibt String.class zurück
   * <p>
   * usw.
   *  
   */
  public static Class typeToClass( String schemaType )
  {
    if( schemaType.equals( "xs:string" ) )
      return String.class;
    else if( schemaType.equals( "xs:double" ) )
      return Double.class;
    else if( schemaType.equals( "xs:integer" ) )
      return Integer.class;
    else if( schemaType.equals( "xs:date" ) )
      return Date.class;
    else
      return String.class;
  }
}