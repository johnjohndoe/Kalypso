package org.kalypso.util.parser;

/**
 * Abstrakte Parser das in toString( Object ) allgemeine Tests durchführt. Unterklassen brauchen
 * dann nur den echten toString() Business in toStringInternal( Object ) zu implementieren.
 * 
 * @author schlienger
 */
public abstract class AbstractParser implements IParser
{
  /**
   * Bevor toStringInternal() aufgerufen wird, testet die Implementierung dieser
   * Methode ob es sich um den richtigen Objekttyp handelt.
   * <p>
   * Wenn der Objekt null ist, dann wird "null" zurückgegeben.
   * 
   * @see org.kalypso.util.parser.IParser#toString(java.lang.Object)
   */
  public String toString( Object obj ) 
  {
//    if( ( obj != null ) && !getObjectClass().isAssignableFrom( obj.getClass() ) )
//      throw new ParserException( "Object " + obj.toString() + " (type: " + obj.getClass().getName()
//          + ") is not of the type " + getObjectClass().getName() );

    if( obj == null )
      return "null";

    return toStringInternal( obj );
  }

  /**
   * Diese Methode wird von toString( Object ) aufgerufen damit Unterklassen der
   * toString Business tatsächlich implementieren können.
   * 
   * @param obj
   * @return string
   */
  public abstract String toStringInternal( Object obj );
  
  /**
   * @see org.kalypso.util.parser.IParser#compare(java.lang.String,
   *      java.lang.String)
   */
  public int compare( String value1, String value2 ) throws ParserException
  {
    return compare( parse( value1 ), parse( value2 ) );
  }
}