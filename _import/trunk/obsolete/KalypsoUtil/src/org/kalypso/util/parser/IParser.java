package org.kalypso.util.parser;

/**
 * Ein String basierte Parser: kann aus String Objekte mit dem entsprechende
 * Wert erzeugen. Kann auch aus einem Objekt eine String Representation
 * erzeugen.
 * <p>
 * Der Parser wird mit eine String konfiguriert was der Format der geparste
 * Werte beschreibt.
 * 
 * @author schlienger 
 */
public interface IParser
{
  /** Liefert die Klasse der Objekte was dieser Parser unterstützt */
  public Class getObjectClass();

  /** Liefert der Format was beschreibt wie die Werte formatiert sind */
  public String getFormat();

  /** Liefert eine String Representation vom Objekt im gültigen Format */
  public String toString( Object obj ) throws ParserException;
  
  /** Erzeugt einen Objekt aus der String Representation */
  public Object parse( String text ) throws ParserException;
}
