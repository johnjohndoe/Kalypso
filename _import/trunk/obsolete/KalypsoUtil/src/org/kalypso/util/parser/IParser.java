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
  /**
   * Liefert die Klasse der Objekte was dieser Parser unterstützt 
   * @return class
   */
  public Class getObjectClass();

  /**
   * Liefert der Format was beschreibt wie die Werte formatiert sind 
   * @return format string
   */
  public String getFormat();

  /**
   * Liefert eine String Representation vom Objekt im gültigen Format 
   * @param obj
   * @return string representation
   * @throws ParserException
   */
  public String toString( Object obj ) throws ParserException;
  
  /**
   * Erzeugt einen Objekt aus der String Representation 
   * @param text
   * @return instance
   * @throws ParserException
   */
  public Object parse( String text ) throws ParserException;
  
  /**
   * Compares the values
   * 
   * @param value1
   * @param value2
   * @return -1, 0 or 1
   * @throws ParserException
   */
  public int compare( String value1, String value2 ) throws ParserException;
  
  /**
   * Compares the values
   * 
   * @param value1
   * @param value2
   * @return -1, 0, 1
   * @throws ParserException
   */
  public int compare( Object value1, Object value2 ) throws ParserException;
}
