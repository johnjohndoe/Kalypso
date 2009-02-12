/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.util.parser;

/**
 * Abstrakte Parser das in toString( Object ) allgemeine Tests durchführt. Unterklassen brauchen dann nur den echten
 * toString() Business in toStringInternal( Object ) zu implementieren.
 * 
 * @author schlienger
 */
public abstract class AbstractParser implements IParser
{
  /**
   * Bevor toStringInternal() aufgerufen wird, testet die Implementierung dieser Methode ob es sich um den richtigen
   * Objekttyp handelt.
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
   * Diese Methode wird von toString( Object ) aufgerufen damit Unterklassen der toString Business tatsächlich
   * implementieren können.
   * 
   * @param obj
   * @return string
   */
  public abstract String toStringInternal( Object obj );

  /**
   * @see org.kalypso.util.parser.IParser#compare(java.lang.String, java.lang.String)
   */
  public int compare( String value1, String value2 ) throws ParserException
  {
    return compare( parse( value1 ), parse( value2 ) );
  }
}