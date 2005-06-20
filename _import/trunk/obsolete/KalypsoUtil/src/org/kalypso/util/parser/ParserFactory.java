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

import java.util.Date;
import java.util.Properties;

import org.kalypso.util.factory.ConfigurableCachableObjectFactory;
import org.kalypso.util.factory.FactoryException;
import org.kalypso.util.parser.impl.DateParser;
import org.kalypso.util.parser.impl.DoubleParser;
import org.kalypso.util.parser.impl.IntegerParser;
import org.kalypso.util.parser.impl.StringParser;

/**
 * Eine Factory-Klasse was Instanzen von IParser anhand der Parser-String-Spezifikation erzeugt.
 * 
 * @author schlienger
 */
public class ParserFactory
{
  private final ConfigurableCachableObjectFactory m_objFactory;

  /**
   * @param props
   *          beinhaltet der mapping type -- className für welche Objekte erzeugt werden
   * @param cl
   *          der ClassLoader der benutzt werden soll um die Klassen zu laden
   */
  public ParserFactory( final Properties props, final ClassLoader cl )
  {
    // cache not active because with use arguments when instanciating the parser
    m_objFactory = new ConfigurableCachableObjectFactory( props, false, cl );
  }

  /**
   * Erzeugt den gewünschten Parser anhand vom type und formatSpec
   * 
   * @param type
   * @param formatSpec
   * @return parser
   * @throws FactoryException
   */
  public IParser createParser( final String type, final String formatSpec ) throws FactoryException
  {
    String[] args = null;

    if( formatSpec != null )
    {
      args = new String[1];
      args[0] = formatSpec;
    }

    return (IParser)m_objFactory.getObjectInstance( type, IParser.class, args );
  }

  /**
   * @param dataClass
   * @return adequate parser for the given dataclass
   */
  public static IParser createParser( final Class dataClass )
  {
    if( Date.class.isAssignableFrom( dataClass ) )
      return new DateParser();

    if( Integer.class.isAssignableFrom( dataClass ) )
      return new IntegerParser();

    if( Double.class.isAssignableFrom( dataClass ) )
      return new DoubleParser();

    return new StringParser();
  }
}
