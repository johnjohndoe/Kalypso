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
package org.kalypso.util.xml;

import org.kalypso.util.parser.impl.DateParser;
import org.kalypso.util.parser.impl.DoubleParser;

/**
 * Utility class that contains the default java formaters for the types found in
 * xml.
 * 
 * @author schlienger
 */
public class XmlTypes
{
  private XmlTypes( )
  {
    // do not instanciate
  }

  /**
   * Parser for the type <code>date</code>. It uses following format string:
   * 
   * <pre>
   * yyyy-MM-dd'T'HH:mm:ss
   * </pre>
   */
  public final static DateParser PDATE = new DateParser(
      "yyyy-MM-dd'T'HH:mm:ss" );

  /**
   * Parser for the type <code>double</code>. It uses the default behaviour
   * of the java.lang.Double class.
   */
  public final static DoubleParser PDOUBLE = new DoubleParser();
}
