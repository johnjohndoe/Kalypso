/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Bj�rnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universit�t Hamburg-Harburg, Institut f�r Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.wspm.tuhh.core.ctripple;

import java.io.IOException;
import java.math.BigDecimal;

import org.kalypso.contribs.java.lang.NumberUtils;

/**
 * @author Holger Albert
 */
public class CodedTrippleProfilePoint
{
  private final String m_name;

  private final double m_easting;

  private final double m_northing;

  private final double m_height;

  private final String m_code;

  public CodedTrippleProfilePoint( String name, double easting, double northing, double height, String code )
  {
    m_name = name;
    m_easting = easting;
    m_northing = northing;
    m_height = height;
    m_code = code;
  }

  public String getName( )
  {
    return m_name;
  }

  public double getEasting( )
  {
    return m_easting;
  }

  public double getNorthing( )
  {
    return m_northing;
  }

  public double getHeight( )
  {
    return m_height;
  }

  public String getCode( )
  {
    return m_code;
  }

  public BigDecimal getStation( )
  {
    // TODO No need here?
    return null;
  }

  public static CodedTrippleProfilePoint createProfilePoint( String line ) throws IOException
  {
    String[] tokens = line.split( ";" );
    if( tokens.length != 5 )
      throw new IOException( String.format( "Could not parse the line '%s'. The format should be '<name>; <easting>; <northing>; <height>; <code>'...", line ) );

    String name = tokens[0];
    double easting = NumberUtils.parseDouble( tokens[1] );
    double northing = NumberUtils.parseDouble( tokens[2] );
    double height = NumberUtils.parseDouble( tokens[3] );
    String code = tokens[4];

    return new CodedTrippleProfilePoint( name, easting, northing, height, code );
  }
}