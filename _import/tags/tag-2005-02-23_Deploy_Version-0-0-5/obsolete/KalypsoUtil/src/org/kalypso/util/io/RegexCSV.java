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
package org.kalypso.util.io;

import java.io.IOException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Parses a CSV file. Save is possible using a Writer.
 * 
 * @author schlienger
 */
public class RegexCSV extends AbstractCSV
{
  private final Pattern m_pattern;

  /**
   * Constructor
   * 
   * @param p
   *          regex pattern to use when disecating the lines
   * @param line
   *          the line number to start reading the values at
   * @param ignoreEmptyLines
   *          when true empty lines are ignored and not added to this object
   * 
   * @throws IOException
   */
  public RegexCSV( final Pattern p, final int line,
      final boolean ignoreEmptyLines ) throws IOException
  {
    super( line, ignoreEmptyLines );

    m_pattern = p;
  }

  /**
   * @see org.kalypso.util.io.AbstractCSV#handleCurrentLine(java.lang.String)
   */
  protected void handleCurrentLine( final String line )
  {
    Matcher m = m_pattern.matcher( line );
    if( m.matches() )
    {
      String[] sLine = new String[m.groupCount()];

      for( int i = 0; i < sLine.length; i++ )
        sLine[i] = m.group( i + 1 );

      m_lines.add( sLine );
    }
  }
}