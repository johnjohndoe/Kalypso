/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.wspwin.core.prf.datablock;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.StringTokenizer;

/**
 * @author kimwerner
 */
public class DataBlockHeader
{
  private String m_firstLine;

  private String m_secondLine;

  private String m_thirdLine;

  private final List<Integer> m_specifications = new ArrayList<Integer>();

  public DataBlockHeader( final BufferedReader br ) throws IOException
  {
    m_firstLine = br.readLine();
    m_secondLine = br.readLine();
    setThirdLine( br.readLine() );
  }

  public DataBlockHeader( final String firstLine )
  {
    this( firstLine, "" ); //$NON-NLS-1$
  }

  /**
   * Same as {@link #DataBlockHeader(String, "", int)}
   */
  public DataBlockHeader( final String firstLine, final int specialId )
  {
    this( firstLine, "", specialId ); //$NON-NLS-1$
  }

  public DataBlockHeader( final String firstLine, final String secondLine )
  {
    this( firstLine, secondLine, 0 );
  }

  /**
   * @param specialId
   *          The special data block id (last number in second line).
   */
  public DataBlockHeader( final String firstLine, final String secondLine, final int specialId )
  {
    m_firstLine = firstLine;
    m_secondLine = secondLine;
    m_thirdLine = "0  0  0  0  0  0  0  0  " + specialId; //$NON-NLS-1$

    parseThirdLine();
  }

  public String getFirstLine( )
  {
    return m_firstLine;
  }

  public String getSecondLine( )
  {
    return m_secondLine;
  }

  public Integer getSpecification( final int index )
  {
    return (index < m_specifications.size() ? m_specifications.get( index ) : -1);
  }

  public String getThirdLine( )
  {
    return m_thirdLine;
  }

  private void parseThirdLine( )
  {
    final StringTokenizer sT = m_thirdLine == null ? null : new StringTokenizer( m_thirdLine );
    if( sT == null )
      return;

    while( sT.hasMoreTokens() )
    {
      try
      {
        final int i = Integer.parseInt( sT.nextToken() );
        m_specifications.add( i );
      }
      catch( final NumberFormatException e )
      {
        m_specifications.clear();
        break;
      }
    }

  }

  public void printToPrinter( final PrintWriter pw )
  {
    pw.println( m_firstLine );
    pw.println( m_secondLine );
    pw.println( m_thirdLine );
  }

  public void setFirstLine( final String firstLine )
  {
    m_firstLine = firstLine;
  }

  public void setSecondLine( final String secondLine )
  {
    m_secondLine = secondLine;
  }

  public void setSpecifications( final Integer[] specification )
  {
    m_specifications.clear();
    m_specifications.addAll( Arrays.asList( specification ) );
  }

  public void setThirdLine( final String thirdLine )
  {
    m_thirdLine = thirdLine;
    parseThirdLine();
  }

}
