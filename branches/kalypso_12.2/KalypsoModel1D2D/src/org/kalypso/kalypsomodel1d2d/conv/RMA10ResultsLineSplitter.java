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
package org.kalypso.kalypsomodel1d2d.conv;

import java.util.Arrays;
import java.util.StringTokenizer;

/**
 * simple implementation of string splitter based on {@link java.util.StringTokenizer} or on very simple parser of
 * string without strong validation and error handling.
 * 
 * the goal of this class is to out source the string handling and to implement some faster result parsing.
 * 
 */
class RMA10ResultsLineSplitter
{
  private boolean m_boolUseSimpleMethod = true;

  private final String m_strFullString;

  private final char m_charSeparator;

  private boolean m_boolSeparator = false;

  private int m_intStrLen = 0;

  private int m_intSepCounter = 0;

  private char[] m_charStringAsArray;

  private int[] m_intPositionsAsArray;

  StringTokenizer m_stringTokenizer;

  RMA10ResultsLineSplitter( final String pStrFull, final char pCharSeparator )
  {
    m_strFullString = pStrFull;
    init();
    m_charSeparator = pCharSeparator;
    m_intPositionsAsArray = new int[m_intStrLen];
    Arrays.fill( m_intPositionsAsArray, m_intStrLen );
    parseLine();
  }

  RMA10ResultsLineSplitter( final String pStrFull, final String pStrSeparators )
  {
    m_strFullString = pStrFull;
    init();
    m_charSeparator = ' ';
    m_stringTokenizer = new StringTokenizer( pStrFull, pStrSeparators );
    m_boolUseSimpleMethod = false;
  }

  private void init( )
  {
    m_intStrLen = m_strFullString.length();
    m_charStringAsArray = m_strFullString.toCharArray();
  }

  private void parseLine( )
  {
    m_boolSeparator = true;
    int j = 0;
    for( int i = 0; i < m_intStrLen; ++i )
    {
      if( m_charStringAsArray[i] == m_charSeparator )
      {
        if( !m_boolSeparator )
        {
          m_boolSeparator = true;
        }
      }
      else
      {
        if( m_boolSeparator )
        {
          m_boolSeparator = false;
          m_intPositionsAsArray[j++] = i;
          m_intSepCounter++;
        }
      }
    }
  }

  public final int getIntSepCounter( )
  {
    if( m_boolUseSimpleMethod )
      return m_intSepCounter;
    return m_stringTokenizer.countTokens();
  }

  public String getSub( final int pIntPos )
  {
    return m_strFullString.substring( m_intPositionsAsArray[pIntPos], m_intPositionsAsArray[pIntPos + 1] ).trim();
  }

  public String getFirstSub( )
  {
    return m_stringTokenizer.nextToken();
  }

  public String getNextSub( )
  {
    if( m_stringTokenizer.hasMoreTokens() )
    {
      final String lStrRes = m_stringTokenizer.nextToken();
      return lStrRes;
    }
    return ""; //$NON-NLS-1$
  }
}