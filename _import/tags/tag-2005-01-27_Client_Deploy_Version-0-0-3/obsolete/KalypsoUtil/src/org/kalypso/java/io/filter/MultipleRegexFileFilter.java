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
package org.kalypso.java.io.filter;

import java.io.File;

import java.util.regex.Pattern;

/**
 * Ein FileFilter was mit Regex definiert werden kann.
 * 
 * @author belger;schlienger
 */
public class MultipleRegexFileFilter implements java.io.FileFilter
{
  /** Die Regex Pattern, eines davon muss die gesuchte Datei erfüllen */
  private Pattern[] m_patterns;

  /** Pattern auch auf Verzeichnisse anwenden */
  private final boolean m_bFilterDirs;

  /**
   * falls false, wird für accept( File f ) immer false zurückgegeben, falls f
   * ein Verzeichnis ist
   */
  private final boolean m_bShowDir;

  /**
   * Constructor
   * 
   * @param patterns
   *          string regex pattern unqualified filename must match to be
   *          included.
   */
  public MultipleRegexFileFilter( final String[] patterns, boolean bFilterDirs, boolean bShowDirs,
      boolean bIgnoreCase )
  {
    m_bFilterDirs = bFilterDirs;
    m_bShowDir = bShowDirs;

    setPatterns( patterns, bIgnoreCase );
  }

  public void setPatterns( String[] patterns, boolean bIgnoreCase )
  {
    if( patterns == null )
    {
      m_patterns = null;

      return;
    }

    m_patterns = new Pattern[patterns.length];

    int flags = bIgnoreCase ? ( Pattern.CASE_INSENSITIVE | Pattern.UNICODE_CASE ) : 0;

    for( int patternID = 0; patternID < patterns.length; patternID++ )
      m_patterns[patternID] = Pattern.compile( patterns[patternID], flags );
  }

  /**
   * Select only files that match the pattern.
   * 
   * @param file
   *          the name of the file
   * 
   * @return true if and only if the name should be included in the file list;
   *         false otherwise.
   */
  public boolean accept( File file )
  {
    if( m_patterns == null )
      return false;

    // falls Verzeichnisse nicht gezeigt werden sollen, gleich zurück
    if( file.isDirectory() && !m_bShowDir )
      return false;

    // falls Pattern nicht auf Verzeichnis engewendet wereden soll, gleich
    // zurück
    if( file.isDirectory() && !m_bFilterDirs )
return true;

    String name = file.getName();

    for( int patternID = 0; patternID < m_patterns.length; patternID++ )
    {
      if( m_patterns[patternID].matcher( name ).matches() == true )
        return true;
    }

    return false;
  }
}