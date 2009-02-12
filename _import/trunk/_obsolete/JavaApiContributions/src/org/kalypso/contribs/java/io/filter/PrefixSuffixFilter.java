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
package org.kalypso.contribs.java.io.filter;

import java.io.File;
import java.io.FilenameFilter;

/**
 * A class that filters file names according to a prexif and a suffix. A file which name begins with prefix and ends
 * with suffix will be accepted.
 * 
 * @author schlienger
 */
public class PrefixSuffixFilter implements FilenameFilter
{
  private final String m_prefix;

  private final String m_suffix;

  /**
   * constructor
   */
  public PrefixSuffixFilter( String prefix, String suffix )
  {
    if( prefix == null || suffix == null )
      throw new IllegalArgumentException( "Parameters prefix/suffix must not be null" );

    m_prefix = prefix;
    m_suffix = suffix;
  }

  /**
   * @see java.io.FilenameFilter#accept(java.io.File, java.lang.String)
   */
  public boolean accept( File dir, String name )
  {
    if( name.startsWith( m_prefix ) && name.endsWith( m_suffix ) )
      return true;

    return false;
  }
}
