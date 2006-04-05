/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.convert.namodel.timeseries.diff;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;

import org.kalypso.commons.diff.IDiffComparator;
import org.kalypso.commons.diff.IDiffObject;
import org.kalypso.convert.namodel.timeseries.BlockTimeSeries;

/**
 * @author kuepfer
 */
public class BlockTimeSeriesDiffObject implements IDiffObject
{

  private static final String SEPERATOR = "#";

  private final BlockTimeSeries m_blockTimeSeries;

  private final List<String> m_pathes;

  public BlockTimeSeriesDiffObject( File file )
  {
    BlockTimeSeries series = new BlockTimeSeries();
    series.importBlockFile( file );
    m_blockTimeSeries = series;
    String name = file.getName();
    Enumeration keys = m_blockTimeSeries.getKeys();
    List<String> list = new ArrayList<String>();
    while( keys.hasMoreElements() )
    {
      String element = (String) keys.nextElement();
      list.add( name + SEPERATOR + element );
    }
    m_pathes = list;
  }

  /**
   * @see org.kalypso.commons.diff.IDiffObject#exists(java.lang.String)
   */
  public boolean exists( String path )
  {
    return m_pathes.contains( path );
  }

  /**
   * @see org.kalypso.commons.diff.IDiffObject#getDiffComparator(java.lang.String)
   */
  public IDiffComparator getDiffComparator( String path )
  {
    return new BlockTimeSeriesDiffComperator();
  }

  /**
   * @see org.kalypso.commons.diff.IDiffObject#getContent(java.lang.String)
   */
  public Object getContent( String path )
  {
    // returns TreeMap
    final String key = path.substring( path.indexOf( SEPERATOR ) + SEPERATOR.length() );
    return m_blockTimeSeries.getTimeSerie( key );
  }

  /**
   * @see org.kalypso.commons.diff.IDiffObject#getPathes()
   */
  public String[] getPathes( )
  {
    return m_pathes.toArray( new String[m_pathes.size()] );
  }

}
