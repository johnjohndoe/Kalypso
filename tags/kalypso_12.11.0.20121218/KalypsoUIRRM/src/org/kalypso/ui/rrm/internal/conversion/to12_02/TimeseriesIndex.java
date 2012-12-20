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
package org.kalypso.ui.rrm.internal.conversion.to12_02;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.TreeMap;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.IOCase;
import org.apache.commons.lang3.ArrayUtils;

/**
 * Index for the imported global timeseries of the project. Used to guess timeseries for the catchment models of the
 * imported simulations.
 *
 * @author Gernot Belger
 */
public class TimeseriesIndex
{
  private final Collection<TimeseriesIndexEntry> m_entries = new ArrayList<>();

  private final Map<String, TimeseriesIndexEntry[]> m_filenameHash = new HashMap<>();

  /**
   * Makes sure that pathes that point to the same file on this file system are considered the same regarding this
   * index.
   */
  private final Comparator<String> m_fileComparator = new Comparator<String>()
  {
    @Override
    public int compare( final String o1, final String o2 )
    {
      final String n1 = FilenameUtils.normalize( o1 );
      final String n2 = FilenameUtils.normalize( o2 );

      return IOCase.SYSTEM.checkCompareTo( n1, n2 );
    }
  };

  private final Map<String, TimeseriesIndexEntry> m_oldHrefIndex = new TreeMap<>( m_fileComparator );

  public void addEntry( final TimeseriesIndexEntry entry )
  {
    m_entries.add( entry );

    /* Fill old href index */
    m_oldHrefIndex.put( entry.getOldProjectRelativePath(), entry );

    /* Add into filename hash */
    final String filename = entry.getSourceFilename();
    if( !m_filenameHash.containsKey( filename ) )
      m_filenameHash.put( filename, new TimeseriesIndexEntry[0] );

    final TimeseriesIndexEntry[] newFileNameArray = ArrayUtils.add( m_filenameHash.get( filename ), entry );
    m_filenameHash.put( filename, newFileNameArray );
  }

  public TimeseriesIndexEntry[] findTimeseries( final String filename )
  {
    return m_filenameHash.get( filename );
  }

  public TimeseriesIndexEntry findTimeseriesByOldHref( final String projectRelativePath )
  {
    return m_oldHrefIndex.get( projectRelativePath );
  }
}