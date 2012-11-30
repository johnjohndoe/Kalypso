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

import org.eclipse.core.runtime.IPath;
import org.joda.time.Interval;
import org.joda.time.LocalTime;
import org.joda.time.Period;

/**
 * Entry of the {@link TimeseriesIndex}.
 * 
 * @author Gernot Belger
 */
public class TimeseriesIndexEntry
{
  private final String m_href;

  private final String m_parameterType;

  private final Period m_timestep;

  private final IPath m_relativeSourcePath;

  private final LocalTime m_timestamp;

  private final Interval m_dateRange;

  public TimeseriesIndexEntry( final IPath relativeSourcePath, final String href, final String parameterType, final Period timestep, final LocalTime timestamp, final Interval dateRange )
  {
    m_relativeSourcePath = relativeSourcePath;
    m_href = href;
    m_parameterType = parameterType;
    m_timestep = timestep;
    m_timestamp = timestamp;
    m_dateRange = dateRange;
  }

  public String getSourceFilename( )
  {
    return m_relativeSourcePath.lastSegment();
  }

  public String getHref( )
  {
    return m_href;
  }

  public String getParameterType( )
  {
    return m_parameterType;
  }

  public Period getTimestep( )
  {
    return m_timestep;
  }

  public LocalTime getTimestamp( )
  {
    return m_timestamp;
  }

  public String getOldProjectRelativePath( )
  {
    return m_relativeSourcePath.toPortableString();
  }

  public Interval getDateRange( )
  {
    return m_dateRange;
  }
}