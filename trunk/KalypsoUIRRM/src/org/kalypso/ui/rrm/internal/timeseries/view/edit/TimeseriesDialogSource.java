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
package org.kalypso.ui.rrm.internal.timeseries.view.edit;

import java.io.File;
import java.io.IOException;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.ArrayUtils;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.zml.core.base.IMultipleZmlSourceElement;
import org.kalypso.zml.core.base.IZmlSourceElement;

/**
 * @author Dirk Kuch
 */
public class TimeseriesDialogSource implements IMultipleZmlSourceElement
{
  private final ITimeseries m_timeseries;

  private IObservation m_observation;

  private File m_tempFile;

  private IZmlSourceElement[] m_sources;

  private ClonedTimeseriesSource m_element;

  public TimeseriesDialogSource( final ITimeseries timeseries ) throws Exception
  {
    m_timeseries = timeseries;

    init();
  }

  private void init( ) throws IOException, SensorException
  {
    final ZmlLink link = m_timeseries.getDataLink();
    final File source = link.getJavaFile();
    m_tempFile = new File( FileUtils.getTempDirectory(), source.getName() );

    // FIXME: why copy?!

    // FIXME: handle case where file does not exist?

    FileUtils.copyFile( source, m_tempFile );

    m_observation = ZmlFactory.parseXML( m_tempFile.toURI().toURL() );
  }

  @Override
  public String getIdentifier( )
  {
    return m_timeseries.getParameterType();
  }

  @Override
  public IZmlSourceElement[] getSources( )
  {
    if( ArrayUtils.isEmpty( m_sources ) )
    {
      m_element = new ClonedTimeseriesSource( m_timeseries, m_observation );
      m_sources = new IZmlSourceElement[] { m_element };
    }

    return m_sources;
  }

  @Override
  public void add( final IZmlSourceElement source )
  {
    throw new UnsupportedOperationException();
  }

  @Override
  public String getType( )
  {
    return m_timeseries.getParameterType();
  }

  public void save( )
  {
    try
    {
      if( m_element.isDirty() )
      {
        final ZmlLink link = m_timeseries.getDataLink();
        link.saveObservation( m_observation );
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  public void dispose( )
  {
    m_observation = null;
    FileUtils.deleteQuietly( m_tempFile );
  }
}