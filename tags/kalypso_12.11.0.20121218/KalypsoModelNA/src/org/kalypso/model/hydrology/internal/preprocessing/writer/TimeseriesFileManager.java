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
package org.kalypso.model.hydrology.internal.preprocessing.writer;

import java.io.File;
import java.util.HashMap;

import javax.xml.namespace.QName;

import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.internal.IDManager;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Gernot Belger
 */
public class TimeseriesFileManager
{
  private static final String STD_TEMP_FILENAME = "std.tmp"; //$NON-NLS-1$

  private static final String STD_VERD_FILENAME = "std.ver"; //$NON-NLS-1$

  private final HashMap<String, String> m_fileMap = new HashMap<>();

  private final IDManager m_idManager;

  private final boolean m_usePrecipicationForm;

  public TimeseriesFileManager( final IDManager idManager, final boolean usePrecipicationForm )
  {
    m_idManager = idManager;
    m_usePrecipicationForm = usePrecipicationForm;
  }

  private String getFilename( final Feature feature, final String axisType, final String key )
  {
    if( !m_fileMap.containsKey( key ) )
    {
      final int asciiID = m_idManager.getAsciiID( feature );
      final String name = "C_" + Integer.toString( asciiID ).trim() + "." + axisType; //$NON-NLS-1$//$NON-NLS-2$
      m_fileMap.put( key, name );
    }
    return m_fileMap.get( key );
  }

  private String getEingabeFilename( final Feature feature, final String propName, final String axisType )
  {
    final TimeseriesLinkType link = (TimeseriesLinkType) feature.getProperty( new QName( NaModelConstants.NS_NAMODELL, propName ) );
    final String key = propName + link.getHref();
    return getFilename( feature, axisType, key );
  }

  public String getNiederschlagEingabeDateiString( final Catchment catchment )
  {
    if( m_usePrecipicationForm )
    {
      final String key = catchment.getSynthZR();
      return getFilename( catchment, ITimeseriesConstants.TYPE_RAINFALL, key );
    }

    return getEingabeFilename( catchment, "niederschlagZR", ITimeseriesConstants.TYPE_RAINFALL ); //$NON-NLS-1$
  }

  public String getTemperaturEingabeDateiString( final Catchment catchment )
  {
    final ZmlLink temperatureLink = catchment.getTemperatureLink();
    if( temperatureLink.isLinkSet() )
      return getEingabeFilename( catchment, "temperaturZR", ITimeseriesConstants.TYPE_TEMPERATURE ); //$NON-NLS-1$

    return STD_TEMP_FILENAME;
  }

  public File getTemperaturEingabeDatei( final Catchment catchment, final File dir )
  {
    final String name = getTemperaturEingabeDateiString( catchment );
    return new File( dir, name );
  }

  public File getNiederschlagEingabeDatei( final Catchment catchment, final File dir )
  {
    return new File( dir, getNiederschlagEingabeDateiString( catchment ) );
  }

  public File getVerdunstungEingabeDatei( final Catchment catchment, final File dir )
  {
    final String name = getVerdunstungEingabeFilename( catchment );
    return new File( dir, name );
  }

  public String getVerdunstungEingabeFilename( final Catchment catchment )
  {
    final ZmlLink evaporationLink = catchment.getEvaporationLink();
    if( evaporationLink.isLinkSet() )
      return getEingabeFilename( catchment, "verdunstungZR", ITimeseriesConstants.TYPE_EVAPORATION ); //$NON-NLS-1$

    return STD_VERD_FILENAME;
  }

}
