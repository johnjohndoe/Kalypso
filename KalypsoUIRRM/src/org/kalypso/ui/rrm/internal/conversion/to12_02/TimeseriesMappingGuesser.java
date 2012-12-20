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

import java.io.File;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.model.hydrology.binding.timeseriesMappings.TimeseriesMappingType;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;

/**
 * Guesses the global timeseries that was used to create a local timeseries in the calculation case.
 * 
 * @author Gernot Belger
 */
public class TimeseriesMappingGuesser
{
  private final IStatusCollector m_log = new StatusCollector( KalypsoUIRRMPlugin.getID() );

  private final ZmlLink m_modelTimeseriesLink;

  private final TimeseriesMappingType m_mappingType;

  private final TimeseriesIndex m_timeseriesIndex;

  private String m_result;

  private final Map<String, TimeseriesIndexEntry> m_oldMappings;

  private final Map<String, Set<TimeseriesIndexEntry>> m_conversionMap;

  public TimeseriesMappingGuesser( final ZmlLink modelTimeseriesLink, final TimeseriesMappingType mappingType, final TimeseriesIndex timeseriesIndex, final Map<String, TimeseriesIndexEntry> oldMappings, final Map<String, Set<TimeseriesIndexEntry>> conversionMap )
  {
    m_modelTimeseriesLink = modelTimeseriesLink;
    m_mappingType = mappingType;
    m_timeseriesIndex = timeseriesIndex;
    m_oldMappings = oldMappings;
    m_conversionMap = conversionMap;
  }

  public String getResult( )
  {
    return m_result;
  }

  public IStatus execute( )
  {
    if( validateTargetLink() )
      m_result = guessTimeseries();

    if( m_result != null )
      m_log.add( IStatus.OK, Messages.getString( "CatchmentTimeseriesGuesser_0" ), null, m_result ); //$NON-NLS-1$

    final String message = String.format( Messages.getString( "TimeseriesMappingGuesser.0" ), m_modelTimeseriesLink.getFeature().getName() ); //$NON-NLS-1$
    return m_log.asMultiStatusOrOK( message, message );
  }

  private boolean validateTargetLink( )
  {
    if( !m_modelTimeseriesLink.isLinkExisting() )
    {
      final String message = String.format( Messages.getString( "CatchmentTimeseriesGuesser_3" ) ); //$NON-NLS-1$
      m_log.add( IStatus.WARNING, message );
    }

    return true;
  }

  private String guessTimeseries( )
  {
    final TimeseriesIndexEntry guess1 = guessByMapping();
    if( guess1 != null )
    {
      doRegisterEvapotionTimeseries( guess1 );

      return guess1.getHref();
    }

    final TimeseriesIndexEntry guess2 = guessByValues();
    if( guess2 != null )
    {
      doRegisterEvapotionTimeseries( guess2 );

      return guess2.getHref();
    }

    final TimeseriesIndexEntry guess3 = guessByFilename();
    if( guess3 != null )
    {
      doRegisterEvapotionTimeseries( guess3 );

      return guess3.getHref();
    }

    return null;
  }

  private void doRegisterEvapotionTimeseries( final TimeseriesIndexEntry item )
  {
    final String parameterType = m_mappingType.getLinkParameterType();
    if( !StringUtils.equals( ITimeseriesConstants.TYPE_EVAPORATION_WATER_BASED, parameterType ) )
      return;

    final String needed = m_mappingType.getLinkParameterType();
    if( StringUtils.equals( needed, item.getParameterType() ) )
      return;

    Set<TimeseriesIndexEntry> convert = m_conversionMap.get( needed );
    if( convert == null )
    {
      convert = new LinkedHashSet<>();
      m_conversionMap.put( needed, convert );
    }

    convert.add( item );
  }

  private TimeseriesIndexEntry guessByMapping( )
  {
    final File targetFile = m_modelTimeseriesLink.getJavaFile();
    if( targetFile == null )
      return null;

    m_log.add( IStatus.INFO, Messages.getString( "TimeseriesMappingGuesser.1" ) ); //$NON-NLS-1$

    final String name = targetFile.getName();
    final String timeseriesName = FilenameUtils.removeExtension( name );

    final TimeseriesIndexEntry entry = m_oldMappings.get( timeseriesName );
    if( entry != null )
    {
      final String message = String.format( Messages.getString( "TimeseriesMappingGuesser.2" ), entry.getOldProjectRelativePath() ); //$NON-NLS-1$
      m_log.add( IStatus.OK, message );
    }
    else
      m_log.add( IStatus.INFO, Messages.getString( "TimeseriesMappingGuesser.3" ) ); //$NON-NLS-1$

    return entry;
  }

  private TimeseriesIndexEntry guessByValues( )
  {
    // TODO: try 2: search a timeseries that has the right values
    return null;
  }

  private TimeseriesIndexEntry guessByFilename( )
  {
    m_log.add( IStatus.INFO, Messages.getString( "CatchmentTimeseriesGuesser_4" ) ); //$NON-NLS-1$

    final String existingTimeseriesFilename = findExistingFilename();

    if( StringUtils.isBlank( existingTimeseriesFilename ) )
    {
      m_log.add( IStatus.WARNING, Messages.getString( "CatchmentTimeseriesGuesser_5" ) ); //$NON-NLS-1$
      return null;
    }

    /* find timeseries with same name and parameter type */
    final TimeseriesIndexEntry infos[] = m_timeseriesIndex.findTimeseries( existingTimeseriesFilename );
    return findBuestGuess( infos );
  }

  private TimeseriesIndexEntry findBuestGuess( final TimeseriesIndexEntry[] infos )
  {
    if( ArrayUtils.isEmpty( infos ) )
    {
      m_log.add( IStatus.WARNING, Messages.getString( "CatchmentTimeseriesGuesser_6" ) ); //$NON-NLS-1$
      return null;
    }

    final String neededParameterType = m_mappingType.getLinkParameterType();

    for( final TimeseriesIndexEntry info : infos )
    {
      if( isTypeOf( neededParameterType, info ) )
      {
        if( infos.length > 1 )
          m_log.add( IStatus.INFO, Messages.getString( "CatchmentTimeseriesGuesser_7" ) ); //$NON-NLS-1$

        return info;
      }
    }

    m_log.add( IStatus.WARNING, Messages.getString( "CatchmentTimeseriesGuesser_8" ), null, neededParameterType ); //$NON-NLS-1$

    return null;
  }

  /**
   * special handling for WATER_BASED evaporation time series.
   */
  private boolean isTypeOf( final String needed, final TimeseriesIndexEntry info )
  {
    final String parameterType = info.getParameterType();
    if( StringUtils.equals( needed, parameterType ) )
      return true;

    if( ITimeseriesConstants.TYPE_EVAPORATION_WATER_BASED.equals( needed ) && StringUtils.equals( ITimeseriesConstants.TYPE_EVAPORATION_LAND_BASED, parameterType ) )
    {
      // FIXME: remember this case! -> change parameter type later

      return true;
    }

    return false;
  }

  private String findExistingFilename( )
  {
    final File targetFile = m_modelTimeseriesLink.getJavaFile();
    if( targetFile == null )
      return null;

    return targetFile.getName();
  }
}