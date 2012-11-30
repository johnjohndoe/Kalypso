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
import java.util.Map;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.IStatus;
import org.joda.time.Interval;
import org.joda.time.LocalTime;
import org.joda.time.Period;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;

/**
 * Helper the guesses the catchment timeserties from the former referenced timeseries.
 * 
 * @author Gernot Belger
 */
public class CatchmentTimeseriesGuesser
{
  private final IStatusCollector m_log = new StatusCollector( KalypsoUIRRMPlugin.getID() );

  private final ZmlLink m_modelTargetLink;

  private final String m_parameterType;

  private TimeseriesIndexEntry m_result;

  private final TimeseriesIndex m_index;

  private final Map<String, TimeseriesIndexEntry> m_oldMappings;

  public CatchmentTimeseriesGuesser( final ZmlLink modelTargetLink, final String parameterType, final TimeseriesIndex index, final Map<String, TimeseriesIndexEntry> oldMappings )
  {
    m_modelTargetLink = modelTargetLink;
    m_parameterType = parameterType;
    m_index = index;
    m_oldMappings = oldMappings;
  }

  public String getResult( )
  {
    if( m_result == null )
      return null;

    return m_result.getHref();
  }

  public Period getResultTimestep( )
  {
    if( m_result == null )
      return null;

    return m_result.getTimestep();
  }

  public LocalTime getResultTimestamp( )
  {
    if( m_result == null )
      return null;

    return m_result.getTimestamp();
  }

  public Interval getResultRange( )
  {
    if( m_result == null )
      return null;

    return m_result.getDateRange();
  }

  public IStatus execute( )
  {
    if( validateTargetLink() )
      m_result = guessTimeseries();

    if( m_result != null )
    {
      final String href = m_result.getHref();
      m_log.add( IStatus.OK, Messages.getString( "CatchmentTimeseriesGuesser_0" ), null, href ); //$NON-NLS-1$
    }

    // TODO: validate: check if values of existing timeseries correspond to guessed timeseries

    final String message = String.format( Messages.getString( "CatchmentTimeseriesGuesser_1" ), m_modelTargetLink.getFeature().getName() ); //$NON-NLS-1$
    return m_log.asMultiStatusOrOK( message, message );
  }

  private boolean validateTargetLink( )
  {
    if( !m_modelTargetLink.isLinkSet() )
    {
      final String message = String.format( Messages.getString( "CatchmentTimeseriesGuesser_2" ) ); //$NON-NLS-1$
      m_log.add( IStatus.WARNING, message );
      return false;
    }

    if( !m_modelTargetLink.isLinkExisting() )
    {
      final String message = String.format( Messages.getString( "CatchmentTimeseriesGuesser_3" ) ); //$NON-NLS-1$
      m_log.add( IStatus.WARNING, message );
    }

    return true;
  }

  private TimeseriesIndexEntry guessTimeseries( )
  {
    final TimeseriesIndexEntry guess1 = guessByMapping();
    if( guess1 != null )
      return guess1;

    final TimeseriesIndexEntry guess2 = guessByValues();
    if( guess2 != null )
      return guess2;

    return guessByFilename();
  }

  private TimeseriesIndexEntry guessByMapping( )
  {
    final File targetFile = m_modelTargetLink.getJavaFile();
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
    // Removed log, we only do this kind of guess
    // m_log.add( IStatus.INFO, Messages.getString( "CatchmentTimeseriesGuesser_4" ) ); //$NON-NLS-1$

    final String existingTimeseriesFilename = findExistingFilename();

    if( StringUtils.isBlank( existingTimeseriesFilename ) )
    {
      m_log.add( IStatus.WARNING, Messages.getString( "CatchmentTimeseriesGuesser_5" ) ); //$NON-NLS-1$
      return null;
    }

    // TODO find timeseries with same name and parameterType
    final TimeseriesIndexEntry infos[] = m_index.findTimeseries( existingTimeseriesFilename );
    return findBuestGuess( infos );
  }

  private TimeseriesIndexEntry findBuestGuess( final TimeseriesIndexEntry[] infos )
  {
    if( ArrayUtils.isEmpty( infos ) )
    {
      m_log.add( IStatus.WARNING, Messages.getString( "CatchmentTimeseriesGuesser_6" ) ); //$NON-NLS-1$
      return null;
    }

    if( infos.length == 1 )
      return infos[0];

    for( final TimeseriesIndexEntry info : infos )
    {
      final String parameterType = info.getParameterType();
      if( parameterType.equals( m_parameterType ) )
      {
        // TODO: also use timestep to determine best guess; find all infos with same type
        m_log.add( IStatus.INFO, Messages.getString( "CatchmentTimeseriesGuesser_7" ) ); //$NON-NLS-1$
        return info;
      }
    }

    m_log.add( IStatus.WARNING, Messages.getString( "CatchmentTimeseriesGuesser_8" ), null, m_parameterType ); //$NON-NLS-1$

    return null;
  }

  private String findExistingFilename( )
  {
    final File targetFile = m_modelTargetLink.getJavaFile();
    if( targetFile == null )
      return null;

    return targetFile.getName();
  }
}