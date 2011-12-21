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

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;

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

  private String m_result;

  private final TimeseriesIndex m_index;

  public CatchmentTimeseriesGuesser( final ZmlLink modelTargetLink, final String parameterType, final TimeseriesIndex index )
  {
    m_modelTargetLink = modelTargetLink;
    m_parameterType = parameterType;
    m_index = index;
  }

  public String getResult( )
  {
    return m_result;
  }

  public IStatus execute( )
  {
    if( validateTargetLink() )
      m_result = guessTimeseries();

    // TODO: validate: check if values of existing timeseries correspond to guessed timeseries

    final String message = String.format( "Guess timeseries for catchment '%s'", m_modelTargetLink.getFeature().getName() );
    return m_log.asMultiStatusOrOK( message, message );
  }

  private boolean validateTargetLink( )
  {
    if( !m_modelTargetLink.isLinkSet() )
    {
      final String message = String.format( "Timeseries link not set, aborting guess." );
      m_log.add( IStatus.WARNING, message );
      return false;
    }

    if( !m_modelTargetLink.isLinkExisting() )
    {
      final String message = String.format( "Referenced timeseries does not exist. Only guess by file name." );
      m_log.add( IStatus.WARNING, message );
    }

    return true;
  }

  private String guessTimeseries( )
  {
    final String guess1 = guessByMapping();
    if( guess1 != null )
      return guess1;

    final String guess2 = guessByValues();
    if( guess2 != null )
      return guess2;

    return guessByFilename();
  }

  private String guessByMapping( )
  {
    // TODO: try 1: use mapping file
    return null;
  }

  private String guessByValues( )
  {
    // TODO: try 2: search a timeseries that has the right values
    return null;
  }

  private String guessByFilename( )
  {
    m_log.add( IStatus.INFO, "Try to find timeseries by filename" );

    final String existingTimeseriesFilename = findExistingFilename();

    if( StringUtils.isBlank( existingTimeseriesFilename ) )
    {
      m_log.add( IStatus.WARNING, "Filename is empty, aborting." );
      return null;
    }

    // TODO find timeseries with same name and parameterType
    final TimeseriesIndexEntry infos[] = m_index.findTimeseries( existingTimeseriesFilename );
    final TimeseriesIndexEntry bestGuess = findBuestGuess( infos );

    if( bestGuess == null )
      return null;

    final String href = bestGuess.getHref();

    m_log.add( IStatus.OK, "Found timeseries: %s", null, href );

    return href;
  }

  private TimeseriesIndexEntry findBuestGuess( final TimeseriesIndexEntry[] infos )
  {
    if( ArrayUtils.isEmpty( infos ) )
    {
      m_log.add( IStatus.WARNING, "No timeseries with same filename found" );
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
        m_log.add( IStatus.INFO, "Multiple timeseries with same file name found, using first one with same parameter type." );
        return info;
      }
    }

    m_log.add( IStatus.WARNING, "Multiple timeseries with same file name found, but none with parameter type '%s'", null, m_parameterType );

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