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
package org.kalypso.model.wspm.tuhh.schema.simulation;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.math.BigDecimal;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.apache.commons.io.IOUtils;
import org.apache.commons.io.LineIterator;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhStationComparator;
import org.kalypso.model.wspm.tuhh.schema.KalypsoModelWspmTuhhSchemaPlugin;
import org.kalypso.model.wspm.tuhh.schema.i18n.Messages;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.simulation.core.util.LogHelper;

/**
 * This class is used to process a kalypso1d.exe length-section file.
 * 
 * @author Gernot Belger
 */
public class LengthSectionParser
{
  private final File m_lsFile;

  private final ISimulationResultEater m_resultEater;

  private final File m_outputDir;

  private final String m_epsThinning;

  private final List<ResultLengthSection> m_lengthSections = new ArrayList<>();

  private final String m_titlePattern;

  private final TuhhCalculation m_calculation;

  private final String m_lsFilePattern;

  private final URL m_ovwMapURL;

  public LengthSectionParser( final TuhhCalculation calculation, final File lsFile, final ISimulationResultEater resultEater, final File outputDir, final String epsThinning, final String titlePattern, final String lsFilePattern, final URL ovwMapURL )
  {
    m_calculation = calculation;
    m_lsFile = lsFile;
    m_resultEater = resultEater;
    m_outputDir = outputDir;
    m_epsThinning = epsThinning;
    m_titlePattern = titlePattern;
    m_lsFilePattern = lsFilePattern;
    m_ovwMapURL = ovwMapURL;
  }

  public IStatus process( final LogHelper log ) throws SimulationException, IOException
  {
    return processIntern( log );
  }

  private IStatus processIntern( final LogHelper log ) throws SimulationException, IOException
  {
    if( !m_lsFile.exists() )
      return new Status( IStatus.ERROR, KalypsoModelWspmTuhhSchemaPlugin.getID(), Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.LengthSectionParser.0" ) + m_lsFile.getName() ); //$NON-NLS-1$

    /* Add Input File to results */
    m_resultEater.addResult( "LengthSection", m_lsFile ); //$NON-NLS-1$
    log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.LengthSectionParser.1" ) ); //$NON-NLS-1$

    final String strHeader = FileUtilities.toString( getClass().getResource( "resources/headerLenghSection.txt" ), IWspmTuhhConstants.WSPMTUHH_CODEPAGE ); //$NON-NLS-1$
    final String strFooter = FileUtilities.toString( getClass().getResource( "resources/footerLenghSection.txt" ), IWspmTuhhConstants.WSPMTUHH_CODEPAGE ); //$NON-NLS-1$

    return processLSFile( strHeader, strFooter, log );
  }

  private IStatus processLSFile( final String header, final String footer, final LogHelper log ) throws FileNotFoundException, IOException
  {
    final Collection<IStatus> result = new ArrayList<>();
    final WspmWaterBody waterBody = m_calculation.getReach().getWaterBody();
    final TuhhStationComparator stationComparator = new TuhhStationComparator( waterBody.isDirectionUpstreams() );

    LineIterator lineIterator = null;

    ResultLengthSection lsProc = null;
    try
    {
      lineIterator = IOUtils.lineIterator( new FileInputStream( m_lsFile ), IWspmTuhhConstants.WSPMTUHH_CODEPAGE );

      BigDecimal firstStation = null; // station of previous line
      while( lineIterator.hasNext() )
      {
        if( log.checkCanceled() )
          return Status.CANCEL_STATUS;

        final String nextLine = lineIterator.nextLine();

        /* Introduce space around 'NaN' and '***' values to make it parseable */

        if( nextLine.contains( "NaN" ) ) //$NON-NLS-1$
          log.log( false, Messages.getString( "LengthSectionParser.0" ) ); //$NON-NLS-1$

        // TODO: handle NaN-values to keep information alive (unfortunally BigDecimal throws a NumberFormatException)
        final String cleanLine1 = nextLine.replaceAll( "-NaN", " null " ); //$NON-NLS-1$ //$NON-NLS-2$
        final String cleanLine2 = cleanLine1.replaceAll( "NaN", " null " ); //$NON-NLS-1$ //$NON-NLS-2$
        final String cleanLine3 = cleanLine2.replaceAll( "-999.999", " null " ); //$NON-NLS-1$ //$NON-NLS-2$

        final BigDecimal station = NumberUtils.parseQuietDecimal( cleanLine3, 0, 11, IWspmTuhhConstants.STATION_SCALE );
        final BigDecimal runoff = NumberUtils.parseQuietDecimal( cleanLine3, 17, 27, 3 );

        /* Any lines where station or runoff cannot be parsed are filtered out */
        if( Objects.isNull( station, runoff ) )
          continue;

        /* A new section begins, if the new station is lower than the next station */
        final boolean sectionEnd = firstStation == null || stationComparator.compare( firstStation, station ) == 0;

        if( sectionEnd )
        {
          if( lsProc != null )
          {
            final IStatus processorResult = closeProcessor( lsProc, footer );
            if( !processorResult.isOK() )
              result.add( processorResult );
          }

          log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.LengthSectionParser.2" ), runoff ); //$NON-NLS-1$
          lsProc = new ResultLengthSection( runoff, m_outputDir, m_calculation, m_epsThinning, m_ovwMapURL );
          lsProc.addLine( header );

          lsProc.setTitlePattern( m_titlePattern );
          lsProc.setLsFilePattern( m_lsFilePattern );
        }

        /* clean line */
        lsProc.addLine( cleanLine3 );

        if( firstStation == null )
          firstStation = station;
      }

      if( lsProc != null )
      {
        final IStatus processorResult = closeProcessor( lsProc, footer );
        if( !processorResult.isOK() )
          result.add( processorResult );
      }
    }
    finally
    {
      LineIterator.closeQuietly( lineIterator );
    }

    if( result.isEmpty() )
      return Status.OK_STATUS;

    final IStatus[] children = result.toArray( new IStatus[result.size()] );
    final String msg = String.format( Messages.getString( "LengthSectionParser.1" ) ); //$NON-NLS-1$
    return new MultiStatus( KalypsoModelWspmTuhhSchemaPlugin.getID(), 0, children, msg, null );
  }

  private IStatus closeProcessor( final ResultLengthSection lsProc, final String footer )
  {
    lsProc.addLine( footer );
    lsProc.close();

    m_lengthSections.add( lsProc );

    return lsProc.getResult();
  }

  public ResultLengthSection[] getProcessedLengthSections( )
  {
    return m_lengthSections.toArray( new ResultLengthSection[m_lengthSections.size()] );
  }
}