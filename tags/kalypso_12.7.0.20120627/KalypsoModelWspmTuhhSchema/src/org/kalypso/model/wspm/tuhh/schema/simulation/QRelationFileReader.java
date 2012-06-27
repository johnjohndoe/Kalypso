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
import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;
import java.math.BigDecimal;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.runtime.Assert;
import org.kalypso.model.wspm.tuhh.schema.gml.QIntervallResult;
import org.kalypso.model.wspm.tuhh.schema.i18n.Messages;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.simulation.core.util.LogHelper;

/**
 * Reads a file in the .prof or .km file format.
 * 
 * @author Gernot Belger
 */
public class QRelationFileReader
{
  private TupleResult m_result;

  private final LogHelper m_log;

  private BigDecimal m_station;

  private BigDecimal m_bankfull;

  private final Pattern m_stationAndBankFullPattern;

  private final String[] m_components;

  private final int[] m_componentIndex;

  private IObservation<TupleResult> m_observation;

  private final QIntervalIndex m_intervalIndex;

  private QIntervallResult m_qresult;

  private IRecord[] m_existingRecords;

  private int m_currentRecord;

  private BigDecimal m_slope;

  /**
   * @param components
   *          The components the read columns should be filled in. The components are in the same order as the read
   *          columns. If a component is <code>null</code>, the corresponding column will be skipped.
   */
  public QRelationFileReader( final LogHelper log, final Pattern stationAndBankfullPattern, final String[] components, final QIntervalIndex intervalIndex )
  {
    m_log = log;
    m_stationAndBankFullPattern = stationAndBankfullPattern;
    m_components = components;
    m_intervalIndex = intervalIndex;
    m_componentIndex = new int[components.length];
  }

  public void setStation( final BigDecimal station )
  {
    m_station = station;
  }

  public void setSlope( final BigDecimal slope )
  {
    m_slope = slope;
  }

  private void initObservation( final String filename )
  {
    if( m_qresult != null )
      return;

    if( m_station == null )
      return;

    m_qresult = m_intervalIndex.addOrGet( m_station );

    if( m_bankfull != null )
      m_qresult.setBankfull( m_bankfull );

    m_observation = m_qresult.getOrCreatePointsObservation();
    m_result = m_observation.getResult();

    /* Set some common attributes */
    m_observation.setName( m_qresult.getName() );

    final String newDescription = addToDescription( m_qresult.getDescription(), filename );
    m_qresult.setDescription( newDescription );

    m_observation.setDescription( m_qresult.getDescription() );

    if( m_slope != null )
      m_qresult.setSlope( m_slope );

    /* Remember existing records for later */
    m_existingRecords = m_result.toArray( new IRecord[0] );

    /* Hash component indices for fast access later */
    for( int i = 0; i < m_components.length; i++ )
    {
      final String componentID = m_components[i];
      if( componentID != null )
      {
        final int index = m_result.indexOfComponent( componentID );
        if( index == -1 )
        {
          /* Add non-existent components now */
          final IComponent component = m_qresult.createPointsComponent( componentID );
          m_result.addComponent( component );
          final int newindex = m_result.indexOfComponent( componentID );
          m_componentIndex[i] = newindex;
        }
        else
          m_componentIndex[i] = index;
      }
    }
  }

  private String addToDescription( final String description, final String filename )
  {
    if( org.apache.commons.lang3.StringUtils.isBlank( description ) )
      return Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeProcessor.19" ) + filename; //$NON-NLS-1$

    return description + ", " + filename; //$NON-NLS-1$
  }

  public void read( final File inputFile ) throws IOException
  {
    final String filename = inputFile.getName();

    LineNumberReader reader = null;
    try
    {
      reader = new LineNumberReader( new FileReader( inputFile ) );

      while( reader.ready() )
      {
        final String line = reader.readLine();
        if( line == null )
          break;

        final int lineNumber = reader.getLineNumber();
        readLine( line, lineNumber, filename );
      }

      reader.close();

      m_qresult.setPointsObservation( m_observation );
    }
    finally
    {
      IOUtils.closeQuietly( reader );
    }
  }

  private void readLine( final String line, final int lineNumber, final String filename )
  {
    final String[] tokens = line.trim().split( " +" ); //$NON-NLS-1$
    if( tokens == null )
      return;

    if( readStationAndBankfull( line ) )
      return;

    if( tokens.length < m_components.length )
      return;

    try
    {
      /* Determine if this is a good line */
      parseToken( tokens[0] );
    }
    catch( final NumberFormatException nfe )
    {
      /* Just ignore this line */
      return;
    }

    initObservation( filename );

    Assert.isNotNull( m_result );

    /* Do parse the line */
    final IRecord record = createNewRecord();
    for( int i = 0; i < m_components.length; i++ )
      addValue( record, i, tokens, filename, lineNumber );

    m_currentRecord++;
  }

  private void addValue( final IRecord record, final int i, final String[] tokens, final String filename, final int lineNumber )
  {
    /* Skip null components */
    final String component = m_components[i];
    if( component == null )
      return;

    try
    {
      final BigDecimal value = parseToken( tokens[i] );
      record.setValue( m_componentIndex[i], value );
    }
    catch( final NumberFormatException nfe )
    {
      /* A good line but bad content. Give user a hint that something might be wrong. */
      final String msg = Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeProcessor.21" ); //$NON-NLS-1$
      m_log.log( false, msg, filename, lineNumber, tokens[i] ); //$NON-NLS-1$
    }
  }

  private IRecord createNewRecord( )
  {
    if( m_currentRecord < m_existingRecords.length )
      return m_existingRecords[m_currentRecord];

    final IRecord record = m_result.createRecord();
    m_result.add( record );

    return record;
  }

  private BigDecimal parseToken( final String token )
  {
    return new BigDecimal( token.replace( 'D', 'E' ) );
  }

  private boolean readStationAndBankfull( final String line )
  {
    if( m_station != null )
      return false;

    if( m_stationAndBankFullPattern == null )
      return false;

    final Matcher matcher = m_stationAndBankFullPattern.matcher( line );
    if( matcher.matches() )
    {
      final String stationText = matcher.group( 1 );
      m_station = new BigDecimal( stationText.trim() );

      final String bankfullText = matcher.group( 3 );
      m_bankfull = new BigDecimal( bankfullText.trim() );

      return true;
    }
    else
      return false;
  }
}