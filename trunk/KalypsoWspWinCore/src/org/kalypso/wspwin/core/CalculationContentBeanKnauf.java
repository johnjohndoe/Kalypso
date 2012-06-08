/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.wspwin.core;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;
import java.io.PrintWriter;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;

/**
 * represents the contents of one calculation (.001, .002, ... file)
 * 
 * @author Belger
 */
public class CalculationContentBeanKnauf extends AbstractCalculationContentBean
{
  public enum START_CONDITION
  {
    WSP, // Anfangswasserspiegel
    HNORM, // stationär gleichförmiges Gefälle
    HGRENZ
    // Grenztiefe
  }

  public enum LAW_OF_FLOW
  {
    unknown,
    MANNING_STRICKLER,
    PRANDTL_COLEBROOK,
    EINSTEIN,
    PC_BEWUCHS_KAISER,
    PC_BEWUCHS_NUDING,
    PC_BEWUCHS_MERTENS,
    PC_BEWUCHS_PASCHE
  }

  public static final String CODE_LP = "lp"; //$NON-NLS-1$

  private final BigDecimal m_startStation;

  private final BigDecimal m_endStation;

  private final START_CONDITION m_startCondition;

  private final BigDecimal m_startWaterlevel;

  private final BigDecimal m_stationarySlope;

  private final String m_runoffName;

  private final BigDecimal m_qMin;

  private final BigDecimal m_qMax;

  private final BigDecimal m_qStep;

  private final LAW_OF_FLOW m_lawOfFlow;

  private final String[] m_exportLines = new String[8];

  private final String m_line25;

  private final String m_line26;

  private final String[] m_endLines;

  private final int m_startConditionType;

  public static CalculationContentBeanKnauf read( final CalculationBean bean, final File file ) throws IOException
  {
    try (LineNumberReader lnr = new LineNumberReader( new FileReader( file ) ))
    {
      return readContent( bean, lnr );
    }
  }

  private static CalculationContentBeanKnauf readContent( final CalculationBean bean, final LineNumberReader lnr ) throws IOException
  {
    // Name: skip, was already read from .ber
    expectNextLine( lnr );

    final BigDecimal startStation = readBigDecimal( lnr );
    final BigDecimal endStation = readBigDecimal( lnr );

    // Skip 5 empty lines
    for( int i = 0; i < 5; i++ )
      expectNextLine( lnr );

    final String startConditionName = expectNextLine( lnr );
    final START_CONDITION startCondition = START_CONDITION.valueOf( startConditionName );

    final BigDecimal startWaterlevel = readBigDecimal( lnr );
    final BigDecimal stationarySlope = readBigDecimal( lnr );
    // 0 or 1, depending on startCondiotion
    final int startConditionType = readInt( lnr );

    final String runoffName = expectNextLine( lnr );

    // Skip 8 lines concerning how to export results
    final String[] exportLines = new String[8];
    for( int i = 0; i < exportLines.length; i++ )
      exportLines[i] = expectNextLine( lnr );

    final BigDecimal qMin = readBigDecimal( lnr );
    final BigDecimal qMax = readBigDecimal( lnr );
    final BigDecimal qStep = readBigDecimal( lnr );

    // Ausdruck Anzahl Profile pro Seite
    final String line25 = expectNextLine( lnr );
    final String line26 = expectNextLine( lnr );

    // Fließgesetz
    final int lawOfFlowIndex = readInt( lnr );
    LAW_OF_FLOW lawOfFlow;
    if( lawOfFlowIndex >= LAW_OF_FLOW.values().length )
      lawOfFlow = LAW_OF_FLOW.unknown;
    else
      lawOfFlow = LAW_OF_FLOW.values()[lawOfFlowIndex - 1];

    /* Read end of files */
    final Collection<String> endLinesList = new ArrayList<>();
    while( lnr.ready() )
      endLinesList.add( expectNextLine( lnr ) );
    final String[] endLines = endLinesList.toArray( new String[endLinesList.size()] );

    return new CalculationContentBeanKnauf( bean, startStation, endStation, startCondition, startConditionType, startWaterlevel, stationarySlope, runoffName, qMin, qMax, qStep, lawOfFlow, exportLines, line25, line26, endLines );
  }

  public CalculationContentBeanKnauf( final CalculationBean bean, final BigDecimal startStation, final BigDecimal endStation, final START_CONDITION startCondition, final int startConditionType, final BigDecimal startWaterlevel, final BigDecimal stationarySlope, final String runoffName, final BigDecimal qMin, final BigDecimal qMax, final BigDecimal qStep, final LAW_OF_FLOW lawOfFlow, final String[] exportLines, final String line25, final String line26, final String[] endLines )
  {
    super( bean );

    m_startStation = startStation;
    m_endStation = endStation;
    m_startCondition = startCondition;
    m_startConditionType = startConditionType;
    m_startWaterlevel = startWaterlevel;
    m_stationarySlope = stationarySlope;
    m_runoffName = runoffName;
    m_qMin = qMin;
    m_qMax = qMax;
    m_qStep = qStep;
    m_lawOfFlow = lawOfFlow;
    m_line25 = line25;
    m_line26 = line26;
    m_endLines = endLines;

    for( int i = 0; i < m_exportLines.length; i++ )
      m_exportLines[i] = exportLines[i];
  }

  public LAW_OF_FLOW getLawOfFlow( )
  {
    return m_lawOfFlow;
  }

  public BigDecimal getStartStation( )
  {
    return m_startStation;
  }

  public BigDecimal getEndStation( )
  {
    return m_endStation;
  }

  public START_CONDITION getStartCondition( )
  {
    return m_startCondition;
  }

  public BigDecimal getStartWaterlevel( )
  {
    return m_startWaterlevel;
  }

  public BigDecimal getStationarySlope( )
  {
    return m_stationarySlope;
  }

  public String getRunoffName( )
  {
    return m_runoffName;
  }

  public BigDecimal getQMax( )
  {
    return m_qMax;
  }

  public BigDecimal getQMin( )
  {
    return m_qMin;
  }

  public BigDecimal getQStep( )
  {
    return m_qStep;
  }

  @Override
  public void write( final File profDir ) throws IOException
  {
    final CalculationBean calculationBean = getCalculationBean();
    final String fileName = calculationBean.getFileName();
    final File file = new File( profDir, fileName );

    try (PrintWriter pw = new PrintWriter( file ))
    {
      pw.println( calculationBean.getName() );

      printBigDecimal( pw, m_startStation );
      printBigDecimal( pw, m_endStation );

      // Skip 5 empty lines
      for( int i = 0; i < 5; i++ )
        pw.println();

      pw.println( m_startCondition.name() );

      printBigDecimal( pw, m_startWaterlevel );
      printBigDecimal( pw, m_stationarySlope );

      // 0 or 1, depending on startCondiotion
      pw.println( m_startConditionType );

      pw.println( m_runoffName );

      // Skip 8 lines concerning how to export results
      for( int i = 0; i < 8; i++ )
        pw.println( m_exportLines[i] );

      printBigDecimal( pw, m_qMin );
      printBigDecimal( pw, m_qMax );
      printBigDecimal( pw, m_qStep );

      // Ausdruck Anzahl Profile pro Seite
      pw.println( m_line25 );
      pw.println( m_line26 );

      // Fließgesetz
      pw.println( m_lawOfFlow.ordinal() + 1 );

      // Thats it, we are not interested in all the rest
      for( final String endLine : m_endLines )
        pw.println( endLine );
    }
  }

  public int getStartConditionType( )
  {
    return m_startConditionType;
  }

  public String[] getExportLines( )
  {
    return m_exportLines;
  }

  public String getLine25( )
  {
    return m_line25;
  }

  public String getLine26( )
  {
    return m_line26;
  }

  public String[] getEndLines( )
  {
    return m_endLines;
  }
}