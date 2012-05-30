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

  private BigDecimal m_startStation;

  private BigDecimal m_endStation;

  private START_CONDITION m_startCondition;

  private BigDecimal m_startWaterlevel;

  private BigDecimal m_stationarySlope;

  private String m_runoffName;

  private BigDecimal m_qMin;

  private BigDecimal m_qMax;

  private BigDecimal m_qStep;

  private LAW_OF_FLOW m_lawOfFlow;

  private final String[] m_exportLines = new String[8];

  private String m_line25;

  private String m_line26;

  private String[] m_endLines;

  private int m_startConditionType;

  public CalculationContentBeanKnauf( final CalculationBean bean )
  {
    super( bean );
  }

  public static CalculationContentBeanKnauf read( final CalculationBean bean, final File file ) throws IOException
  {
    final CalculationContentBeanKnauf calculation = new CalculationContentBeanKnauf( bean );

    try (LineNumberReader lnr = new LineNumberReader( new FileReader( file ) ))
    {
      calculation.readContent( lnr );

      return calculation;
    }
  }

  private void readContent( final LineNumberReader lnr ) throws IOException
  {
    // Name: skip, was already read from .ber
    expectNextLine( lnr );

    m_startStation = readBigDecimal( lnr );
    m_endStation = readBigDecimal( lnr );

    // Skip 5 empty lines
    for( int i = 0; i < 5; i++ )
      expectNextLine( lnr );

    final String startConditionName = expectNextLine( lnr );
    m_startCondition = START_CONDITION.valueOf( startConditionName );

    m_startWaterlevel = readBigDecimal( lnr );
    m_stationarySlope = readBigDecimal( lnr );
    // 0 or 1, depending on startCondiotion
    m_startConditionType = readInt( lnr );

    m_runoffName = expectNextLine( lnr );

    // Skip 8 lines concerning how to export results
    for( int i = 0; i < 8; i++ )
      m_exportLines[i] = expectNextLine( lnr );

    m_qMin = readBigDecimal( lnr );
    m_qMax = readBigDecimal( lnr );
    m_qStep = readBigDecimal( lnr );

    // Ausdruck Anzahl Profile pro Seite
    m_line25 = expectNextLine( lnr );
    m_line26 = expectNextLine( lnr );

    // Fließgesetz
    final int lawOfFlowIndex = readInt( lnr );
    if( lawOfFlowIndex >= LAW_OF_FLOW.values().length )
      m_lawOfFlow = LAW_OF_FLOW.unknown;
    else
      m_lawOfFlow = LAW_OF_FLOW.values()[lawOfFlowIndex - 1];

    /* Read end of files */
    final Collection<String> endLines = new ArrayList<>();
    while( lnr.ready() )
      endLines.add( expectNextLine( lnr ) );
    m_endLines = endLines.toArray( new String[endLines.size()] );
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
}