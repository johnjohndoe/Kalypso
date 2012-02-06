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

import org.apache.commons.io.IOUtils;

/**
 * represents the contents of one calculation (.001, .002, ... file)
 * 
 * @author Belger
 */
public class CalculationContentBeanKnauf
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

  private double m_startStation;

  private double m_endStation;

  private START_CONDITION m_startCondition;

  private double m_startWaterlevel;

  private double m_stationarySlope;

  private String m_runoffName;

  private double m_qMin;

  private double m_qMax;

  private double m_qStep;

  private LAW_OF_FLOW m_lawOfFlow;

  public void read( final File file ) throws IOException
  {
    LineNumberReader lnr = null;

    try
    {
      lnr = new LineNumberReader( new FileReader( file ) );

      readContent( lnr );
    }
    finally
    {
      IOUtils.closeQuietly( lnr );
    }
  }

  private void readContent( final LineNumberReader lnr ) throws IOException
  {
    // Name: skip, was already read from .ber
    CalculationContentBeanPasche.expectNextLine( lnr );

    m_startStation = CalculationContentBeanPasche.readDouble( lnr );
    m_endStation = CalculationContentBeanPasche.readDouble( lnr );

    // Skip 5 empty lines
    for( int i = 0; i < 5; i++ )
      CalculationContentBeanPasche.expectNextLine( lnr );

    final String startConditionName = CalculationContentBeanPasche.expectNextLine( lnr );
    m_startCondition = START_CONDITION.valueOf( startConditionName );

    m_startWaterlevel = CalculationContentBeanPasche.readDouble( lnr );
    m_stationarySlope = CalculationContentBeanPasche.readDouble( lnr );
    // 0 or 1, depending on startCondiotion
    CalculationContentBeanPasche.expectNextLine( lnr );

    m_runoffName = CalculationContentBeanPasche.expectNextLine( lnr );

    // Skip 8 lines concerning how to export results
    for( int i = 0; i < 8; i++ )
      CalculationContentBeanPasche.expectNextLine( lnr );

    m_qMin = CalculationContentBeanPasche.readDouble( lnr );
    m_qMax = CalculationContentBeanPasche.readDouble( lnr );
    m_qStep = CalculationContentBeanPasche.readDouble( lnr );

    // Ausdruck Anzahl Profile pro Seite
    CalculationContentBeanPasche.expectNextLine( lnr );
    CalculationContentBeanPasche.expectNextLine( lnr );

    // Fließgesetz
    final int lawOfFlowIndex = CalculationContentBeanPasche.readInt( lnr );
    if( lawOfFlowIndex >= LAW_OF_FLOW.values().length )
      m_lawOfFlow = LAW_OF_FLOW.unknown;
    else
      m_lawOfFlow = LAW_OF_FLOW.values()[1];

    // Thats it, we are not interested in all the rest

    // 0
    // 0
    // 0
    // 0.005
    // 0.100
    // 1.310
    // 1.500
    // 0
    // NASIM 0
    // 0
    // 0
    // 0
    // 67
    // 0
    // 0
    // 0
    // 0
    // 1.0000
  }

  public LAW_OF_FLOW getLawOfFlow( )
  {
    return m_lawOfFlow;
  }

  public double getStartStation( )
  {
    return m_startStation;
  }

  public double getEndStation( )
  {
    return m_endStation;
  }

  public START_CONDITION getStartCondition( )
  {
    return m_startCondition;
  }

  public double getStartWaterlevel( )
  {
    return m_startWaterlevel;
  }

  public double getStationarySlope( )
  {
    return m_stationarySlope;
  }

  public String getRunoffName( )
  {
    return m_runoffName;
  }

  public double getQMax( )
  {
    return m_qMax;
  }

  public double getQMin( )
  {
    return m_qMin;
  }

  public double getQStep( )
  {
    return m_qStep;
  }
}