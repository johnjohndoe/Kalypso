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
package org.kalypso.wspwin.core;

import java.io.File;
import java.io.IOException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;

import org.kalypso.commons.java.io.FileUtilities;

/**
 * Represents one line of a wsp.cfg file.
 * @author thuel2
 */
public class ZustandBean
{
  private final String m_name;

  private final String m_waterName;

  private final String m_fileName;

  private double m_startStation;

  private double m_endStation;

  private final Date m_date;

  public ZustandBean( final String name, final String waterName, final String fileName, final double startStation, final double endStation, final Date date )
  {
    m_name = name;
    m_waterName = waterName;
    m_fileName = fileName;
    m_startStation = startStation;
    m_endStation = endStation;
    m_date = date;
  }

  public Date getDate( )
  {
    return m_date;
  }

  public double getEndStation( )
  {
    return m_endStation;
  }

  public String getFileName( )
  {
    return m_fileName;
  }

  public String getName( )
  {
    return m_name;
  }

  public double getStartStation( )
  {
    return m_startStation;
  }

  public String getWaterName( )
  {
    return m_waterName;
  }

  public WspWinZustand readZustand( final File profDir, final WspWinProfProj profProj ) throws IOException, ParseException
  {
    final WspWinZustand zustand = new WspWinZustand( this, profProj );
    zustand.read( new File( profDir, getFileName() ) );
    return zustand;
  }

  private File getZustandFile( final File profDir, final String suffix )
  {
    final String strFileName = getFileName();
    final String strBaseName = FileUtilities.nameWithoutExtension( strFileName );
    return new File( profDir, strBaseName + "." + suffix );
  }

  public RunOffEventBean[] readRunOffs( final File profDir ) throws ParseException, IOException
  {
    final File qwtFile = getZustandFile( profDir, "qwt" ); //$NON-NLS-1$
    return RunOffEventBean.read( qwtFile );
  }

  public RunOffEventBean[] readWspFixes( final File profDir ) throws ParseException, IOException
  {
    final File wsfFile = getZustandFile( profDir, "wsf" ); //$NON-NLS-1$
    return RunOffEventBean.read( wsfFile );
  }

  public LocalEnergyLossBean[] readLocalEnergyLosses( final File profDir ) throws ParseException, IOException
  {
    final File lelFile = getZustandFile( profDir, "psi" ); //$NON-NLS-1$
    return LocalEnergyLossBean.read( lelFile );
  }

  public CalculationBean[] readCalculations( final File profDir ) throws ParseException, IOException
  {
    final File berFile = getZustandFile( profDir, "ber" ); //$NON-NLS-1$
    return CalculationBean.readBerFile( berFile );
  }

  public String formatLine( )
  {
    final String waterName = ProfileBean.shortenName( m_waterName );
    final String stateName = ProfileBean.shortenName( m_name );
    final String dateText = new SimpleDateFormat( "dd.MM.yyyy" ).format( m_date );
    return String.format( Locale.US, "%-14s %-14s %s  %13.6f  %13.6f  %13s", waterName, stateName, dateText, m_startStation, m_endStation, m_fileName );
  }

  public void setStartStation( final double startStation )
  {
    m_startStation = startStation;
  }

  public void setEndStation( final double endStation )
  {
    m_endStation = endStation;
  }
}