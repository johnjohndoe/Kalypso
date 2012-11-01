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
import java.math.BigDecimal;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.Locale;

import org.apache.commons.lang3.builder.ToStringBuilder;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.wspwin.core.WspCfg.TYPE;

/**
 * Represents one line of a wsp.cfg file.
 * 
 * @author thuel2
 */
public class ZustandBean
{
  private final String m_name;

  private final String m_waterName;

  private final String m_fileName;

  private BigDecimal m_startStation;

  private BigDecimal m_endStation;

  private final Date m_date;

  public ZustandBean( final String name, final String waterName, final String fileName, final BigDecimal startStation, final BigDecimal endStation, final Date date )
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

  public BigDecimal getEndStation( )
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

  public BigDecimal getStartStation( )
  {
    return m_startStation;
  }

  public String getWaterName( )
  {
    return m_waterName;
  }

  public WspWinZustand readZustand( final TYPE projectType, final File profDir ) throws IOException, ParseException
  {
    final WspWinZustand zustand = new WspWinZustand( this );
    zustand.read( new File( profDir, getFileName() ) );

    // TODO: improve error handling: import completely fails if we get an error in one of the sub-files

    final RunOffEventBean[] runOffs = readRunOffs( profDir );
    for( final RunOffEventBean runOff : runOffs )
      zustand.addRunoff( runOff );

    final RunOffEventBean[] wspFixes = readWspFixes( profDir );
    for( final RunOffEventBean wspFix : wspFixes )
      zustand.addWspFix( wspFix );

    final LocalEnergyLossBean[] localEnergyLosses = readLocalEnergyLosses( profDir );
    for( final LocalEnergyLossBean loss : localEnergyLosses )
      zustand.addLoss( loss );

    final ICalculationContentBean[] calculations = readCalculations( projectType, profDir );
    for( final ICalculationContentBean calculation : calculations )
      zustand.addCalculation( calculation );

    return zustand;
  }

  private RunOffEventBean[] readRunOffs( final File profDir ) throws ParseException, IOException
  {
    final File qwtFile = new File( profDir, getRunoffFilename() );
    return RunOffEventBean.read( qwtFile );
  }

  private void writeRunOffs( final File profDir, final RunOffEventBean[] runoff ) throws IOException
  {
    final File qwtFile = new File( profDir, getRunoffFilename() ); //$NON-NLS-1$
    RunOffEventBean.write( qwtFile, runoff );
  }

  private RunOffEventBean[] readWspFixes( final File profDir ) throws ParseException, IOException
  {
    final File wsfFile = new File( profDir, getWspFixFilename() ); //$NON-NLS-1$
    return RunOffEventBean.read( wsfFile );
  }

  private void writeWspFixes( final File profDir, final RunOffEventBean[] fixation ) throws IOException
  {
    final File wsfFile = new File( profDir, getWspFixFilename() ); //$NON-NLS-1$
    RunOffEventBean.write( wsfFile, fixation );
  }

  private void writeLosses( final File profDir, final LocalEnergyLossBean[] losses ) throws IOException
  {
    /* Do not write empty loss file */
    if( losses.length == 0 )
      return;

    final File lelFile = new File( profDir, getLossFilename() );
    LocalEnergyLossBean.write( lelFile, losses );
  }

  private void writeCalculations( final File profDir, final ICalculationContentBean[] calculations ) throws IOException
  {
    final CalculationBean[] beans = new CalculationBean[calculations.length];
    for( int i = 0; i < calculations.length; i++ )
    {
      beans[i] = calculations[i].getCalculationBean();

      calculations[i].write( profDir );
    }

    final File berFile = new File( profDir, getCalculationsFilename() ); //$NON-NLS-1$
    CalculationBean.writeBerFile( berFile, beans );
  }

  private LocalEnergyLossBean[] readLocalEnergyLosses( final File profDir ) throws ParseException, IOException
  {
    final File lelFile = new File( profDir, getLossFilename() );
    return LocalEnergyLossBean.read( lelFile );
  }

  public String getLossFilename( )
  {
    return getZustandFile( "psi" ); //$NON-NLS-1$
  }

  public String getRunoffFilename( )
  {
    return getZustandFile( "qwt" ); //$NON-NLS-1$
  }

  public String getWspFixFilename( )
  {
    return getZustandFile( "wsf" ); //$NON-NLS-1$
  }

  private String getCalculationsFilename( )
  {
    return getZustandFile( "BER" ); //$NON-NLS-1$
  }

  private ICalculationContentBean[] readCalculations( final TYPE projectType, final File profDir ) throws ParseException, IOException
  {
    /* Read ber file */
    final File berFile = new File( profDir, getCalculationsFilename() );
    final CalculationBean[] calculationBeans = CalculationBean.readBerFile( berFile );

    final Collection<ICalculationContentBean> contentBeans = new ArrayList<>( calculationBeans.length );

    for( final CalculationBean calculationBean : calculationBeans )
    {
      try
      {
        final ICalculationContentBean contentBean = calculationBean.readContent( projectType, profDir );
        contentBeans.add( contentBean );
      }
      catch( final IOException e )
      {
        e.printStackTrace();
        // TODO: collect status and inform user about failed calculation contents
      }
    }

    return contentBeans.toArray( new ICalculationContentBean[contentBeans.size()] );
  }

  private String getZustandFile( final String suffix )
  {
    final String strFileName = getFileName();
    final String strBaseName = FileUtilities.nameWithoutExtension( strFileName );
    return strBaseName + "." + suffix; //$NON-NLS-1$
  }

  public String formatLine( )
  {
    final String waterName = ProfileBean.shortenName( m_waterName, ProfileBean.MAX_WATERNAME_LENGTH );
    final String stateName = ProfileBean.shortenName( m_name, ProfileBean.MAX_STATENAME_LENGTH );
    final String dateText = new SimpleDateFormat( "d.M.yyyy" ).format( m_date ); //$NON-NLS-1$
    return String.format( Locale.US, "%-14s %-14s %-10s  %13.6f  %13.6f  %13s", waterName, stateName, dateText, m_startStation, m_endStation, m_fileName ); //$NON-NLS-1$
  }

  public void writeZustand( final File wspwinDir, final WspWinZustand zustand ) throws IOException
  {
    zustand.write( wspwinDir );

    final WspWinProject wspWinProject = new WspWinProject( wspwinDir );
    final File profDir = wspWinProject.getProfDir();

    writeRunOffs( profDir, zustand.getRunOffEvents() );
    writeWspFixes( profDir, zustand.getWspFixations() );

    writeCalculations( profDir, zustand.getCalculations() );
    writeLosses( profDir, zustand.getLosses() );
  }

  void setStartStation( final BigDecimal startStation )
  {
    m_startStation = startStation;
  }

  void setEndStation( final BigDecimal endStation )
  {
    m_endStation = endStation;
  }

  @Override
  public String toString( )
  {
    return ToStringBuilder.reflectionToString( this );
  }
}