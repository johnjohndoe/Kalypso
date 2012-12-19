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

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.LineNumberReader;
import java.math.BigDecimal;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.StringTokenizer;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.SystemUtils;
import org.kalypso.wspwin.core.i18n.Messages;

/**
 * Represents the contents of a .str file
 *
 * @author Gernot Belger
 */
public class WspWinZustand
{
  private final List<ProfileBean> m_profileBeans = new ArrayList<>();

  private final List<ZustandSegmentBean> m_segmentBeans = new ArrayList<>();

  private final Collection<RunOffEventBean> m_runoffs = new ArrayList<>();

  private final Collection<RunOffEventBean> m_wspFixes = new ArrayList<>();

  private final Collection<LocalEnergyLossBean> m_losses = new ArrayList<>();

  private final Collection<ICalculationContentBean> m_calculations = new ArrayList<>();

  private final ZustandBean m_bean;

  public WspWinZustand( final ZustandBean bean )
  {
    m_bean = bean;
  }

  public ZustandBean getBean( )
  {
    return m_bean;
  }

  public ProfileBean[] getProfileBeans( )
  {
    return m_profileBeans.toArray( new ProfileBean[m_profileBeans.size()] );
  }

  public ZustandSegmentBean[] getSegmentBeans( )
  {
    return m_segmentBeans.toArray( new ZustandSegmentBean[m_segmentBeans.size()] );
  }

  public void addProfile( final ProfileBean profile )
  {
    m_profileBeans.add( profile );
  }

  public void addSegment( final ZustandSegmentBean segment )
  {
    m_segmentBeans.add( segment );
  }

  /**
   * Reads content from .str file
   */
  public void read( final File strFile ) throws IOException, ParseException
  {
    LineNumberReader reader = null;
    try
    {
      reader = new LineNumberReader( new FileReader( strFile ) );

      final int[] counts = WspWinProfProj.readStrHeader( reader );
      final int profilCount = counts[0];
      final int segmentCount = counts[1];

      final ProfileBean[] profileBeans = ProfileBean.readProfiles( reader, profilCount );
      for( final ProfileBean profileBean : profileBeans )
        addProfile( profileBean );

      final ZustandSegmentBean[] segmentBeans = readZustandSegments( reader, segmentCount, strFile.getName() );
      for( final ZustandSegmentBean zustandSegmentBean : segmentBeans )
        addSegment( zustandSegmentBean );
    }
    finally
    {
      IOUtils.closeQuietly( reader );
    }
  }

  private static ZustandSegmentBean[] readZustandSegments( final LineNumberReader reader, final int segmentCount, final String filename ) throws ParseException, IOException
  {
    final List<ZustandSegmentBean> beans = new ArrayList<>( 20 );

    int readSegments = 0;
    while( reader.ready() )
    {
      final String line = reader.readLine();
      /* Skip empty lines; we have WspWin projects with and without a separating empty line */
      if( StringUtils.isBlank( line ) )
        continue;

      final StringTokenizer tokenizer = new StringTokenizer( line );
      if( tokenizer.countTokens() != 7 )
        throw new ParseException( Messages.getString( "org.kalypso.wspwin.core.WspWinZustand.2", filename, reader.getLineNumber() ), reader.getLineNumber() ); //$NON-NLS-1$

      try
      {
        final BigDecimal stationFrom = new BigDecimal( tokenizer.nextToken() );
        final BigDecimal stationTo = new BigDecimal( tokenizer.nextToken() );
        final double distanceVL = Double.parseDouble( tokenizer.nextToken() );
        final double distanceHF = Double.parseDouble( tokenizer.nextToken() );
        final double distanceVR = Double.parseDouble( tokenizer.nextToken() );

        final String fileNameFrom = tokenizer.nextToken();
        final String fileNameTo = tokenizer.nextToken();

        final ZustandSegmentBean bean = new ZustandSegmentBean( stationFrom, stationTo, fileNameFrom, fileNameTo, distanceVL, distanceHF, distanceVR );
        beans.add( bean );

        readSegments++;
      }
      catch( final NumberFormatException e )
      {
        e.printStackTrace();
        throw new ParseException( Messages.getString( "org.kalypso.wspwin.core.WspWinZustand.3", filename, reader.getLineNumber() ), reader.getLineNumber() ); //$NON-NLS-1$
      }
    }

    if( readSegments != segmentCount )
      throw new ParseException( Messages.getString( "org.kalypso.wspwin.core.WspWinZustand.1", filename, reader.getLineNumber() ), reader.getLineNumber() ); //$NON-NLS-1$

    return beans.toArray( new ZustandSegmentBean[beans.size()] );
  }

  void updateSegmentInfo( )
  {
    m_segmentBeans.clear();

    BigDecimal minStation = new BigDecimal( Double.MAX_VALUE );
    BigDecimal maxStation = new BigDecimal( -Double.MAX_VALUE );
    for( int i = 0; i < m_profileBeans.size() - 1; i++ )
    {
      final ProfileBean fromProfile = m_profileBeans.get( i );
      final ProfileBean toProfile = m_profileBeans.get( i + 1 );

      final BigDecimal stationFrom = fromProfile.getStation();
      final BigDecimal stationTo = toProfile.getStation();

      /* calculate global min/max station */
      minStation = minStation.min( stationFrom );
      minStation = minStation.min( stationTo );
      maxStation = maxStation.max( stationFrom );
      maxStation = maxStation.max( stationTo );

      /* recalculate distance to next profile */
      // REMARK: special handling for the distance of Profiles with 'Mehrfeld-Code': here the distance must be
      // the distance to the next real profile (i.e. the next profile without Mehfeld-Code and with a different station)
      final ProfileBean nextRealProfile = findNextRealProfile( fromProfile, i + 1 );

      final double distance;
      if( nextRealProfile == null )
      {
        // should never happen
        distance = Math.abs( stationTo.doubleValue() - stationFrom.doubleValue() );
        System.out.println( "Error in zustand" ); //$NON-NLS-1$
      }
      else
      {
        final BigDecimal nextStation = nextRealProfile.getStation();
        distance = Math.abs( nextStation.doubleValue() - stationFrom.doubleValue() );
      }

      final String fileNameFrom = fromProfile.getFileName();
      final String fileNameTo = toProfile.getFileName();

      final ZustandSegmentBean segment = new ZustandSegmentBean( stationFrom, stationTo, fileNameFrom, fileNameTo, distance, distance, distance );
      m_segmentBeans.add( segment );
    }

    m_bean.setStartStation( minStation );
    m_bean.setEndStation( maxStation );
  }

  private ProfileBean findNextRealProfile( final ProfileBean fromProfile, final int startIndex )
  {
    final String fromCode = fromProfile.getMehrfeldCode();
    final BigDecimal fromStation = fromProfile.getStation();

    for( int i = startIndex; i < m_profileBeans.size(); i++ )
    {
      final ProfileBean nextProfile = m_profileBeans.get( i );
      final String nextCode = nextProfile.getMehrfeldCode();
      final BigDecimal nextStation = nextProfile.getStation();

      /* If stations are different, we assume that it is not a mehfeld-bridge */
      if( nextStation.compareTo( fromStation ) != 0 )
        return nextProfile;

      /*
       * If codes are euqal, both are probably code '0' (two codes in same bridge are not allowed), so we assume it is
       * not a mehfeld bridge
       */
      if( ObjectUtils.equals( nextCode, fromCode ) )
        return nextProfile;

      /* continue search */
    }

    return null;
  }

  public void write( final File wspwinDir ) throws IOException
  {
    final WspWinProject wspWinProject = new WspWinProject( wspwinDir );
    final File profDir = wspWinProject.getProfDir();

    final File strFile = new File( profDir, m_bean.getFileName() );

    final BufferedWriter pw = new BufferedWriter( new FileWriter( strFile ) );

    /* Header */
    final ZustandBean bean = getBean();
    final String waterName = ProfileBean.shortenName( bean.getWaterName(), ProfileBean.MAX_WATERNAME_LENGTH );
    final String name = ProfileBean.shortenName( bean.getName(), ProfileBean.MAX_STATENAME_LENGTH );
    pw.append( String.format( "%d %d %s %s%n", m_profileBeans.size(), m_segmentBeans.size(), waterName, name ) ); //$NON-NLS-1$

    /* Profiles */
    for( final ProfileBean profile : m_profileBeans )
      pw.append( profile.formatStrLine() ).append( SystemUtils.LINE_SEPARATOR );

    pw.append( SystemUtils.LINE_SEPARATOR );

    /* Segments */
    for( final ZustandSegmentBean segment : m_segmentBeans )
      pw.append( segment.formatLine() ).append( SystemUtils.LINE_SEPARATOR );

    pw.close();
  }

  public void addRunoff( final RunOffEventBean runOff )
  {
    m_runoffs.add( runOff );
  }

  public void addWspFix( final RunOffEventBean wspFix )
  {
    m_wspFixes.add( wspFix );
  }

  public void addLoss( final LocalEnergyLossBean loss )
  {
    m_losses.add( loss );
  }

  public void addCalculation( final ICalculationContentBean calculation )
  {
    m_calculations.add( calculation );
  }

  public RunOffEventBean[] getRunOffEvents( )
  {
    return m_runoffs.toArray( new RunOffEventBean[m_runoffs.size()] );
  }

  public RunOffEventBean[] getWspFixations( )
  {
    return m_wspFixes.toArray( new RunOffEventBean[m_wspFixes.size()] );
  }

  public LocalEnergyLossBean[] getLosses( )
  {
    return m_losses.toArray( new LocalEnergyLossBean[m_losses.size()] );
  }

  public ICalculationContentBean[] getCalculations( )
  {
    return m_calculations.toArray( new ICalculationContentBean[m_calculations.size()] );
  }

  @Override
  public String toString( )
  {
    final StringBuilder buffer = new StringBuilder();

    buffer.append( m_bean.toString() );

    return buffer.toString();
  }
}