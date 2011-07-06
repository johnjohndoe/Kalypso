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
package org.kalypso.model.wspm.tuhh.core.wspwin;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.math.BigDecimal;
import java.util.Arrays;

import org.apache.commons.io.IOUtils;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.serializer.IProfilSink;
import org.kalypso.model.wspm.tuhh.core.gml.ITuhhCalculation.FLIESSGESETZ;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhSegmentStationComparator;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhStationRange;
import org.kalypso.model.wspm.tuhh.core.i18n.Messages;
import org.kalypso.model.wspm.tuhh.core.wspwin.prf.PrfSink;

/**
 * @author Gernot Belger
 */
public class TuhhZustandWriter
{
  private final TuhhReach m_reach;

  private final TuhhReachProfileSegment[] m_segments;

  private final TuhhStationRange m_stationRange;

  private final String m_roughnessType;

  public TuhhZustandWriter( final TuhhCalculation calculation, final TuhhStationRange stationRange )
  {
    m_stationRange = stationRange;
    m_reach = calculation.getReach();
    m_segments = m_reach.getReachProfileSegments();

    final TuhhSegmentStationComparator stationComparator = new TuhhSegmentStationComparator( stationRange.getDirection() );
    final org.kalypso.model.wspm.tuhh.core.gml.ITuhhCalculation.FLIESSGESETZ fliessgesetz = calculation.getFliessgesetz();
    m_roughnessType = getRoughnessForFG( fliessgesetz );

    Arrays.sort( m_segments, stationComparator );
  }

  public void write( final File zustFile ) throws IOException
  {
    PrintWriter zustWriter = null;
    try
    {
      zustFile.getParentFile().mkdirs();
      zustWriter = new PrintWriter( new BufferedWriter( new FileWriter( zustFile ) ) );

      final File profDir = zustFile.getParentFile();

      int fileCount = 0;
      for( final TuhhReachProfileSegment segment : m_segments )
      {
        writeProfile( zustWriter, segment, profDir, fileCount++ );
      }

      if( fileCount == 0 )
      {
        final String error = String.format( Messages.getString( "WspWinExporter.4" ) ); //$NON-NLS-1$
        throw new IllegalArgumentException( error );
      }

      zustWriter.flush();
      zustWriter.close();
    }
    finally
    {
      IOUtils.closeQuietly( zustWriter );
    }
  }

  private void writeProfile( final PrintWriter zustWriter, final TuhhReachProfileSegment segment, final File profDir, final int fileCount ) throws IOException
  {
    final BigDecimal station = segment.getStation();
    if( m_stationRange.isOutside( station ) )
      return;

    PrintWriter prfWriter = null;
    try
    {
      final String prfName = "Profil_" + fileCount + ".prf"; //$NON-NLS-1$ //$NON-NLS-2$
      final double exportStation = station.doubleValue() * m_stationRange.getExportSign();

      zustWriter.print( prfName );
      zustWriter.print( " " ); //$NON-NLS-1$
      zustWriter.println( exportStation );

      final IProfileFeature profileMember = segment.getProfileMember();

      final IProfil profil = profileMember.getProfil();
      profil.setStation( exportStation );

      final File outPrfFile = new File( profDir, prfName );
      prfWriter = new PrintWriter( outPrfFile );
      final IProfilSink ps = new PrfSink( m_roughnessType );
      ps.write( new IProfil[] { profil }, prfWriter );
      prfWriter.flush();
      prfWriter.close();
    }
    finally
    {
      IOUtils.closeQuietly( prfWriter );
    }
    prfWriter.close();
  }

  private static String getRoughnessForFG( final FLIESSGESETZ fg )
  {
    if( FLIESSGESETZ.DARCY_WEISBACH_MIT_FORMEINFLUSS.equals( fg ) )
      return IWspmConstants.POINT_PROPERTY_RAUHEIT_KS;
    else if( FLIESSGESETZ.DARCY_WEISBACH_OHNE_FORMEINFLUSS.equals( fg ) )
      return IWspmConstants.POINT_PROPERTY_RAUHEIT_KS;
    else if( FLIESSGESETZ.MANNING_STRICKLER.equals( fg ) )
      return IWspmConstants.POINT_PROPERTY_RAUHEIT_KST;
    else
      return ""; //$NON-NLS-1$
  }
}