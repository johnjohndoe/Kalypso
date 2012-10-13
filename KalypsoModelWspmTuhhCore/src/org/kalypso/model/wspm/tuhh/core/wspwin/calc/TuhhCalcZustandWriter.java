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
package org.kalypso.model.wspm.tuhh.core.wspwin.calc;

import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.Arrays;

import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhSegmentStationComparator;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhStationRange;
import org.kalypso.model.wspm.tuhh.core.i18n.Messages;
import org.kalypso.model.wspm.tuhh.core.wspwin.WspWinProfileWriter;

/**
 * Writes the state for calculation with kalypso-1d.exe (which is different from wspwin format!).
 *
 * @author Gernot Belger
 */
public class TuhhCalcZustandWriter
{
  private final TuhhReach m_reach;

  private final TuhhReachProfileSegment[] m_segments;

  private final TuhhStationRange m_stationRange;

  private final String m_roughnessType;

  private final boolean m_preferRoughnessClasses;

  private final boolean m_preferVegetationClasses;

  public TuhhCalcZustandWriter( final TuhhReach reach, final TuhhStationRange stationRange, final String roughnessType, final boolean preferingRoughnessClasses, final boolean preferingVegetationClasses )
  {
    m_stationRange = stationRange;
    m_reach = reach;
    m_roughnessType = roughnessType;
    m_preferRoughnessClasses = preferingRoughnessClasses;
    m_preferVegetationClasses = preferingVegetationClasses;
    m_segments = m_reach.getReachProfileSegments();

    final TuhhSegmentStationComparator stationComparator = new TuhhSegmentStationComparator( stationRange.getDirection() );
    Arrays.sort( m_segments, stationComparator );
  }

  public void write( final File zustFile ) throws IOException
  {
    final WspWinCalcZustand wspWinZustand = new WspWinCalcZustand( m_reach.getName(), zustFile.getName() );

    final File profDir = zustFile.getParentFile();
    int fileCount = 0;
    for( final TuhhReachProfileSegment segment : m_segments )
      writeProfile( wspWinZustand, segment, profDir, fileCount++ );

    zustFile.getParentFile().mkdirs();

    if( fileCount == 0 )
    {
      final String error = String.format( Messages.getString( "WspWinExporter.4" ) ); //$NON-NLS-1$
      throw new IllegalArgumentException( error );
    }

    wspWinZustand.write( zustFile );
  }

  private void writeProfile( final WspWinCalcZustand zustand, final TuhhReachProfileSegment segment, final File profDir, final int fileCount ) throws IOException
  {
    final BigDecimal station = segment.getStation();
    if( m_stationRange.isOutside( station ) )
      return;

    final String prfName = "Profil_" + fileCount + ".prf"; //$NON-NLS-1$ //$NON-NLS-2$
    final BigDecimal exportStation = station.multiply( new BigDecimal( m_stationRange.getExportSign() ) );

    zustand.addProfile( prfName, exportStation );

    final IProfileFeature profileMember = segment.getProfileMember();

    final IProfile profil = profileMember.getProfile();
    profil.setStation( exportStation.doubleValue() );

    final File outPrfFile = new File( profDir, prfName );

    final WspWinProfileWriter writer = new WspWinProfileWriter( profil, fileCount, m_roughnessType, m_preferRoughnessClasses, m_preferVegetationClasses );

    writer.write( outPrfFile );
  }
}