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

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.math.BigDecimal;

import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhStationRange;
import org.kalypso.model.wspm.tuhh.core.profile.energyloss.ENERGYLOSS_TYPE;
import org.kalypso.model.wspm.tuhh.core.profile.energyloss.Energyloss;
import org.kalypso.model.wspm.tuhh.core.profile.energyloss.EnergylossProfileObject;

/**
 * Writes the energyloss psi-file for calculation with kalypso-1d.exe .
 *
 * @author kimwerner
 */
public class TuhhCalcEnergylossWriter
{
  private final TuhhReach m_reach;

  private final TuhhReachProfileSegment[] m_segments;

  private final TuhhStationRange m_stationRange;

  public TuhhCalcEnergylossWriter( final TuhhReach reach, final TuhhStationRange stationRange )
  {
    m_reach = reach;
    m_stationRange = stationRange;

    m_segments = m_reach.getReachProfileSegments();
  }

  public void write( final File psiFile ) throws IOException
  {
    psiFile.getParentFile().mkdirs();

    try( PrintWriter psiWriter = new PrintWriter( new BufferedWriter( new FileWriter( psiFile ) ) ); )
    {
      for( final TuhhReachProfileSegment segment : m_segments )
        writeSegment( psiWriter, segment );
    }
  }

  private void writeSegment( final PrintWriter psiWriter, final TuhhReachProfileSegment segment )
  {
    final IProfileFeature profileFeature = segment.getProfileMember();
    if( profileFeature == null )
    {
      System.out.println( this.getClass() + ": No profilemember found in segment " + segment.getId() ); //$NON-NLS-1$
      return;
    }

    final BigDecimal station = segment.getStation();
    if( m_stationRange.isOutside( station ) )
      return;

    final IProfile profil = profileFeature.getProfile();

    final EnergylossProfileObject[] energylossProfileObjects = profil.getProfileObjects( EnergylossProfileObject.class );

    for( final EnergylossProfileObject energylossProfileObject : energylossProfileObjects )
    {
      // REMARK: if we have more than one energyloss object inside one profile, we get several line for one station

      final Energyloss[] energylosses = energylossProfileObject.getEnergylosses();
      writeLosses( psiWriter, energylosses, station );
    }
  }

  private void writeLosses( final PrintWriter psiWriter, final Energyloss[] energylosses, final BigDecimal station )
  {
    if( energylosses == null || energylosses.length == 0 )
    {
      System.out.println( this.getClass() + ": Profile " + station + " does not contain any Energyloss values" ); //$NON-NLS-1$ //$NON-NLS-2$
      return;
    }

    psiWriter.print( "STATION " + station ); //$NON-NLS-1$

    int zusatzVerlustCount = 1;
    for( final Energyloss energyloss : energylosses )
    {
      final String type = energyloss.getType();

      // REMARK: using 'zusatzverlust' for all kinds except 'einlauf' because calculation core only hadles 'einlauf' in a different way.
      String lossName;
      if( ENERGYLOSS_TYPE.eEinlauf.getId().equals( type ) )
        lossName = ENERGYLOSS_TYPE.eEinlauf.getId();
      else
        lossName = ENERGYLOSS_TYPE.eZusatzverlust.getId() + zusatzVerlustCount++;

      psiWriter.format( " %s %s", lossName, energyloss.getValue() ); //$NON-NLS-1$ //$NON-NLS-2$
    }
    psiWriter.println();
  }
}