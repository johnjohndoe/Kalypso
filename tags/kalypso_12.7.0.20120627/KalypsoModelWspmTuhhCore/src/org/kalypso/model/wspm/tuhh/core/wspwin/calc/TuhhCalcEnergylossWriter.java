/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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

import org.apache.commons.io.IOUtils;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhStationRange;
import org.kalypso.model.wspm.tuhh.core.profile.energyloss.ENERGYLOSS_TYPE;
import org.kalypso.model.wspm.tuhh.core.profile.energyloss.EnergylossProfileObject;
import org.kalypso.model.wspm.tuhh.core.profile.energyloss.IEnergylossProfileObject;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;

/**
 * Writes the energyloss psi-file for calculation with kalypso-1d.exe .
 * 
 * @author kimwerner
 */
public class TuhhCalcEnergylossWriter
{
  private final TuhhReach m_reach;

  private final TuhhReachProfileSegment[] m_segments;

  public TuhhCalcEnergylossWriter( final TuhhReach reach, final TuhhStationRange stationRange )
  {
    m_reach = reach;
    m_segments = m_reach.getReachProfileSegments();
  }

  public void write( final File psiFile ) throws IOException
  {
    PrintWriter psiWriter = null;
    try
    {
      psiFile.getParentFile().mkdirs();
      psiWriter = new PrintWriter( new BufferedWriter( new FileWriter( psiFile ) ) );
      for( final TuhhReachProfileSegment segment : m_segments )
      {
        final IProfileFeature profileFeature = segment.getProfileMember();
        if( profileFeature == null ){
          System.out.println(this.getClass()+": No profilemember found in segment "+segment.getId()); //$NON-NLS-1$
          continue;
        }
        final IProfil profil = profileFeature.getProfil();
        final EnergylossProfileObject[] energylosses = profil.getProfileObjects( EnergylossProfileObject.class );
        if( energylosses == null )
          continue;
        for( final EnergylossProfileObject energyloss : energylosses )
        {
          final TupleResult result = energyloss.getObservation().getResult();
          if( result == null )
          {
            System.out.println(this.getClass()+": Profile "+profil.getName()+" does not contain any Energyloss values"); //$NON-NLS-1$ //$NON-NLS-2$
            continue;
          }
          final int iType = result.indexOfComponent( IEnergylossProfileObject.PROPERTY_TYPE );
          final int iValue = result.indexOfComponent( IEnergylossProfileObject.PROPERTY_VALUE );
          psiWriter.print( "STATION " + segment.getStation() ); //$NON-NLS-1$
          int i = 1;
          for( final IRecord record : result )
          {
            final String type = record.getValue( iType ).toString();
            if( type.compareTo( ENERGYLOSS_TYPE.eEinlauf.getId() ) == 0 )
            {
              psiWriter.print( " " + ENERGYLOSS_TYPE.eEinlauf.getId() + " " + record.getValue( iValue ) ); //$NON-NLS-1$ //$NON-NLS-2$
            }
            else
            {
              psiWriter.print( " " + ENERGYLOSS_TYPE.eZusatzverlust.getId() + i++ + " " + record.getValue( iValue ) ); //$NON-NLS-1$ //$NON-NLS-2$
            }
          }
          psiWriter.println();
        }
      }
      psiWriter.flush();
      psiWriter.close();
    }
    finally
    {
      IOUtils.closeQuietly( psiWriter );
    }
  }
}