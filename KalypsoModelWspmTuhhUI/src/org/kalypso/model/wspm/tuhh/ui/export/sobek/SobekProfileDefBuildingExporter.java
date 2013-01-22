/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.wspm.tuhh.ui.export.sobek;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;

import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.sobek.profiles.SobekProfileDef;
import org.kalypso.model.wspm.core.profil.sobek.utils.hw.BridgeResult;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.observation.result.IRecord;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * @author Gernot
 */
class SobekProfileDefBuildingExporter
{
  private final File m_targetDir;

  public SobekProfileDefBuildingExporter( final File targetDir )
  {
    m_targetDir = targetDir;
  }

  public SobekProfileDef export( final String id, final String profileName, final IProfile profil, final IRecord[] points )
  {
    final Collection<Coordinate> lowerCrds = new ArrayList<>();
    final Collection<Coordinate> upperCrds = new ArrayList<>();

    final int widthIndex = profil.indexOfProperty( IWspmConstants.POINT_PROPERTY_BREITE );
    final int heigthIndex = profil.indexOfProperty( IWspmConstants.POINT_PROPERTY_HOEHE );
    final int ukIndex = profil.indexOfProperty( IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE );

    if( widthIndex == -1 || heigthIndex == -1 || ukIndex == -1 )
      // FIXME: add error status
      return null;

    for( final IRecord point : points )
    {
      final Object widthValue = point.getValue( widthIndex );
      final Object heightValue = point.getValue( heigthIndex );
      final Object ukValue = point.getValue( ukIndex );

      if( widthValue instanceof Number )
      {
        final double width = ((Number)widthValue).doubleValue();
        if( heightValue instanceof Number )
        {
          final double heigth = ((Number)heightValue).doubleValue();
          lowerCrds.add( new Coordinate( width, heigth ) );
        }

        if( ukValue instanceof Number )
        {
          final double uk = ((Number)ukValue).doubleValue();
          upperCrds.add( new Coordinate( width, uk ) );
        }
      }
    }

    /* Only for testing purposes: write tube geometry into shape file */
    final File tempDir = new File( m_targetDir, "struct" ); //$NON-NLS-1$
    tempDir.mkdirs();
    final String shapeFileName = "struct_";//$NON-NLS-1$

    final BridgeResult bridgeHw = new BridgeResult( shapeFileName, id, id, profileName, lowerCrds, upperCrds, tempDir );
    // TODO: handle problems/errors (change formattErr to a IStatusCollector)
    return bridgeHw.createProfileDef();
  }
}