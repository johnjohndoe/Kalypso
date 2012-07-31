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
package org.kalypso.model.wspm.pdb.ui.internal.gaf;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSection;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.gaf.GafCodes;
import org.kalypso.model.wspm.pdb.gaf.ICoefficients;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.kalypso.model.wspm.pdb.wspm.CheckinStatePdbOperation;
import org.kalypsodeegree.KalypsoDeegreePlugin;

/**
 * This exporter exports profile data into the gaf format.
 * 
 * @author Holger Albert
 */
public class GafExporter
{
  public GafExporter( )
  {
  }

  public IStatus export( final IProfileFeature[] profiles, final File file, IProgressMonitor monitor )
  {
    if( monitor == null )
      monitor = new NullProgressMonitor();

    try
    {
      monitor.beginTask( "Exporting profiles into the GAF Exchange Format", 1000 );
      monitor.subTask( "Converting profiles..." );

      final Set<CrossSection> crossSections = getCrossSections( profiles, new SubProgressMonitor( monitor, 500 ) );

      monitor.subTask( "Writing profiles..." );

      return writeCrossSections( crossSections, file, new SubProgressMonitor( monitor, 500 ) );
    }
    catch( final Exception ex )
    {
      return new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, ex.getLocalizedMessage(), ex );
    }
    finally
    {
      monitor.done();
    }
  }

  private Set<CrossSection> getCrossSections( final IProfileFeature[] profiles, final IProgressMonitor monitor ) throws IOException
  {
    try
    {
      final GafCodes gafCodes = new GafCodes();
      final ICoefficients coefficients = new SimpleCoefficients();
      final WaterBody[] waterBodies = getWaterBodies( profiles );
      final State state = new State();
      final String coordinateSystem = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

      final CheckinStatePdbOperation operation = new CheckinStatePdbOperation( gafCodes, coefficients, waterBodies, state, profiles, coordinateSystem, null, monitor );
      operation.execute( null );

      return state.getCrossSections();
    }
    catch( final PdbConnectException ex )
    {
      /* HINT: This one should not occure, because we do not connect to the PDB here. */
      ex.printStackTrace();

      return null;
    }
  }

  private WaterBody[] getWaterBodies( final IProfileFeature[] profiles )
  {
    final List<WaterBody> waterBodies = new ArrayList<WaterBody>();

    for( final IProfileFeature profile : profiles )
    {
      final WspmWaterBody water = profile.getWater();
      if( water == null )
        continue;

      final WaterBody waterBody = new WaterBody();
      waterBody.setName( water.getRefNr() );

      waterBodies.add( waterBody );
    }

    return waterBodies.toArray( new WaterBody[] {} );
  }

  private IStatus writeCrossSections( final Set<CrossSection> crossSections, final File file, final IProgressMonitor monitor )
  {
    final GafWriter writer = new GafWriter();
    return writer.write( crossSections, file, monitor );
  }
}