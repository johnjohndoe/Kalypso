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

import org.apache.commons.io.FilenameUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.WspmProject;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.core.gml.classifications.IRoughnessClass;
import org.kalypso.model.wspm.core.gml.classifications.IVegetationClass;
import org.kalypso.model.wspm.core.gml.classifications.IWspmClassification;
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
    /* Monitor. */
    if( monitor == null )
      monitor = new NullProgressMonitor();

    /* The status collector. */
    final IStatusCollector collector = new StatusCollector( WspmPdbUiPlugin.PLUGIN_ID );

    try
    {
      /* Monitor. */
      monitor.beginTask( "Exporting profiles into the GAF Exchange Format", 2000 );
      monitor.subTask( "Converting profiles..." );

      /* Get the cross sections. */
      final Set<CrossSection> crossSections = getCrossSections( profiles, new SubProgressMonitor( monitor, 500 ) );

      /* Monitor. */
      monitor.subTask( "Writing profiles..." );

      /* Create the gaf writer. */
      final GafWriter writer = new GafWriter();

      /* Write the cross sections. */
      final IStatus gafStatus = writer.write( crossSections, file, new SubProgressMonitor( monitor, 500 ) );
      collector.add( gafStatus );

      /* Get the wspm classification. */
      final IWspmClassification wspmClassification = getWspmClassification( profiles );

      /* Export the roughness classes. */
      final IRoughnessClass[] roughnessClasses = wspmClassification != null ? wspmClassification.getRoughnessClasses() : new IRoughnessClass[] {};
      if( roughnessClasses.length > 0 )
      {
        /* Monitor. */
        monitor.subTask( "Writing roughness classes..." );

        /* Create the kst file. */
        final File parentFile = file.getParentFile();
        final String baseName = FilenameUtils.getBaseName( file.getName() );
        final File kstFile = new File( parentFile, String.format( "%s.kst", baseName ) );

        /* Create the kst writer. */
        final KstWriter kstWriter = new KstWriter();

        /* Write the roughness classes. */
        final IStatus kstStatus = kstWriter.write( roughnessClasses, kstFile, new SubProgressMonitor( monitor, 500 ) );
        collector.add( kstStatus );
      }
      else
      {
        /* Monitor. */
        monitor.worked( 500 );
      }

      /* Export the vegetation classes. */
      final IVegetationClass[] vegetationClasses = wspmClassification != null ? wspmClassification.getVegetationClasses() : new IVegetationClass[] {};
      if( vegetationClasses.length > 0 )
      {
        /* Monitor. */
        monitor.subTask( "Writing vegetation classes..." );

        /* Create the bwp file. */
        final File parentFile = file.getParentFile();
        final String baseName = FilenameUtils.getBaseName( file.getName() );
        final File bwpFile = new File( parentFile, String.format( "%s.bwp", baseName ) );

        /* Create the bwp writer. */
        final BwpWriter bwpWriter = new BwpWriter();

        /* Write the vegetation classes. */
        final IStatus bwpStatus = bwpWriter.write( vegetationClasses, bwpFile, new SubProgressMonitor( monitor, 500 ) );
        collector.add( bwpStatus );
      }
      else
      {
        /* Monitor. */
        monitor.worked( 500 );
      }

      return collector.asMultiStatusOrOK( "The export has failed.", "The export was successfull." );
    }
    catch( final Exception ex )
    {
      return new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, ex.getLocalizedMessage(), ex );
    }
    finally
    {
      /* Monitor. */
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

  private IWspmClassification getWspmClassification( final IProfileFeature[] profiles )
  {
    if( profiles == null || profiles.length == 0 )
      return null;

    final IProfileFeature profile = profiles[0];
    final WspmWaterBody water = profile.getWater();
    final WspmProject wspmProject = water.getProject();

    return wspmProject.getClassificationMember();
  }
}