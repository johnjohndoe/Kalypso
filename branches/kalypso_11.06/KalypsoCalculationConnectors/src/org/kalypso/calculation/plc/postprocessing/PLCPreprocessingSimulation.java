/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.calculation.plc.postprocessing;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;

/**
 * @author Stefan Kurzbach
 */
public class PLCPreprocessingSimulation implements ISimulation
{
  public static final String ID = "KalypsoRisk_PLCPreprocessing"; //$NON-NLS-1$

  public final static String INPUT_RISK_MODEL = "RasterModel"; //$NON-NLS-1$

  public final static String INPUT_RISK_RESULT_FOLDER = "RasterFolderSourceOutput"; //$NON-NLS-1$

  public final static String INPUT_FLOOD_MODEL = "FloodModel"; //$NON-NLS-1$

  public final static String INPUT_FLOOD_RESULT_FOLDER = "FloodResultFolder"; //$NON-NLS-1$

  public final static String OUTPUT_FOLDER = "OutputFolder"; //$NON-NLS-1$

  public static final String STATUS_QUO_FOLDER_NAME = "statusQuo"; //$NON-NLS-1$

  /**
   * @see org.kalypso.simulation.core.ISimulation#getSpezifikation()
   */
  @Override
  public URL getSpezifikation( )
  {
    return getClass().getResource( "resources/Specification_PLCPreprocessing.xml" ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.simulation.core.ISimulation#run(java.io.File, org.kalypso.simulation.core.ISimulationDataProvider,
   *      org.kalypso.simulation.core.ISimulationResultEater, org.kalypso.simulation.core.ISimulationMonitor)
   */
  @Override
  public void run( final File tmpdir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor ) throws SimulationException
  {
    try
    {
      // create final folders
      final List<String> folders = new ArrayList<String>();
      folders.add( "final/rrm" ); //$NON-NLS-1$
      folders.add( "final/flood/events" ); //$NON-NLS-1$
      folders.add( "final/risk/raster/output" ); //$NON-NLS-1$
      for( final String folder : folders )
      {
        final File f = new File( tmpdir, folder );
        f.mkdirs();
      }

      final File statusQuoFolder = new File( tmpdir, STATUS_QUO_FOLDER_NAME ); //$NON-NLS-1$

      // move risk outputs to status quo
      if( inputProvider.hasID( INPUT_RISK_MODEL ) )
      {
        final File actualRasterModel = FileUtils.toFile( (URL) inputProvider.getInputForID( INPUT_RISK_MODEL ) );
        FileUtils.copyFileToDirectory( actualRasterModel, statusQuoFolder );

        final File actualRasterFolderOutput = FileUtils.toFile( (URL) inputProvider.getInputForID( INPUT_RISK_RESULT_FOLDER ) );
        final File statusQuoRasterFolderOutput = new File( statusQuoFolder, "raster/output" ); //$NON-NLS-1$
        FileUtils.moveDirectory( actualRasterFolderOutput, statusQuoRasterFolderOutput );
      }

      // move flood outputs to status quo
      if( inputProvider.hasID( INPUT_FLOOD_MODEL ) )
      {
        final File actualFloodModel = FileUtils.toFile( (URL) inputProvider.getInputForID( INPUT_FLOOD_MODEL ) );
        FileUtils.copyFileToDirectory( actualFloodModel, new File( statusQuoFolder, "models" ) ); //$NON-NLS-1$

        final File actualFloodResults = FileUtils.toFile( (URL) inputProvider.getInputForID( INPUT_FLOOD_RESULT_FOLDER ) );
        final File statusQuoFloodResults = new File( statusQuoFolder, "events" ); //$NON-NLS-1$
        FileUtils.moveDirectory( actualFloodResults, statusQuoFloodResults );
      }

    }
    catch( final IOException e )
    {
      throw new SimulationException( "Vorbereiten der Planer-Client-Simulation fehlgeschlagen.", e );
    }

    resultEater.addResult( OUTPUT_FOLDER, tmpdir );
  }

}