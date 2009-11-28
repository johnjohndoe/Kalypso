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
package org.kalypso.risk.model.simulation;

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
 * @author Dejan Antanaskovic
 * 
 */
public class SimulationKalypsoRisk_PLCPreprocessing implements ISimulation
{
  private final static String INPUT_CALCULATION_NATURE = "CalculationNature";

  private final static String INPUT_RASTERMODEL = "RasterModel";

  private final static String INPUT_RASTERFOLDERSOURCEINPUT = "RasterFolderSourceInput";

  private final static String INPUT_RASTERFOLDERSOURCEOUTPUT = "RasterFolderSourceOutput";

  private final static String INPUT_STATUSQUO_RASTERMODEL = "StatusQuoRasterModel";

  private final static String OUTPUT_FOLDER = "OutputFolder";

  /**
   * @see org.kalypso.simulation.core.ISimulation#getSpezifikation()
   */
  @Override
  public URL getSpezifikation( )
  {
    return getClass().getResource( "Specification_PLCPreprocessing.xml" ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.simulation.core.ISimulation#run(java.io.File, org.kalypso.simulation.core.ISimulationDataProvider,
   *      org.kalypso.simulation.core.ISimulationResultEater, org.kalypso.simulation.core.ISimulationMonitor)
   */
  @Override
  public void run( final File tmpdir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor ) throws SimulationException
  {
    final boolean hasCalculationNature = inputProvider.hasID( INPUT_CALCULATION_NATURE );
    if( !hasCalculationNature )
      return;
    // TODO: Workaround for PLC Ticket #374, switch after fixing to the commented line
    // final String calculationNature = (String) inputProvider.getInputForID( "CalculationNature" );
    String calculationNature = (String) inputProvider.getInputForID( INPUT_CALCULATION_NATURE );
    calculationNature = calculationNature.substring( calculationNature.lastIndexOf( "/" ) + 1 );

    if( "PLC".equals( calculationNature ) )
    {
      boolean statusQuoModelExists = false;
      if( inputProvider.hasID( INPUT_STATUSQUO_RASTERMODEL ) )
      {
        final File model = FileUtils.toFile( (URL) inputProvider.getInputForID( INPUT_STATUSQUO_RASTERMODEL ) );
        statusQuoModelExists = model.exists();
      }
      try
      {
        if( !statusQuoModelExists )
        {
          final File actualRasterModel = FileUtils.toFile( (URL) inputProvider.getInputForID( INPUT_RASTERMODEL ) );
          if( !actualRasterModel.exists() )
            throw new SimulationException( "Raster model does not exist!" );
          final File actualRasterFolderInput = FileUtils.toFile( (URL) inputProvider.getInputForID( INPUT_RASTERFOLDERSOURCEINPUT ) );
          if( !actualRasterFolderInput.exists() )
            throw new SimulationException( "Calculated coverages folder 'input' does not exist!" );
          final File actualRasterFolderOutput = FileUtils.toFile( (URL) inputProvider.getInputForID( INPUT_RASTERFOLDERSOURCEOUTPUT ) );
          if( !actualRasterFolderOutput.exists() )
            throw new SimulationException( "Calculated coverages folder 'output' does not exist!" );
          final List<String> folders = new ArrayList<String>();
          folders.add( "PLC" );
          folders.add( "PLC/statusQuo" );
          folders.add( "PLC/statusQuo/raster" );
          folders.add( "PLC/statusQuo/raster/input" );
          folders.add( "PLC/statusQuo/raster/output" );
          folders.add( "PLC/difference" );
          folders.add( "PLC/difference/raster" );
          folders.add( "PLC/difference/raster/output" );
          folders.add( "PLC/final" );
          folders.add( "PLC/final/rrm" );
          folders.add( "PLC/final/risk" );
          folders.add( "PLC/final/risk/raster" );
          folders.add( "PLC/final/risk/raster/output" );
          for( final String folder : folders )
          {
            final File f = new File( tmpdir, folder );
            f.mkdirs();
            final File d = new File( f, "control.ctl" );
            d.createNewFile();
          }
          final File differenceModelFolder = new File( tmpdir, "PLC/difference" );
          final File statusQuoModelFolder = new File( tmpdir, "PLC/statusQuo" );
          final File statusQuoRasterFolderInput = new File( tmpdir, "PLC/statusQuo/raster/input" );
          final File statusQuoRasterFolderOutput = new File( tmpdir, "PLC/statusQuo/raster/output" );
          FileUtils.copyDirectory( actualRasterFolderInput, statusQuoRasterFolderInput );
          FileUtils.copyDirectory( actualRasterFolderOutput, statusQuoRasterFolderOutput );
          FileUtils.copyFileToDirectory( actualRasterModel, statusQuoModelFolder );
          FileUtils.copyFileToDirectory( actualRasterModel, differenceModelFolder );
        }
      }
      catch( final IOException e )
      {
        throw new SimulationException( e.getLocalizedMessage() );
      }
      resultEater.addResult( OUTPUT_FOLDER, tmpdir );
    }
  }

}