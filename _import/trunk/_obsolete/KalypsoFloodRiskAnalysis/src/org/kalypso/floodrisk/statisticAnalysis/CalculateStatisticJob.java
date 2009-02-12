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
package org.kalypso.floodrisk.statisticAnalysis;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Hashtable;

import org.kalypso.floodrisk.data.ContextModel;
import org.kalypso.floodrisk.data.RasterDataModel;
import org.kalypso.floodrisk.internationalize.Messages;
import org.kalypso.floodrisk.process.IProcessResultEater;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationDataPath;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridCoverage;

/**
 * CalculateStatisticJob
 * <p>
 * Job for calculating statistics of damageGrids created by
 * 
 * @author Nadja Peiler (15.06.2005)
 */
public class CalculateStatisticJob implements ISimulation
{

  // IDs
  // input
  public static final String DamageRasterID = "DamageRaster"; //$NON-NLS-1$

  public static final String LanduseRasterDataID = "LanduseRasterData"; //$NON-NLS-1$

  // optional
  public static final String AdministrationUnitRasterDataID = "AdministrationUnitRasterData"; //$NON-NLS-1$

  public static final String ContextModelID = "ContextModel"; //$NON-NLS-1$

  // optional
  public static final String TemplateRasterID = "TemplateRaster"; //$NON-NLS-1$

  // output
  public static final String StatisticDataID = "StatisticData"; //$NON-NLS-1$

  RasterDataModel rasterDataModel = new RasterDataModel();

  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#run(java.io.File,
   *      org.kalypso.services.calculation.job.ICalcDataProvider, org.kalypso.services.calculation.job.ICalcResultEater,
   *      org.kalypso.services.calculation.job.ICalcMonitor)
   */
  public void run( File tmpdir, ISimulationDataProvider inputProvider, ISimulationResultEater resultEater, ISimulationMonitor monitor ) throws SimulationException
  {
    try
    {
      // Generate input
      // damageRaster
      monitor.setMessage( Messages.getString("statisticAnalysis.CalculateStatisticJob.LoadingInputData") ); //$NON-NLS-1$
      URL damageRasterGML = (URL) inputProvider.getInputForID( DamageRasterID );
      RectifiedGridCoverage damageRaster = rasterDataModel.getRectifiedGridCoverage( damageRasterGML );

      // landuseRaster
      URL landuseRasterGML = (URL) inputProvider.getInputForID( LanduseRasterDataID );
      RectifiedGridCoverage landuseRaster = rasterDataModel.getRectifiedGridCoverage( landuseRasterGML );

      // administrationUnitRaster
      RectifiedGridCoverage administrationUnitRaster = null;
      if( inputProvider.getInputForID( AdministrationUnitRasterDataID ) != null )
      {
        URL administrationUnitRasterGML = (URL) inputProvider.getInputForID( AdministrationUnitRasterDataID );
        administrationUnitRaster = rasterDataModel.getRectifiedGridCoverage( administrationUnitRasterGML );
      }

      // contextModel
      URL contextModelGML = (URL) inputProvider.getInputForID( ContextModelID );
      ContextModel contextModel = new ContextModel( contextModelGML );

      monitor.setProgress( 40 );

      // Calculation
      // statisticAnalysis
      monitor.setMessage( Messages.getString("statisticAnalysis.CalculateStatisticJob.Calculating") ); //$NON-NLS-1$
      Hashtable statistics = null;
      if( inputProvider.getInputForID( TemplateRasterID ) != null )
      {
        // templateRaster
        URL templateRasterGML = (URL) inputProvider.getInputForID( TemplateRasterID );
        RectifiedGridCoverage templateRaster = rasterDataModel.getRectifiedGridCoverage( templateRasterGML );
        if( administrationUnitRaster != null )
          statistics = StatisticAnalysis.getStatisticsWithTemplate( damageRaster, landuseRaster, administrationUnitRaster, templateRaster );
        else
          statistics = StatisticAnalysis.getStatisticsWithTemplate( damageRaster, landuseRaster, templateRaster );
      }
      else
      {
        if( administrationUnitRaster != null )
          statistics = StatisticAnalysis.getStatistics( damageRaster, landuseRaster, administrationUnitRaster );
        else
          statistics = StatisticAnalysis.getStatistics( damageRaster, landuseRaster );
      }

      monitor.setProgress( 20 );

      // Generate output
      // statisticData
      monitor.setMessage( Messages.getString("statisticAnalysis.CalculateStatisticJob.SavingResults") ); //$NON-NLS-1$
      SimulationDataPath statisticDataOutputBean = (SimulationDataPath) ((IProcessResultEater) resultEater).getOutputMap().get( StatisticDataID );
      File statisticDataFile = new File( statisticDataOutputBean.getPath() );
      if( !statisticDataFile.exists() )
      {
        statisticDataFile.createNewFile();
      }
      if( administrationUnitRaster != null )
        StatisticAnalysis.exportStatisticAsXML( statistics, contextModel.getLanduseList(), contextModel.getAdministrationUnitList(), statisticDataFile.toURL() );
      else
        StatisticAnalysis.exportStatisticAsXML( statistics, contextModel.getLanduseList(), statisticDataFile.toURL() );
      resultEater.addResult( statisticDataOutputBean.getId(), null );

      monitor.setProgress( 40 );
    }
    catch( MalformedURLException e )
    {
      throw new SimulationException( "CalculateDamageJob Service Exception: Malformed URL", e ); //$NON-NLS-1$
    }
    catch( Exception e )
    {
      throw new SimulationException( "CalculateDamageJob Service Exception", e ); //$NON-NLS-1$
    }
  }

  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#getSpezifikation()
   */
  public URL getSpezifikation( )
  {
    return getClass().getResource( "resources/statisticCalcjob_spec.xml" ); //$NON-NLS-1$
  }

}