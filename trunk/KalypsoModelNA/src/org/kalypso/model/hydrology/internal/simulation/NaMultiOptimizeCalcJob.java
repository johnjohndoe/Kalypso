/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.model.hydrology.internal.simulation;

import java.io.File;
import java.net.URL;
import java.util.logging.Logger;

import org.kalypso.model.hydrology.INaSimulationData;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.NaSimulationDataFactory;
import org.kalypso.model.hydrology.internal.NaOptimizeLoader;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.optimize.OptimizeMonitor;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * @author Gernot Belger
 */
public class NaMultiOptimizeCalcJob implements ISimulation
{
  public static final String SPEC_XML_LOCATION = "multioptimize_spec.xml"; //$NON-NLS-1$

  private File m_optimizeResultFile;

  private File m_commonResultsDir;

  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#run(java.io.File,
   *      org.kalypso.services.calculation.job.ICalcDataProvider, org.kalypso.services.calculation.job.ICalcResultEater,
   *      org.kalypso.services.calculation.job.ICalcMonitor)
   */
  @Override
  public void run( final File tmpdir, final ISimulationDataProvider dataProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor ) throws SimulationException
  {
    final INaSimulationData data = null;

    try
    {
      monitor.setMessage( "Lade Optimierungskonfiguration..." );

      final Logger logger = Logger.getAnonymousLogger();

      final NaOptimizeLoader optimizeLoader = new NaOptimizeLoader( dataProvider );
      final NodeList optimizeNodes = optimizeLoader.loadOptimizeNodesForMulti();

      m_commonResultsDir = new File( tmpdir, "commonResultDir" );
      m_optimizeResultFile = new File( m_commonResultsDir, "fullOptimizeResult.gml" );

      // TODO: copy first optimize bean to optimizeResultFile

      // TODO: prepare common resultDir: copy from data provider result into this dir

      for( int i = 0; i < optimizeNodes.getLength(); i++ )
      {
        final Node optimizeNode = optimizeNodes.item( i );

        final String optimizeRunDirname = String.format( "optimizeRun_%d", i );
        final File optimizeRunDir = new File( tmpdir, optimizeRunDirname );

        runOptimize( optimizeRunDir, dataProvider, monitor, logger );
      }

      publishResults( resultEater );
    }
    catch( final SimulationException e )
    {
      throw e;
    }
    catch( final Exception e )
    {
      final String msg = Messages.getString( "org.kalypso.convert.namodel.NaModelCalcJob.0", e.getLocalizedMessage() ); //$NON-NLS-1$
      throw new SimulationException( msg, e );
    }
    finally
    {
      if( data != null )
        data.dispose();
    }
  }

  private void runOptimize( final File optimizeRunDir, final ISimulationDataProvider dataProvider, final ISimulationMonitor monitor, final Logger logger ) throws SimulationException
  {
    // FIXME: tweak data provider in order to fetch results from common result dir
    // FIXME: tweak data provider in order to use the right optimize bean

    final OptimizeDataProvider optimizeDataProvider = new OptimizeDataProvider( dataProvider );
    optimizeDataProvider.setInput( NaModelConstants.IN_OPTIMIZE_ID, m_optimizeResultFile );
    final Object optimizePath = "";
    optimizeDataProvider.setInput( NaModelConstants.IN_OPTIMIZE_FEATURE_PATH_ID, optimizePath );
    optimizeDataProvider.setInput( NaModelConstants.IN_RESULTS_DIR_ID, m_commonResultsDir );

    final INaSimulationData data = NaSimulationDataFactory.load( dataProvider );

    final NAOptimizingJob job = new NAOptimizingJob( optimizeRunDir, data, new OptimizeMonitor( monitor ), logger );
    job.run( monitor );

    // FIXME: copy best results into common resultsDir

    // FIXME: copy best result bean to common results dir

    // FIXME: reload best result bean and step to next

  }

  private void publishResults( final ISimulationResultEater resultEater ) throws SimulationException
  {
    resultEater.addResult( NaModelConstants.OUT_ZML, m_commonResultsDir );
    resultEater.addResult( NaModelConstants.OUT_OPTIMIZEFILE, m_optimizeResultFile );
  }

  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#getSpezifikation()
   */
  @Override
  public URL getSpezifikation( )
  {
    return getClass().getResource( SPEC_XML_LOCATION );
  }
}