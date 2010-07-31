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
package org.kalypso.model.hydrology.internal.processing;

import java.io.File;
import java.net.URL;
import java.util.logging.Logger;

import org.kalypso.convert.namodel.i18n.Messages;
import org.kalypso.convert.namodel.job.NaModelParameterAnalyseSimulation;
import org.kalypso.convert.namodel.optimize.NAOptimizingJob;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.optimize.IOptimizingJob;
import org.kalypso.optimize.OptimizeMonitor;
import org.kalypso.optimize.OptimizerCalJob;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author doemming
 */
public class NaModelCalcJob implements ISimulation
{
  public static final String NACALCJOB_SPEC_XML_LOCATION = "/org/kalypso/model/hydrology/internal/processing/nacalcjob_spec.xml"; //$NON-NLS-1$

  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#run(java.io.File,
   *      org.kalypso.services.calculation.job.ICalcDataProvider, org.kalypso.services.calculation.job.ICalcResultEater,
   *      org.kalypso.services.calculation.job.ICalcMonitor)
   */
  @Override
  public void run( final File tmpdir, final ISimulationDataProvider dataProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor ) throws SimulationException
  {
    try
    {
      final Logger logger = Logger.getAnonymousLogger();

      final ISimulation calcJob = createCalcJob( dataProvider, logger, tmpdir, monitor );
      if( calcJob != null )
        calcJob.run( tmpdir, dataProvider, resultEater, monitor );
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
  }

  private ISimulation createCalcJob( final ISimulationDataProvider dataProvider, final Logger logger, final File tmpdir, final ISimulationMonitor monitor ) throws Exception
  {
    // FIXME: check: is the analyse job actually still used?
    // why not declare a seaprate top-level job?
    if( dataProvider.hasID( NaModelConstants.IN_ANALYSE_MODELL_XSD_ID ) )
      return new NaModelParameterAnalyseSimulation( logger );

    // testen ob calcjob optimization hat
    final URL controlLocation = (URL) dataProvider.getInputForID( NaModelConstants.IN_CONTROL_ID );
    final GMLWorkspace controlWorkspace = GmlSerializer.createGMLWorkspace( controlLocation, null );
    final Feature rootFeature = controlWorkspace.getRootFeature();
    final boolean optimize = FeatureHelper.booleanIsTrue( rootFeature, NaModelConstants.NACONTROL_AUTOCALI_PROP, false );
    if( optimize )
    {
      final IOptimizingJob optimizeJob = new NAOptimizingJob( tmpdir, dataProvider, new OptimizeMonitor( monitor ) );
      return new OptimizerCalJob( logger, optimizeJob );
    }

    return new NaModelInnerCalcJob();
  }

  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#getSpezifikation()
   */
  @Override
  public URL getSpezifikation( )
  {
    return getClass().getResource( NACALCJOB_SPEC_XML_LOCATION );
  }
}