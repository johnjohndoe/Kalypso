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
import org.kalypso.model.hydrology.binding.NAOptimize;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.model.hydrology.util.optimize.NAOptimizingJob;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;

/**
 * @author doemming
 */
public class NaModelCalcJob implements ISimulation
{
  public static final String NACALCJOB_SPEC_XML_LOCATION = "nacalcjob_spec.xml"; //$NON-NLS-1$

  @Override
  public void run( final File tmpdir, final ISimulationDataProvider dataProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor ) throws SimulationException
  {
    INaSimulationData data = null;
    INaSimulationRunnable runnable = null;

    try
    {
      monitor.setMessage( Messages.getString( "NaModelCalcJob.0" ) ); //$NON-NLS-1$
      data = NaSimulationDataFactory.load( dataProvider );

      runnable = createRunnable( data, tmpdir );
      /* final boolean success = */runnable.run( monitor );
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
      // FIXME: what to do of not succeeded?
      publishResults( resultEater, runnable );
      if( data != null )
        data.dispose();
    }
  }

  private void publishResults( final ISimulationResultEater resultEater, final INaSimulationRunnable runnable ) throws SimulationException
  {
    if( runnable == null )
      return;

    final File resultDir = runnable.getResultDir();
    final File optimizeResult = runnable.getOptimizeResult();

    if( resultDir.isDirectory() )
      resultEater.addResult( NaModelConstants.OUT_ZML, resultDir );
    else
      throw new SimulationException( Messages.getString( "NaModelCalcJob.1" ) ); //$NON-NLS-1$

    if( optimizeResult != null )
    {
      if( optimizeResult.isFile() )
        resultEater.addResult( NaModelConstants.OUT_OPTIMIZEFILE, optimizeResult );
      else
        throw new SimulationException( Messages.getString( "NaModelCalcJob.1" ) ); //$NON-NLS-1$
    }
  }

  private INaSimulationRunnable createRunnable( final INaSimulationData data, final File tmpdir ) throws Exception
  {
    final boolean doOptimize = isOptimize( data );
    if( doOptimize )
    {
      // FIXME: replace with other logging framework, in preference eclipse's
      final Logger logger = Logger.getAnonymousLogger();
      return new NAOptimizingJob( tmpdir, data, logger );
    }

    return new NaModelInnerCalcJob( data, tmpdir );
  }

  private boolean isOptimize( final INaSimulationData data )
  {
    final NAOptimize optimize = data.getNaOptimize();
    if( optimize == null )
      return false;

    return optimize.doOptimize();
  }

  @Override
  public URL getSpezifikation( )
  {
    return getClass().getResource( NACALCJOB_SPEC_XML_LOCATION );
  }
}