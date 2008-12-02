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
package org.kalypso.model.wspm.sobek.calculation.job;

import java.io.File;
import java.net.URL;

import org.kalypso.contribs.eclipse.ui.progress.ConsoleHelper;
import org.kalypso.contribs.java.io.MyPrintStream;
import org.kalypso.model.wspm.sobek.calculation.job.i18n.Messages;
import org.kalypso.model.wspm.sobek.calculation.job.worker.SimulationBaseWorker;
import org.kalypso.model.wspm.sobek.calculation.job.worker.SimulationPi2SobekWorker;
import org.kalypso.model.wspm.sobek.calculation.job.worker.SimulationSobek2PIWorker;
import org.kalypso.model.wspm.sobek.calculation.job.worker.SimulationSobekOpenMIWorker;
import org.kalypso.model.wspm.sobek.calculation.job.worker.SimulationUpdateDataWorker;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;

/**
 * @author thuel2
 */
public class WspmSobekCalcJob implements ISimulation
{
  private static final String CALCJOB_SPEC = "resources/model_spec.xml"; //$NON-NLS-1$

  private final MyPrintStream m_nofdpStream;

  private final MyPrintStream m_sobekStream;

  public WspmSobekCalcJob( )
  {
    this( null, null );
  }

  public WspmSobekCalcJob( MyPrintStream nofdpStream, MyPrintStream sobekStream )
  {
    m_nofdpStream = nofdpStream;
    m_sobekStream = sobekStream;
  }

  /**
   * @see org.kalypso.simulation.core.ISimulation#getSpezifikation()
   */
  public URL getSpezifikation( )
  {
    return getClass().getResource( CALCJOB_SPEC );
  }

  /**
   * @see org.kalypso.simulation.core.ISimulation#run(java.io.File, org.kalypso.simulation.core.ISimulationDataProvider,
   *      org.kalypso.simulation.core.ISimulationResultEater, org.kalypso.simulation.core.ISimulationMonitor)
   */
  public void run( final File tmpdir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor ) throws SimulationException
  {
    ConsoleHelper.writeLine( m_nofdpStream, Messages.WspmSobekCalcJob_0 );

    final SimulationBaseWorker baseWorker = new SimulationBaseWorker( m_nofdpStream );
    baseWorker.run( tmpdir, inputProvider, resultEater, monitor );

    final SimulationUpdateDataWorker dataWorker = new SimulationUpdateDataWorker( m_nofdpStream );
    dataWorker.run( tmpdir, inputProvider, resultEater, monitor );

    final SimulationPi2SobekWorker pi2SobekWorker = new SimulationPi2SobekWorker( m_nofdpStream, m_sobekStream );
    pi2SobekWorker.run( tmpdir, inputProvider, resultEater, monitor );

    dataWorker.run( tmpdir, inputProvider, resultEater, monitor );

    final SimulationSobekOpenMIWorker sobekWorker = new SimulationSobekOpenMIWorker( m_nofdpStream, m_sobekStream );
    sobekWorker.run( tmpdir, inputProvider, resultEater, monitor );

    final SimulationSobek2PIWorker sobek2Pi = new SimulationSobek2PIWorker( m_nofdpStream, m_sobekStream );
    sobek2Pi.run( tmpdir, inputProvider, resultEater, monitor );

    /* add results of calculation */
    File points = new File( tmpdir, ISobekCalculationJobConstants.CALCULATION_RESULT_POINTS_PATH );
    File structures = new File( tmpdir, ISobekCalculationJobConstants.CALCULATION_RESULT_STRUCTURES_PATH );

    if( !points.exists() )
      throw new SimulationException( Messages.WspmSobekCalcJob_1 );
// if( !structures.exists() )
// throw new SimulationException( Messages.WspmSobekCalcJob_2 );

    resultEater.addResult( ISobekCalculationJobConstants.CALCULATION_RESULT_POINTS, points );
    if( structures.exists() )
      resultEater.addResult( ISobekCalculationJobConstants.CALCULATION_RESULT_STRUCTURES, structures );

    ConsoleHelper.writeLine( m_nofdpStream, Messages.WspmSobekCalcJob_3 );
    ConsoleHelper.writeLine( m_nofdpStream, "" ); //$NON-NLS-1$
  }
}
