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
package org.kalypso.ui.rrm.internal.tests;

import static org.junit.Assert.fail;

import javax.xml.namespace.QName;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.preference.IPreferenceStore;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.core.preferences.IKalypsoCorePreferences;
import org.kalypso.model.hydrology.binding.cm.IMultiGenerator;
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.project.RrmProject;
import org.kalypso.model.hydrology.project.RrmScenario;
import org.kalypso.model.hydrology.project.RrmSimulation;
import org.kalypso.model.rcm.binding.IRainfallGenerator;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ui.rrm.internal.calccase.CatchmentModelHelper;
import org.kalypso.ui.rrm.internal.calccase.MultiCatchmentModelInfo;
import org.kalypso.ui.rrm.internal.calccase.MultiCatchmentModelRunner;

/**
 * Test for verifying a multi catchment model.
 *
 * @author Holger Albert
 */
public class MultiCatchmentModelTest
{
  /**
   * The temporary project.
   */
  private IProject m_project;

  /**
   * This function sets up the test case.
   */
  @Before
  public void setUp( ) throws Exception
  {
    /* Update timezone. */
    final IPreferenceStore preferenceStore = KalypsoCorePlugin.getDefault().getPreferenceStore();
    preferenceStore.setValue( IKalypsoCorePreferences.DISPLAY_TIMEZONE, "GMT+1" ); //$NON-NLS-1$

    /* Create the temporary project. */
    m_project = TestUtilities.createProject( "MultiSample" ); //$NON-NLS-1$

    /* Unzip the resources. */
    TestUtilities.unzipResources( "/etc/tests/resources/catchmentmodels/multisample.zip", m_project ); //$NON-NLS-1$
  }

  /**
   * This function tears down the test case.
   */
  @After
  public void tearDown( ) throws Exception
  {
    m_project = null;
  }

  /**
   * This function executes the test.
   */
  @Test
  public void test( ) throws Exception
  {
    /* Get the simulation folder. */
    final RrmProject rrmProject = new RrmProject( m_project );

    final RrmScenario rrmScenario = rrmProject.getBaseScenario();
    final IFolder simulationsFolder = rrmScenario.getSimulationsFolder();
    final IFolder actualSimulationFolder = simulationsFolder.getFolder( "Actual" ); //$NON-NLS-1$
    final IFolder expectedSimulationFolder = simulationsFolder.getFolder( "Expected" ); //$NON-NLS-1$

    /* Create the simulation. */
    final RrmSimulation simulation = new RrmSimulation( actualSimulationFolder );

    /* Load the calculation.gml. */
    final NAControl control = CatchmentModelHelper.loadControl( simulation );

    /* Load the na model. */
    final NaModell model = CatchmentModelHelper.loadModel( simulation );

    /* Execute the catchment models. */
    executeCatchmentModels( simulation, control, model, control.getGeneratorN(), Catchment.PROP_PRECIPITATION_LINK, ITimeseriesConstants.TYPE_RAINFALL );
    executeCatchmentModels( simulation, control, model, control.getGeneratorE(), Catchment.PROP_EVAPORATION_LINK, ITimeseriesConstants.TYPE_EVAPORATION_LAND_BASED );
    executeCatchmentModels( simulation, control, model, control.getGeneratorT(), Catchment.PROP_TEMPERATURE_LINK, ITimeseriesConstants.TYPE_MEAN_TEMPERATURE );

    /* Compare the actual results with the expected results. */
    final IStatus status = CatchmentModelHelper.compareTimeseries( actualSimulationFolder, expectedSimulationFolder );

    /* Save the status. */
    TestUtilities.saveLogQuietly( status, m_project.getLocation().toFile() );

    /* Fail, if it is not ok. */
    if( !status.isOK() )
      fail( "The actual timeseries do not match the expected timeseries. See the log for details." ); //$NON-NLS-1$
  }

  /**
   * This function executes the catchment model.
   *
   * @param simulation
   *          The simulation.
   * @param control
   *          The na control.
   * @param model
   *          The na model.
   * @param generator
   *          The generator.
   * @param targetLink
   *          The target link.
   * @param parameterType
   *          The parameter type.
   */
  private void executeCatchmentModels( final RrmSimulation simulation, final NAControl control, final NaModell model, final IRainfallGenerator generator, final QName targetLink, final String parameterType ) throws CoreException
  {
    /* Create the catchment model info. */
    final MultiCatchmentModelInfo info = new MultiCatchmentModelInfo( simulation, control, model, (IMultiGenerator) generator, targetLink, parameterType );

    /* Create the catchment model runner. */
    final MultiCatchmentModelRunner runner = new MultiCatchmentModelRunner();

    /* Execute the catchment model. */
    runner.executeCatchmentModel( info, new NullProgressMonitor() );
  }
}