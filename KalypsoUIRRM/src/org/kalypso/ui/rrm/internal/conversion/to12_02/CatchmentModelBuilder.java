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
package org.kalypso.ui.rrm.internal.conversion.to12_02;

import java.io.File;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.model.hydrology.cm.binding.ICatchmentModel;
import org.kalypso.model.rcm.binding.ILinearSumGenerator;
import org.kalypso.model.rcm.binding.IRainfallGenerator;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * Helper that guesses a CatchmentModel for each existing calculation case.
 *
 * @author Gernot Belger
 */
public class CatchmentModelBuilder
{
  private final IStatusCollector m_log = new StatusCollector( KalypsoUIRRMPlugin.getID() );

  private final ICatchmentModel m_catchmentModel;

  private final File m_simulationDir;

  public CatchmentModelBuilder( final ICatchmentModel catchmentModel, final File simulationDir )
  {
    m_catchmentModel = catchmentModel;
    m_simulationDir = simulationDir;
  }

  public IStatus execute( )
  {
    // TODO get old model.gml

    // TODO: get catchment workspace

    // TODO: create new model with all catchment
    final IFeatureBindingCollection<IRainfallGenerator> generators = m_catchmentModel.getGenerators();
    final ILinearSumGenerator generator = generators.addNew( ILinearSumGenerator.FEATURE_LINEAR_SUM_GENERATOR, ILinearSumGenerator.class );
    generator.setName( m_simulationDir.getName() );

    // TODO: guess one timeseries for each catchment

    // TODO: add and save catchment model

    final String message = "Build catchment models from existing calculation cases";
    return m_log.asMultiStatusOrOK( message, message );
  }
}