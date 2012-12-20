/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.kalypsomodel1d2d.sim.preprocessing;

import java.io.File;
import java.io.IOException;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.kalypsomodel1d2d.conv.results.RestartNodes;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCollection;
import org.kalypso.kalypsosimulationmodel.core.wind.IWindModel;
import org.kalypso.simulation.core.SimulationException;

/**
 * -Data provider for {@link PreRMAKalypsoWorker}.
 * 
 * @author Gernot Belger
 */
public interface IPreRMAData
{
  IControlModel1D2D getControlModel( ) throws CoreException, SimulationException;

  IFEDiscretisationModel1d2d getDiscretisationModel( ) throws CoreException, SimulationException;

  IFlowRelationshipModel getFlowRelationshipModel( ) throws CoreException, SimulationException;

  IRoughnessClsCollection getRoughnessClassCollection( ) throws CoreException, SimulationException;

  IWindModel getWindModel( ) throws CoreException, SimulationException;

  RestartNodes prepareRestart( IControlModel1D2D controlModel, File tmpDir ) throws IOException, SimulationException, CoreException;
}
