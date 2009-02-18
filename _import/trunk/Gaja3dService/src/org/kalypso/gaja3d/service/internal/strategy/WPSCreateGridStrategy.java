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
package org.kalypso.gaja3d.service.internal.strategy;

import java.net.URI;
import java.net.URISyntaxException;
import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import net.opengeospatial.wps.IOValueType.ComplexValueReference;

import org.apache.axis.AxisFault;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.kalypso.gaja3d.simulation.CreateGridSimulation;
import org.kalypso.service.wps.client.WPSRequest;

/**
 * This strategy calls a (possibly local) WPS to start the simulation
 * 
 * @author kurzbach
 */
public class WPSCreateGridStrategy implements CreateGridStrategy {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.kalypso.gaja3d.service.internal.strategy.CreateGridStrategy#createGrid
	 * (java.lang.String, java.lang.String, double, double)
	 */
	public URI createGrid(final URI boundaryLocation,
			final URI demPointsLocation, final double dx, final double dy)
			throws RemoteException {
		IFolder calcCaseFolder;
		try {
			/* Create folder for simulation */
			final IProject project = ResourcesPlugin.getWorkspace().getRoot()
					.getProject(CreateGridSimulation.ID);
			if (!project.exists())
				project.create(null);
			if (!project.isOpen())
				project.open(null);
			calcCaseFolder = project.getFolder(new Path("simulation"));
			if (!calcCaseFolder.exists())
				calcCaseFolder.create(true, true, null);
		} catch (final CoreException e) {
			throw AxisFault.makeFault(e);
		}

		/* Modify the model data to your needs. */
		final Map<String, Object> inputs = new HashMap<String, Object>();
		inputs.put(CreateGridSimulation.INPUT_BOUNDARY, boundaryLocation);
		inputs.put(CreateGridSimulation.INPUT_DEM_POINTS, demPointsLocation);
		inputs.put(CreateGridSimulation.INPUT_DX, Double.toString(dx));
		inputs.put(CreateGridSimulation.INPUT_DY, Double.toString(dy));
		inputs.put("_" + CreateGridSimulation.OUTPUT_DEM_GRID, "DemGrid.asc");
		final List<String> outputs = new ArrayList<String>();
		outputs.add(CreateGridSimulation.OUTPUT_DEM_GRID);
		outputs.add("stdout");
		outputs.add("stderr");

		/* Create the delegate which can handle ISimulations. */
		final String serviceEndpoint = WPSRequest.SERVICE_LOCAL;

		final int timeout = 60 * 60 * 1000;
		final WPSRequest simulationJob = new WPSRequest(
				CreateGridSimulation.ID, serviceEndpoint, timeout);
		final IStatus status = simulationJob.run(inputs, outputs,
				new NullProgressMonitor());

		if (!status.isOK())
			throw AxisFault.makeFault(new CoreException(status));

		/* Get the result. */
		final Map<String, ComplexValueReference> references = simulationJob
				.getReferences();
		final ComplexValueReference demGridLocation = (ComplexValueReference) references
				.get(CreateGridSimulation.OUTPUT_DEM_GRID);
		try {
			return new URI(demGridLocation.getReference());
		} catch (final URISyntaxException e) {
			throw AxisFault.makeFault(e);
		}
	}

}
