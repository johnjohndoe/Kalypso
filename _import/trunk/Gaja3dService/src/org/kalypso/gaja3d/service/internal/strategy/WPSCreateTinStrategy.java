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

import java.net.MalformedURLException;
import java.net.URL;
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
import org.kalypso.gaja3d.simulation.CreateTinSimulation;
import org.kalypso.service.wps.client.WPSRequest;

/**
 * This strategy calls a (possibly local) WPS to start the simulation
 * 
 * @author kurzbach
 */
public class WPSCreateTinStrategy implements CreateTinStrategy {

	private static final String MODEL_TIN_FILE = "ModelTin.zip";

	private double minAngle;
	private double maxArea;

	public URL createTin(final URL boundaryLocation,
			final URL breaklinesLocation) throws RemoteException {
		IFolder calcCaseFolder;
		try {
			/* Create folder for simulation */
			final IProject project = ResourcesPlugin.getWorkspace().getRoot()
					.getProject(CreateTinSimulation.ID);
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
		inputs.put(CreateTinSimulation.INPUT_BOUNDARY, boundaryLocation);
		inputs.put(CreateTinSimulation.INPUT_BREAKLINES, breaklinesLocation);
		if (maxArea != 0)
			inputs.put(CreateTinSimulation.INPUT_MAX_AREA, Double
					.toString(maxArea));
		if (minAngle != 0)
			inputs.put(CreateTinSimulation.INPUT_MIN_ANGLE, Double
					.toString(minAngle));
		inputs.put("_" + CreateTinSimulation.OUTPUT_MODEL_TIN, MODEL_TIN_FILE);
		final List<String> outputs = new ArrayList<String>();
		outputs.add(CreateTinSimulation.OUTPUT_MODEL_TIN);
		outputs.add("stdout");
		outputs.add("stderr");

		/* Create the delegate which can handle ISimulations. */
		final String serviceEndpoint = WPSRequest.SERVICE_LOCAL;

		final WPSRequest simulationJob = new WPSRequest(CreateTinSimulation.ID,
				serviceEndpoint, 300000);
		final IStatus status = simulationJob.run(inputs, outputs,
				new NullProgressMonitor());

		if (!status.isOK())
			throw AxisFault.makeFault(new CoreException(status));

		/* Get the result. */
		final Map<String, ComplexValueReference> references = simulationJob
				.getReferences();
		final ComplexValueReference modelTinLocation = (ComplexValueReference) references
				.get(CreateTinSimulation.OUTPUT_MODEL_TIN);
		try {
			return new URL(modelTinLocation.getReference());
		} catch (final MalformedURLException e) {
			throw AxisFault.makeFault(e);
		}
	}

	@Override
	public void setMaxArea(double maxArea) {
		this.maxArea = maxArea;
	}

	@Override
	public void setMinAngle(double minAngle) {
		this.minAngle = minAngle;
	}

}
