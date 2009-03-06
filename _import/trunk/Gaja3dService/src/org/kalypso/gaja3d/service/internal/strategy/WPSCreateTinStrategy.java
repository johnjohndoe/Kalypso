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
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.gaja3d.simulation.CreateTinSimulation;
import org.kalypso.service.wps.client.WPSRequest;

/**
 * This strategy calls a local WPS to start the simulation
 * 
 * @author kurzbach
 */
public class WPSCreateTinStrategy implements CreateTinStrategy {

	private static final String MODEL_TIN_FILE = "ModelTin.zip";

	private double m_minAngle;
	private double m_maxArea;
	private URI m_breaklinesLocation;
	private URI m_demGridLocation;

	public URI createTin(final URI boundaryLocation) throws RemoteException {
		/* Modify the model data to your needs. */
		final Map<String, Object> inputs = new HashMap<String, Object>();
		inputs.put(CreateTinSimulation.INPUT_BOUNDARY, boundaryLocation);
		if (m_breaklinesLocation != null) {
			inputs.put(CreateTinSimulation.INPUT_BREAKLINES,
					m_breaklinesLocation);
		}
		if (m_demGridLocation != null) {
			inputs.put(CreateTinSimulation.INPUT_DEM_GRID, m_demGridLocation);
		}
		if (m_maxArea != 0) {
			inputs.put(CreateTinSimulation.INPUT_MAX_AREA, Double
					.toString(m_maxArea));
		}
		if (m_minAngle != 0) {
			inputs.put(CreateTinSimulation.INPUT_MIN_ANGLE, Double
					.toString(m_minAngle));
		}
		inputs.put("_" + CreateTinSimulation.OUTPUT_MODEL_TIN, MODEL_TIN_FILE);

		final List<String> outputs = new ArrayList<String>();
		outputs.add(CreateTinSimulation.OUTPUT_MODEL_TIN);
		outputs.add("stdout");
		outputs.add("stderr");

		/* Create the delegate which can handle ISimulations. */
		final String serviceEndpoint = WPSRequest.SERVICE_LOCAL;

		final int timeout = 60 * 60 * 1000;
		final WPSRequest simulationJob = new WPSRequest(CreateTinSimulation.ID,
				serviceEndpoint, timeout);
		final IStatus status = simulationJob.run(inputs, outputs,
				new NullProgressMonitor());

		if (!status.isOK()) {
			throw AxisFault.makeFault(new CoreException(status));
		}

		/* Get the result. */
		final Map<String, ComplexValueReference> references = simulationJob
				.getReferences();
		final ComplexValueReference modelTinLocation = references
				.get(CreateTinSimulation.OUTPUT_MODEL_TIN);
		try {
			return new URI(modelTinLocation.getReference());
		} catch (final URISyntaxException e) {
			throw AxisFault.makeFault(e);
		}
	}

	@Override
	public void setMaxArea(final double maxArea) {
		this.m_maxArea = maxArea;
	}

	@Override
	public void setMinAngle(final double minAngle) {
		this.m_minAngle = minAngle;
	}

	@Override
	public void setDemGridLocation(final URI gridLocation) {
		m_demGridLocation = gridLocation;
	}

	@Override
	public void setBreaklinesLocation(final URI breaklinesLocation) {
		this.m_breaklinesLocation = breaklinesLocation;
	}

}
