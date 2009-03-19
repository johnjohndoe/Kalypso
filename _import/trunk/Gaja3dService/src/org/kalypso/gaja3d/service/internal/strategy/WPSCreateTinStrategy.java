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
import org.kalypso.gaja3d.service.impl.Gaja3dQNames;
import org.kalypso.gaja3d.simulation.CreateTinSimulation;
import org.kalypso.service.wps.client.WPSRequest;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * This strategy calls a local WPS to start the simulation
 * 
 * @author kurzbach
 */
public class WPSCreateTinStrategy extends AbstractWPSStrategy implements
		CreateTinStrategy {

	private static final String BREAKLINES_GML_TEMPLATE = "Breaklines.gml";
	private static final String DEM_GRID_GML_TEMPLATE = "DemGrids.gml";

	private double m_minAngle;
	private double m_maxArea;
	private URI[] m_breaklinesLocations;
	private URI[] m_demGridLocations;

	public URI createTin(final URI[] boundaryLocations) throws RemoteException {
		final GMLWorkspace boundariesWorkspace = buildGMLWorkspace(
				boundaryLocations, BOUNDARIES_GML_TEMPLATE,
				Gaja3dQNames.RP_BOUNDARY);

		final Map<String, Object> inputs = new HashMap<String, Object>();

		inputs.put(CreateTinSimulation.INPUT_BOUNDARY, boundariesWorkspace);

		if (m_breaklinesLocations != null) {
			final GMLWorkspace breaklinesWorkspace = buildGMLWorkspace(
					m_breaklinesLocations, BREAKLINES_GML_TEMPLATE,
					Gaja3dQNames.RP_BREAKLINES);
			inputs.put(CreateTinSimulation.INPUT_BREAKLINES,
					breaklinesWorkspace);
		}

		if (m_demGridLocations != null) {
			final GMLWorkspace demGridsWorkspace = buildGMLWorkspace(
					m_demGridLocations, DEM_GRID_GML_TEMPLATE,
					Gaja3dQNames.RP_DEM_GRID);
			inputs.put(CreateTinSimulation.INPUT_DEM_GRID, demGridsWorkspace);
		}

		if (m_maxArea != 0) {
			inputs.put(CreateTinSimulation.INPUT_MAX_AREA, Double
					.toString(m_maxArea));
		}

		if (m_minAngle != 0) {
			inputs.put(CreateTinSimulation.INPUT_MIN_ANGLE, Double
					.toString(m_minAngle));
		}

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

	public void setMaxArea(final double maxArea) {
		this.m_maxArea = maxArea;
	}

	public void setMinAngle(final double minAngle) {
		this.m_minAngle = minAngle;
	}

	public void setDemGridLocation(final URI[] gridLocations) {
		m_demGridLocations = gridLocations;
	}

	public void setBreaklinesLocation(final URI[] breaklinesLocations) {
		this.m_breaklinesLocations = breaklinesLocations;
	}

}
