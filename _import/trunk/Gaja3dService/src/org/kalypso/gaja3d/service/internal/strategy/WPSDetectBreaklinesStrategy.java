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
import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import net.opengeospatial.wps.ComplexValueType;

import org.apache.axis.AxisFault;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.gaja3d.service.impl.Gaja3dQNames;
import org.kalypso.gaja3d.simulation.DetectBreaklinesSimulation;
import org.kalypso.service.wps.client.WPSRequest;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * This strategy calls a local WPS to start the simulation
 * 
 * @author kurzbach
 */
public class WPSDetectBreaklinesStrategy extends AbstractWPSStrategy implements
		DetectBreaklinesStrategy {
	private static final String DEM_GRID_GML_TEMPLATE = "DemGrids.gml";

	private String edgeMethod = null;
	private String smoothMethod = null;
	private int smooth = -1;
	private String featureMethod = null;
	private double lowThresh = -1;
	private double highThresh = -1;
	private double distanceTolerance = -1;

	public URI[] detectBreaklines(final URI[] boundaryLocations,
			final URI[] demGridLocations) throws RemoteException {
		final GMLWorkspace boundariesWorkspace = buildGMLWorkspace(
				boundaryLocations, BOUNDARIES_GML_TEMPLATE,
				Gaja3dQNames.RP_BOUNDARY);
		final GMLWorkspace demGridsWorkspace = buildGMLWorkspace(
				demGridLocations, DEM_GRID_GML_TEMPLATE,
				Gaja3dQNames.RP_DEM_GRID);

		final Map<String, Object> inputs = new HashMap<String, Object>();
		inputs.put(DetectBreaklinesSimulation.INPUT_BOUNDARY,
				boundariesWorkspace);
		inputs
				.put(DetectBreaklinesSimulation.INPUT_DEM_GRID,
						demGridsWorkspace);

		if (edgeMethod != null) {
			inputs
					.put(DetectBreaklinesSimulation.INPUT_EDGE_FILTER,
							edgeMethod);
		}
		if (smoothMethod != null) {
			inputs.put(DetectBreaklinesSimulation.INPUT_SMOOTH_FILTER,
					smoothMethod);
		}
		if (smooth != -1) {
			inputs.put(DetectBreaklinesSimulation.INPUT_SMOOTH, Integer
					.toString(smooth));
		}
		if (featureMethod != null) {
			inputs.put(DetectBreaklinesSimulation.INPUT_FEATURE_DETECTOR,
					featureMethod);
		}
		if (lowThresh != -1) {
			inputs.put(DetectBreaklinesSimulation.INPUT_LOW_THRESH, Double
					.toString(lowThresh));
		}
		if (highThresh != -1) {
			inputs.put(DetectBreaklinesSimulation.INPUT_HIGH_THRESH, Double
					.toString(highThresh));
		}
		if (distanceTolerance != -1) {
			inputs.put(DetectBreaklinesSimulation.INPUT_DISTANCE_TOLERANCE,
					Double.toString(distanceTolerance));
		}

		final List<String> outputs = new ArrayList<String>();
		outputs.add(DetectBreaklinesSimulation.OUTPUT_BREAKLINES);
		outputs.add("stdout");
		outputs.add("stderr");

		/* Create the delegate which can handle ISimulations. */
		final String serviceEndpoint = WPSRequest.SERVICE_LOCAL;

		final int timeout = 60 * 60 * 1000;
		final WPSRequest simulationJob = new WPSRequest(
				DetectBreaklinesSimulation.ID, serviceEndpoint, timeout);
		final IStatus status = simulationJob.run(inputs, outputs,
				new NullProgressMonitor());

		if (!status.isOK())
			throw AxisFault.makeFault(new CoreException(status));

		try {
			/* Get the result. */
			final Map<String, ComplexValueType> results = simulationJob
					.getComplexValues();
			final ComplexValueType complexValue = results
					.get(DetectBreaklinesSimulation.OUTPUT_BREAKLINES);
			final URI[] breaklinesLocations = parseLocations(complexValue,
					Gaja3dQNames.RP_BREAKLINES);
			return breaklinesLocations;
		} catch (final Exception e) {
			throw AxisFault.makeFault(e);
		}
	}

	/**
	 * @param edgeMethod
	 *            the edgeMethod to set
	 */
	public void setEdgeMethod(String edgeMethod) {
		this.edgeMethod = edgeMethod;
	}

	/**
	 * @param smoothMethod
	 *            the smoothMethod to set
	 */
	public void setSmoothMethod(String smoothMethod) {
		this.smoothMethod = smoothMethod;
	}

	/**
	 * @param smooth
	 *            the smooth to set
	 */
	public void setSmooth(int smooth) {
		this.smooth = smooth;
	}

	/**
	 * @param featureMethod
	 *            the featureMethod to set
	 */
	public void setFeatureMethod(String featureMethod) {
		this.featureMethod = featureMethod;
	}

	/**
	 * @param lowThresh
	 *            the lowThresh to set
	 */
	public void setLowThresh(double lowThresh) {
		this.lowThresh = lowThresh;
	}

	/**
	 * @param highThresh
	 *            the highThresh to set
	 */
	public void setHighThresh(double highThresh) {
		this.highThresh = highThresh;
	}

	/**
	 * @param distanceTolerance
	 *            the distanceTolerance to set
	 */
	public void setDistanceTolerance(double distanceTolerance) {
		this.distanceTolerance = distanceTolerance;
	}

}
