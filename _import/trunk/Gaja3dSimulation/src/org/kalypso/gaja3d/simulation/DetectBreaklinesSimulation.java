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
package org.kalypso.gaja3d.simulation;

import java.io.File;
import java.io.InputStream;
import java.net.URI;
import java.net.URL;
import java.util.List;

import org.apache.commons.io.IOUtils;
import org.apache.commons.vfs.FileName;
import org.apache.commons.vfs.FileObject;
import org.apache.commons.vfs.FileSystemException;
import org.apache.commons.vfs.impl.StandardFileSystemManager;
import org.kalypso.gaja3d.simulation.grid.Gaja3dGridJobSubmitter;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author kurzbach
 */
public class DetectBreaklinesSimulation extends AbstractGaja3dSimulation
		implements ISimulation {
	/**
	 * The model specification.
	 */
	private static final String SIMULATION_SPEC = "detectBreaklines_specification.xml";

	public static final String INPUT_BOUNDARY = "Boundary";

	public static final String INPUT_DEM_GRID = "DemGrid";

	public static final String INPUT_EDGE_FILTER = "EdgeFilter";

	public static final String INPUT_SMOOTH_FILTER = "SmoothFilter";

	public static final String INPUT_SMOOTH = "smooth";

	public static final String INPUT_FEATURE_DETECTOR = "FeatureDetector";

	public static final String INPUT_HIGH_THRESH = "highThresh";

	public static final String INPUT_LOW_THRESH = "lowThresh";

	public static final String INPUT_DISTANCE_TOLERANCE = "distanceTolerance";

	public static final String OUTPUT_BREAKLINES = "Breaklines";

	public static final String ID = "Gaja3d_detectBreaklines";

	private static final String BREAKLINES_GML_TEMPLATE = "Breaklines.gml";

	/**
	 * The constructor.
	 */
	public DetectBreaklinesSimulation() {
	}

	/**
	 * @see org.kalypso.simulation.core.ISimulation#getSpezifikation()
	 */
	public URL getSpezifikation() {
		return getClass().getResource(SIMULATION_SPEC);
	}

	/**
	 * @see org.kalypso.simulation.core.ISimulation#run(java.io.File,
	 *      org.kalypso.simulation.core.ISimulationDataProvider,
	 *      org.kalypso.simulation.core.ISimulationResultEater,
	 *      org.kalypso.simulation.core.ISimulationMonitor)
	 */
	public void run(final File tmpdir,
			final ISimulationDataProvider inputProvider,
			final ISimulationResultEater resultEater,
			final ISimulationMonitor monitor) throws SimulationException {
		m_arguments.add("detectBreaklines");
		m_arguments.add("true");

		FileObject workingDir = null;
		try {
			workingDir = getWorkingDir(tmpdir);

			final Object inputForBoundary = inputProvider
					.getInputForID(INPUT_BOUNDARY);
			final List<String> boundaryList = getGmlList(inputForBoundary,
					Gaja3dUrlCatalog.PROPERTY_BOUNDARY);
			final String boundarySpec = "Boundaries.zip";
			mergeZipsInWorking(boundaryList, boundarySpec, workingDir);
			m_arguments.add(INPUT_BOUNDARY);
			m_arguments.add(boundarySpec);

			final Object inputForDemGrid = inputProvider
					.getInputForID(INPUT_DEM_GRID);
			final List<String> demGridList = getGmlList(inputForDemGrid,
					Gaja3dUrlCatalog.PROPERTY_DEM_GRID);
			final String demGridSpec = "DemGrid_%04d.asc";
			copyToRemote(demGridList, demGridSpec, workingDir);
			m_arguments.add(INPUT_DEM_GRID);
			m_arguments.add("DemGrid_*.asc");

			addReferencedInput(inputProvider, INPUT_EDGE_FILTER, false);
			addReferencedInput(inputProvider, INPUT_SMOOTH_FILTER, false);
			addReferencedInput(inputProvider, INPUT_SMOOTH, false);
			addReferencedInput(inputProvider, INPUT_FEATURE_DETECTOR, false);
			addReferencedInput(inputProvider, INPUT_HIGH_THRESH, false);
			addReferencedInput(inputProvider, INPUT_LOW_THRESH, false);
			addReferencedInput(inputProvider, INPUT_DISTANCE_TOLERANCE, false);

			final Gaja3dGridJobSubmitter jobSubmitter = new Gaja3dGridJobSubmitter();
			jobSubmitter.submitJob(workingDir, monitor, m_arguments);

			final int boundaryCount = boundaryList.size();
			final GMLWorkspace breaklinesWorkspace = buildBreaklinesResult(
					workingDir, boundaryCount);
			resultEater.addResult(OUTPUT_BREAKLINES, breaklinesWorkspace);
		} catch (final SimulationException e) {
			throw e;
		} catch (final Exception e) {
			throw new SimulationException(
					"Problem during breakline detection.", e);
		} finally {
			if (workingDir != null) {
				try {
					workingDir.close();
					final StandardFileSystemManager manager = (StandardFileSystemManager) workingDir
							.getFileSystem().getFileSystemManager();
					manager.close();
				} catch (final FileSystemException e) {
					// gobble
				}
			}
		}
	}

	@SuppressWarnings("unchecked")
	private GMLWorkspace buildBreaklinesResult(final FileObject workingDir,
			final int boundaryCount) throws Exception {
		// try to read breaklines
		InputStream inputStream = null;
		try {
			final URL templateFile = getClass().getResource(
					BREAKLINES_GML_TEMPLATE);
			inputStream = templateFile.openStream();
			final GMLWorkspace breaklinesWorkspace = GmlSerializer
					.createGMLWorkspace(inputStream, null, null);
			final Feature breaklinesFeature = breaklinesWorkspace
					.getRootFeature();
			final List<String> breaklinesList = (List<String>) breaklinesFeature
					.getProperty(Gaja3dUrlCatalog.PROPERTY_BREAKLINES);
			for (int i = 0; i < boundaryCount; i++) {
				final String destName;
				if (boundaryCount == 1)
					destName = "Breaklines.zip";
				else
					destName = String.format("Breaklines_%04d.zip", (i + 1));
				final FileObject destFile = workingDir.resolveFile(destName);
				final FileName destFileName = destFile.getName();
				if (!destFile.exists())
					throw new SimulationException(
							"Required output was not found: " + destFileName);
				final String destUri = destFileName.getURI();
				final URI outputLocation = new URI(destUri);
				breaklinesList.add(outputLocation.toString());
			}
			return breaklinesWorkspace;
		} finally {
			IOUtils.closeQuietly(inputStream);
		}
	}
}