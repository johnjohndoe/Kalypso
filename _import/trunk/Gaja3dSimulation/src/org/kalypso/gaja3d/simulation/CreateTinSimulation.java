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
import java.net.URI;
import java.net.URL;
import java.util.List;

import org.apache.commons.vfs.FileName;
import org.apache.commons.vfs.FileObject;
import org.apache.commons.vfs.FileSystemException;
import org.apache.commons.vfs.impl.StandardFileSystemManager;
import org.kalypso.gaja3d.simulation.grid.Gaja3dGridJobSubmitter;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;

/**
 * @author kurzbach
 */
public class CreateTinSimulation extends AbstractGaja3dSimulation implements
		ISimulation {
	/**
	 * The model specification.
	 */
	private static final String SIMULATION_SPEC = "createTin_specification.xml";

	public static final String INPUT_BOUNDARY = "Boundary";

	public static final String INPUT_BREAKLINES = "Breaklines";

	public static final String INPUT_MAX_AREA = "MaxArea";

	public static final String INPUT_MIN_ANGLE = "MinAngle";

	public static final String INPUT_DEM_GRID = "DemGrid";

	public static final String OUTPUT_MODEL_TIN = "ModelTin";

	public static final String ID = "Gaja3d_createTin";

	/**
	 * The constructor.
	 */
	public CreateTinSimulation() {
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
		m_arguments.add("createTin");
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

			if (inputProvider.hasID(INPUT_DEM_GRID)) {
				m_arguments.add("assignElevations");
				m_arguments.add("true");
				final Object inputForDemGrid = inputProvider
						.getInputForID(INPUT_DEM_GRID);
				final List<String> demGridList = getGmlList(inputForDemGrid,
						Gaja3dUrlCatalog.PROPERTY_DEM_GRID);
				final String demGridSpec = "DemGrid_%04d.asc";
				copyToRemote(demGridList, demGridSpec, workingDir);
				m_arguments.add(INPUT_DEM_GRID);
				m_arguments.add("DemGrid_*.asc");
			}

			if (inputProvider.hasID(INPUT_BREAKLINES)) {
				final Object inputForBreaklines = inputProvider
						.getInputForID(INPUT_BREAKLINES);
				final List<String> breaklinesList = getGmlList(
						inputForBreaklines,
						Gaja3dUrlCatalog.PROPERTY_BREAKLINES);
				final String breaklinesSpec = "Breaklines.zip";
				mergeZipsInWorking(breaklinesList, breaklinesSpec, workingDir);
				m_arguments.add(INPUT_BREAKLINES);
				m_arguments.add(breaklinesSpec);
			}

			addReferencedInput(inputProvider, INPUT_MAX_AREA, false);
			addReferencedInput(inputProvider, INPUT_MIN_ANGLE, false);

			final Gaja3dGridJobSubmitter jobSubmitter = new Gaja3dGridJobSubmitter();
			jobSubmitter.submitJob(workingDir, monitor, m_arguments);

			final FileObject modelTinFile = workingDir
					.resolveFile("ModelTin.zip");
			final FileName modelTinFileName = modelTinFile.getName();
			final String modelTinUri = modelTinFileName.getURI();
			if (!modelTinFile.exists())
				throw new SimulationException("Required output was not found: "
						+ modelTinUri);
			final URI modelTinLocation = new URI(modelTinUri);
			resultEater.addResult(OUTPUT_MODEL_TIN, modelTinLocation);
		} catch (final SimulationException e) {
			throw e;
		} catch (final Exception e) {
			throw new SimulationException(
					"Problem during tin creation.", e);
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
}