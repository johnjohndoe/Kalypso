/**
 * Example program using the GDIJob API
 */
package org.kalypso.gaja3d.simulation.grid;

import java.io.File;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.simulation.core.simspec.Modelspec;

/**
 * Submits a Gaja3d job to the grid using ISimulation inputs/outputs
 * 
 * @author skurzbach
 * 
 */
public class Gaja3dGridJobSubmitter extends SimulationGridJobSubmitter {
	/**
	 * Submit jobs to this GRAM
	 */
	static final String GRAM_HOST = "gramd1.d-grid.uni-hannover.de";

	/**
	 * Name of the service. Will determine the name of the executable package
	 * (zip) and script (sh).
	 */
	private static final URL EXEC_ZIP_URL = Gaja3dGridJobSubmitter.class
			.getResource("Gaja3dService.zip");
	private static final URL EXEC_SCRIPT_URL = Gaja3dGridJobSubmitter.class
			.getResource("Gaja3dService.sh");

	/**
	 * The working directory of Gaja3d
	 */
	static final String WORKING_DIR = ".";

	public Gaja3dGridJobSubmitter() {
		super(GRAM_HOST, EXEC_ZIP_URL, EXEC_SCRIPT_URL);
	}

	@Override
	public void submitJob(Modelspec modelSpec, File tmpdir,
			ISimulationDataProvider inputProvider,
			ISimulationResultEater resultEater, ISimulationMonitor monitor,
			List<String> arguments) throws SimulationException {
		// check if arguments are really available
		if (arguments == null) {
			arguments = new ArrayList<String>();
		}

		// always add workingDir argument
		arguments.add("workingDir");
		arguments.add(WORKING_DIR);
		super.submitJob(modelSpec, tmpdir, inputProvider, resultEater, monitor,
				arguments);
	}
}
