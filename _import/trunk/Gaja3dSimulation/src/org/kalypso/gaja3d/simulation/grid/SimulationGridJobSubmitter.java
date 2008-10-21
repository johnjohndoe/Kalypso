package org.kalypso.gaja3d.simulation.grid;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.xml.NS;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.simulation.core.simspec.DataType;
import org.kalypso.simulation.core.simspec.Modelspec;

import de.unihannover.rvs.gdi.jobsubmit.impl.GDIFileTransfer;
import de.unihannover.rvs.gdi.jobsubmit.impl.GDIJobFactory;
import de.unihannover.rvs.gdi.jobsubmit.interfaces.GDIJob;
import de.unihannover.rvs.gdi.jobsubmit.interfaces.GDIObserver;
import de.unihannover.rvs.gdi.jobsubmit.interfaces.GDIObserverSubject;

public abstract class SimulationGridJobSubmitter {

	/**
	 * AnyURI QName.
	 */
	public static QName QNAME_ANY_URI = new QName(NS.XSD_SCHEMA, "anyURI");

	private String m_status;
	private final String m_host;
	private final URL m_executableZIP;
	private final URL m_executableScript;
	/**
	 * Jobs run on linux only, so this is the correct file separator to use
	 */
	private static final char FILE_SEP = '/';

	public SimulationGridJobSubmitter(final String host,
			final URL executableZIP, URL executableScript) {
		m_host = host;
		m_executableZIP = executableZIP;
		m_executableScript = executableScript;
	}

	/**
	 * This method submits a Job using a GDIJob instance.
	 * 
	 * @param tmpdir
	 * @param monitor
	 * @param resultEater
	 * @param inputProvider
	 * @throws SimulationException
	 */
	public void submitJob(final Modelspec modelSpec, final File tmpdir,
			final ISimulationDataProvider inputProvider,
			final ISimulationResultEater resultEater,
			final ISimulationMonitor monitor, List<String> arguments)
			throws SimulationException {
		// check if arguments are really available
		if (arguments == null) {
			arguments = new ArrayList<String>();
		}

		// setup logging output
		final String stdOutFileName = "stdout";
		final String stdErrFileName = "stderr";
		final String stdOut = new File(tmpdir, stdOutFileName).toURI()
				.toString();
		final String stdErr = new File(tmpdir, stdErrFileName).toURI()
				.toString();
		resultEater.addResult(stdOutFileName, stdOut);
		resultEater.addResult(stdErrFileName, stdErr);
		// no standard in!
		final String stdIn = null;

		// keep track of files to stage in and out
		final List<GDIFileTransfer> fileStageIn = new ArrayList<GDIFileTransfer>();
		final List<GDIFileTransfer> fileStageOut = new ArrayList<GDIFileTransfer>();

		URL executableZIPURL = getExecutableZIP();
		URL executableScriptURL = getExecutableScript();
		String remoteExecutableScriptName = null;
		try {
			// try to get executable resources as file URL
			executableZIPURL = FileLocator.toFileURL(executableZIPURL);
			executableScriptURL = FileLocator.toFileURL(executableScriptURL);

			// add executable ZIP to stage-in
			// name it as last section of file URL
			final String localExecutableZIPName = executableZIPURL
					.toExternalForm();
			final String remoteExecutableZIPName = localExecutableZIPName
					.substring(localExecutableZIPName.lastIndexOf(FILE_SEP) + 1);
			fileStageIn.add(new GDIFileTransfer(localExecutableZIPName,
					remoteExecutableZIPName));

			// add executable script to stage-in
			// name it as last section of file URL
			final String localExecutableScriptName = executableScriptURL
					.toExternalForm();
			remoteExecutableScriptName = localExecutableScriptName
					.substring(localExecutableScriptName.lastIndexOf(FILE_SEP) + 1);
			fileStageIn.add(new GDIFileTransfer(localExecutableScriptName,
					remoteExecutableScriptName));
		} catch (final IOException e1) {
			throw new SimulationException("Problem with executable "
					+ executableZIPURL.toExternalForm() + " or "
					+ executableScriptURL.toExternalForm(), e1);
		}

		// keep track of output names that were mentioned in the inputs
		// map from output id to output name
		final Map<String, String> outputNames = new HashMap<String, String>();

		// convert inputs to command line arguments
		final List<DataType> input = modelSpec.getInput();
		for (final DataType data : input) {
			final String id = data.getId();
			if (!inputProvider.hasID(id) && data.isOptional())
				// ignore missing optional inputs
				continue;
			final Object inputForID = inputProvider.getInputForID(id);
			if (id.startsWith("_")) {
				// this input corresponds to an output and specifies the file
				// name of the output, do not include as input argument
				outputNames.put(id.substring(1), (String) inputForID);
				continue;
			}
			// add key-value argument pair
			// id of input is the key
			arguments.add(id);
			// value depends on type of input
			final QName dataInputType = data.getType();
			if (dataInputType.equals(QNAME_ANY_URI)) {
				// if it is a URI, stage in a file for given URL
				final URL inputURL = (URL) inputForID;
				final String inputURLString = inputURL.toExternalForm();
				final String gridNodeFileString = inputURLString
						.substring(inputURLString.lastIndexOf(FILE_SEP) + 1);
				// transfer from input URL to grid node
				fileStageIn.add(new GDIFileTransfer(inputURLString,
						gridNodeFileString));
				// add local file (on grid node) as argument
				arguments.add(gridNodeFileString);
			} else {
				// regular file or input
				final String inputString = inputForID.toString();
				// add the string representation of the input as an argument
				// this will at least work for strings and numeric types
				arguments.add(inputString);
			}
		}

		// check output arguments
		final List<DataType> output = modelSpec.getOutput();
		for (final DataType data : output) {
			final String id = data.getId();
			// only URI is supported for outputs at the time
			if (data.getType().equals(QNAME_ANY_URI)) {
				// stage out file
				final String outputName = outputNames.get(id);
				final String source;
				if (outputName != null)
					source = outputName;
				else
					source = id;
				final File outputLocation = new File(tmpdir, source);
				final URI uri = outputLocation.toURI();
				final String destination = uri.toString();
				fileStageOut.add(new GDIFileTransfer(source, destination));
				resultEater.addResult(id, outputLocation);
			}
			// TODO: support literal outputs
		}

		// setup grid job
		final GDIJob myJob = GDIJobFactory
				.createGDIJob(GDIJobFactory.defaultGDIJob);
		myJob.setTargetHostName(getHost());
		myJob.setExecutable(remoteExecutableScriptName);
		myJob.setArguments(arguments);
		myJob.setStdOut(stdOut);
		myJob.setStdErr(stdErr);
		myJob.setStdIn(stdIn);
		myJob.setFileStageIn(fileStageIn);
		myJob.setFileStageOut(fileStageOut);
		// register observer
		((GDIObserverSubject) myJob).registerObserver(new GDIObserver() {
			public void update(final String newStatus) {
				m_status = newStatus;
				monitor.setMessage(m_status);
				if ("FINISHED".equals(m_status))
					monitor.setProgress(99);
			}
		});
		myJob.submit();

		// wait for FINISHED or FAILED status
		while (!monitor.isCanceled()
				&& !("FINISHED".equals(monitor.getMessage()) || "FAILED"
						.equals(monitor.getMessage()))) {
			try {
				Thread.sleep(1000);
			} catch (final InterruptedException e) {
				e.printStackTrace();
			}
		}

		if ("FAILED".equals(m_status))
			monitor.setFinishInfo(IStatus.ERROR, "Grid job failed!");
	}

	private String getHost() {
		return m_host;
	}

	protected URL getExecutableZIP() {
		return m_executableZIP;
	}

	private URL getExecutableScript() {
		return m_executableScript;
	}
}