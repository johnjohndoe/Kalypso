/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.simulation.grid;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IStatus;
import org.gridlab.gat.resources.Job.JobState;
import org.kalypso.commons.xml.NS;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.NullSimulationMonitor;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.simulation.core.simspec.DataType;
import org.kalypso.simulation.core.simspec.Modelspec;

import de.unihannover.rvs.gdi.jobsubmit.impl.GDIFileTransfer;
import de.unihannover.rvs.gdi.jobsubmit.impl.GDIJobFactory;
import de.unihannover.rvs.gdi.jobsubmit.interfaces.GDIJob;
import de.unihannover.rvs.gdi.jobsubmit.interfaces.GDIObserver;
import de.unihannover.rvs.gdi.jobsubmit.interfaces.GDIObserverSubject;

public class SimulationGridJobSubmitter {

	/**
	 * AnyURI QName.
	 */
	public static QName QNAME_ANY_URI = new QName(NS.XSD_SCHEMA, "anyURI");

	/**
	 * Status gets updated when job is submitted
	 */
	private JobState m_status = JobState.INITIAL;
	private final String m_host;
	private final URL m_executableZIP;
	private final String m_executableScript;

	/**
	 * Jobs run on linux only, so this is the correct file separator to use
	 */
	private static final char FILE_SEP = '/';

	public SimulationGridJobSubmitter(final String host,
			final URL executableZIP, String executableScript) {
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
			ISimulationMonitor monitor, List<String> arguments)
			throws SimulationException {
		// check if arguments are really available
		if (arguments == null) {
			arguments = new ArrayList<String>();
		}
		if (monitor == null) {
			monitor = new NullSimulationMonitor();
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
		String executableScriptName = getExecutableScript();
		String remoteExecutableScriptName = executableScriptName;
		URL executableScriptURL = null;
		try {
			// try to get executable resources as file URL
			if (executableZIPURL != null) {
				executableZIPURL = FileLocator.toFileURL(executableZIPURL);
				// add executable ZIP to stage-in
				// name it as last section of file URL
				final String localExecutableZIPName = executableZIPURL
						.toExternalForm();
				final String remoteExecutableZIPName = localExecutableZIPName
						.substring(localExecutableZIPName.lastIndexOf(FILE_SEP) + 1);
				fileStageIn.add(new GDIFileTransfer(localExecutableZIPName,
						remoteExecutableZIPName));
			}

			try {
				executableScriptURL = new URL(executableScriptName);
				executableScriptURL = FileLocator
						.toFileURL(executableScriptURL);
			} catch (MalformedURLException e) {
				// ignore
			}

			if (executableScriptURL != null) {
				// add executable script to stage-in
				// name it as last section of file URL
				final String localExecutableScriptName = executableScriptURL
						.toExternalForm();
				remoteExecutableScriptName = localExecutableScriptName
						.substring(localExecutableScriptName
								.lastIndexOf(FILE_SEP) + 1);
				fileStageIn.add(new GDIFileTransfer(localExecutableScriptName,
						remoteExecutableScriptName));
			}
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
		myJob.setStdOut(stdOutFileName);
		myJob.setStdErr(stdErrFileName);
		myJob.setStdIn(stdIn);
		myJob.setFileStageIn(fileStageIn);
		myJob.setFileStageOut(fileStageOut);
		myJob.setQueue("dgitest");
		// register observer
		final ISimulationMonitor monitorFinal = monitor;
		((GDIObserverSubject) myJob).registerObserver(new GDIObserver() {
			public void update(final JobState newStatus) {
				m_status = newStatus;
				monitorFinal.setMessage(m_status.toString());
				if (JobState.STOPPED == m_status
						|| JobState.SUBMISSION_ERROR == m_status
						|| JobState.UNKNOWN == m_status)
					monitorFinal.setProgress(99);
			}
		});
		myJob.submit();

		// TODO
		final OutputStream localJobOut = System.out;

		// wait for FINISHED or FAILED status
		int total = 0;
		finished: while (!monitor.isCanceled()) {
			switch (m_status) {
			case STOPPED:
			case SUBMISSION_ERROR:
			case UNKNOWN:
				break finished;
			case RUNNING:
				final InputStream remoteJobOut = myJob.getStdout();
				total += copyStreamFromOffset(remoteJobOut, localJobOut, total);
			default:
				try {
					Thread.sleep(1000);
				} catch (final InterruptedException e) {
					e.printStackTrace();
				}
			}
		}

		if (JobState.SUBMISSION_ERROR == m_status
				|| JobState.UNKNOWN == m_status)
			monitor.setFinishInfo(IStatus.ERROR, "Grid job failed!");
	}

	private int copyStreamFromOffset(final InputStream in,
			final OutputStream out, final int offset) {
		final BufferedInputStream is = new BufferedInputStream(in);
		int total = 0;
		try {
			is.skip(offset);
			byte[] buf = new byte[512];
			int count = is.read(buf);
			while (count != -1) {
				out.write(buf, 0, count);
				total += count;
				count = is.read(buf);
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
		return total;
	}

	private String getHost() {
		return m_host;
	}

	protected URL getExecutableZIP() {
		return m_executableZIP;
	}

	private String getExecutableScript() {
		return m_executableScript;
	}
}