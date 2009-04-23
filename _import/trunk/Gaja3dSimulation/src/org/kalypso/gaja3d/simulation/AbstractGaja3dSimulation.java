package org.kalypso.gaja3d.simulation;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.apache.commons.vfs.FileContent;
import org.apache.commons.vfs.FileObject;
import org.apache.commons.vfs.FileSystem;
import org.apache.commons.vfs.FileSystemManager;
import org.apache.commons.vfs.impl.StandardFileSystemManager;
import org.kalypso.commons.io.VFSUtilities;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.simulation.grid.GridJobSubmitter;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

import uk.ac.dl.escience.vfs.util.VFSUtil;

public abstract class AbstractGaja3dSimulation {

	public static final String EXECUTABLE_NAME = "Gaja3dService_linux64.sh";

	public static final String INPUT_BOUNDARY = "Boundary";

	private static final URL EXEC_ZIP_URL = AbstractGaja3dSimulation.class
			.getResource("resources/Gaja3dService_linux64.zip");

	private static final URL EXEC_SCRIPT_URL = AbstractGaja3dSimulation.class
			.getResource("resources/" + EXECUTABLE_NAME);

	private static final String WORKING_DIR = ".";

	protected final ArrayList<String> m_arguments = new ArrayList<String>();
	protected final GridJobSubmitter m_jobSubmitter = new GridJobSubmitter();

	public static final String GRID_SERVER_ROOT = "gridftp://gramd1.gridlab.uni-hannover.de";

	public AbstractGaja3dSimulation() {
		try {
			m_jobSubmitter.addExternalInput(EXEC_ZIP_URL.toURI(), null);
			m_jobSubmitter.addExternalInput(EXEC_SCRIPT_URL.toURI(), null);
		} catch (final Exception e) {
			// could be null or not a valid uri
			throw new RuntimeException("Problem with executable.", e);
		}
		// always add workingDir argument
		m_arguments.add("workingDir");
		m_arguments.add(WORKING_DIR);
	}

	@SuppressWarnings("unchecked")
	protected List<String> getGmlList(final Object inputForBoundary,
			final QName propertyName) throws SimulationException {
		final GMLWorkspace boundariesWorkspace = (GMLWorkspace) inputForBoundary;
		final Feature boundariesFeature = boundariesWorkspace.getRootFeature();
		final Object property = boundariesFeature.getProperty(propertyName);
		final List<String> boundaryList = (List<String>) property;
		return boundaryList;
	}

	protected void addReferencedInput(
			final ISimulationDataProvider inputProvider, final String id,
			final boolean required) throws SimulationException {
		if (inputProvider.hasID(id)) {
			m_arguments.add(id);
			final Object inputForID = inputProvider.getInputForID(id);
			if (inputForID instanceof URI) {
				// if it is a URI, stage in a file for given URI
				final URI inputURL = (URI) inputForID;
				m_jobSubmitter.addExternalInput(inputURL, null);
				final String inputURLString = inputURL.toString();
				final String inputBaseName = inputURLString
						.substring(inputURLString.lastIndexOf('/') + 1);
				// add local file (on grid node) as argument
				m_arguments.add(inputBaseName);
			} else if (inputForID instanceof URL) {
				// if it is a URL, stage in a file for given URL
				final URL inputURL = (URL) inputForID;
				try {
					m_jobSubmitter.addExternalInput(inputURL.toURI(), null);
				} catch (final URISyntaxException e) {
					throw new SimulationException(
							"The input URL is not a valid URI for staging.", e);
				}
				final String inputURLString = inputURL.toExternalForm();
				final String inputBaseName = inputURLString
						.substring(inputURLString.lastIndexOf('/') + 1);
				// add local file (on grid node) as argument
				m_arguments.add(inputBaseName);
			} else {
				// regular file or input
				final String inputString = inputForID.toString();
				// add the string representation of the input as an argument
				// this will at least work for strings and numeric types
				m_arguments.add(inputString);
			}
		} else if (required)
			throw new SimulationException("Missing input " + id);
	}

	/**
	 * 
	 * Create working dir (sandbox)
	 * 
	 * @param tmpdir
	 * @return
	 * @throws SimulationException
	 */
	protected FileObject getWorkingDir(final File tmpdir)
			throws SimulationException {
		try {
			final FileSystemManager manager = VFSUtilities.getNewManager();
			final String sandboxRoot = tmpdir.getName();
			final FileObject remoteRoot = manager.resolveFile(GRID_SERVER_ROOT);
			final FileSystem fileSystem = remoteRoot.getFileSystem();
			final String homeDirString = (String) fileSystem
					.getAttribute("HOME_DIRECTORY");
			final FileObject homeDir = remoteRoot.resolveFile(homeDirString);
			final FileObject workingDir = homeDir.resolveFile(sandboxRoot);
			workingDir.createFolder();
			return workingDir;
		} catch (final IOException e) {
			throw new SimulationException(
					"Problem initializing working directory on grid server.", e);
		}
	}

	protected void copyToRemote(final List<String> fileList,
			final String fileSpec, final FileObject workingDir)
			throws SimulationException {
		try {
			final StandardFileSystemManager manager = (StandardFileSystemManager) workingDir
					.getFileSystem().getFileSystemManager();
			for (int i = 0; i < fileList.size(); i++) {
				final String location = fileList.get(i);
				final FileObject file = manager.resolveFile(location);
				final FileObject destFile = workingDir.resolveFile(String
						.format(fileSpec, (i + 1)));
				VFSUtil.copy(file, destFile, null, true);
			}
		} catch (final Exception e) {
			throw new SimulationException("Problem copying files.", e);
		}
	}

	protected void mergeZipsInWorking(final List<String> boundaryList,
			final String zipName, final FileObject workingDir)
			throws SimulationException {
		try {
			final StandardFileSystemManager manager = (StandardFileSystemManager) workingDir
					.getFileSystem().getFileSystemManager();
			final File temp = FileUtilities.createNewTempDir("zipsTemp",
					FileUtilities.TMP_DIR);
			temp.deleteOnExit();
			for (final String boundaryLocation : boundaryList) {
				final FileObject boundaryZipFile = manager
						.resolveFile(boundaryLocation);
				final FileContent content = boundaryZipFile.getContent();
				final InputStream inputStream = content.getInputStream();
				ZipUtilities.unzip(inputStream, temp);
				content.close();
			}

			final FileObject allBoundariesZip = workingDir.resolveFile(zipName);
			final FileContent allBoundariesContent = allBoundariesZip
					.getContent();
			final OutputStream outputStream = allBoundariesContent
					.getOutputStream();
			ZipUtilities.zip(outputStream, temp.listFiles(), temp);
			allBoundariesContent.close();
		} catch (final Exception e) {
			throw new SimulationException("Problem merging zip files.", e);
		}
	}
}