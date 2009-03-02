package org.kalypso.gaja3d.service.impl;

import java.io.StringReader;
import java.rmi.RemoteException;

import javax.security.auth.Subject;

import net.opengeospatial.ows.stubs.GetCapabilitiesType;
import net.opengeospatial.wps.stubs.Capabilities;
import net.opengeospatial.wps.stubs.DescribeProcess;
import net.opengeospatial.wps.stubs.Execute;
import net.opengeospatial.wps.stubs.ExecuteResponseType;
import net.opengeospatial.wps.stubs.ProcessDescriptions;

import org.apache.axis.message.MessageElement;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.globus.gsi.jaas.JaasSubject;
import org.globus.wsrf.Resource;
import org.globus.wsrf.ResourceContext;
import org.globus.wsrf.container.Activator;
import org.globus.wsrf.encoding.ObjectDeserializer;
import org.globus.wsrf.security.SecurityManager;
import org.kalypso.gaja3d.service.stubs.Breaklines;
import org.kalypso.gaja3d.service.stubs.CreateGridParametersType;
import org.kalypso.gaja3d.service.stubs.CreateGridResponseType;
import org.kalypso.gaja3d.service.stubs.CreateTinResponseType;
import org.kalypso.gaja3d.service.stubs.DemGrid;
import org.kalypso.gaja3d.service.stubs.DetectBreaklinesParametersType;
import org.kalypso.gaja3d.service.stubs.DetectBreaklinesResponseType;
import org.kalypso.gaja3d.service.stubs.DistanceTolerance;
import org.kalypso.gaja3d.service.stubs.EdgeFilter;
import org.kalypso.gaja3d.service.stubs.FeatureDetector;
import org.kalypso.gaja3d.service.stubs.Gaja3DResourcePropertyLinkType;
import org.kalypso.gaja3d.service.stubs.GridX;
import org.kalypso.gaja3d.service.stubs.GridY;
import org.kalypso.gaja3d.service.stubs.MaxArea;
import org.kalypso.gaja3d.service.stubs.MinAngle;
import org.kalypso.gaja3d.service.stubs.ModelTin;
import org.kalypso.gaja3d.service.stubs.SmoothFilter;
import org.kalypso.service.wps.utils.WPSUtilities;
import org.xml.sax.InputSource;

public class Gaja3dService {

	/**
	 * System property must specify the WPS to delegate to
	 */
	private static final String WPS_URL = System
			.getProperty("org.kalypso.service.wps.service");

	private Log logger = LogFactory.getLog(Activator.class.getName());

	/*
	 * Private method that gets a reference to the resource specified in the
	 * endpoint reference.
	 */
	private Gaja3dResource getResource() throws RemoteException {
		Object resource = null;
		try {
			resource = ResourceContext.getResourceContext().getResource();
		} catch (final Exception e) {
			throw new RemoteException("", e);
		}
		return (Gaja3dResource) resource;
	}

	/* Implementation of service operations */

	/**
	 * This standard web method is automatically generated to give a full
	 * description of this operation. The response will be an embedded WPS XML
	 * GetCapabilities Response. Please consult the OGC Web Site for the WPS
	 * 1.0.0 specification.
	 */
	public Capabilities getCapabilities(final GetCapabilitiesType parameters)
			throws RemoteException {
		final Gaja3dResource resource = getResource();
		logSecurityInfo("getCapabilities", resource);

		try {
			// extract request as string
			final MessageElement getCapabilities = new MessageElement(
					WPSQNames.GET_CAPABILITIES);
			getCapabilities.setObjectValue(parameters);
			final String requestString = getCapabilities.toString();

			// call service
			final String responseString = WPSUtilities.send(requestString,
					WPS_URL);

			// create response
			final InputSource is = new InputSource(new StringReader(
					responseString));
			final Capabilities capabilities = (Capabilities) ObjectDeserializer
					.deserialize(is, Capabilities.class);
			return capabilities;
		} catch (final Exception e) {
			throw new RemoteException("", e);
		}
	}

	/**
	 * This standard web method is automatically generated to give a full
	 * description of all processes provided by this operation. The response
	 * will be an embedded WPS XML DescribeProcess Response. Please consult the
	 * OGC Web Site for the WPS 0.4.0 specification.
	 */
	public ProcessDescriptions describeProcess(final DescribeProcess parameters)
			throws RemoteException {
		final Gaja3dResource resource = getResource();
		logSecurityInfo("describeProcess", resource);

		try {
			// extract request as string
			final MessageElement describeProcess = new MessageElement(
					WPSQNames.DESCRIBE_PROCESS);
			describeProcess.setObjectValue(parameters);
			final String requestString = describeProcess.toString();

			// call service
			final String responseString = WPSUtilities.send(requestString,
					WPS_URL);

			// create response
			final InputSource is = new InputSource(new StringReader(
					responseString));
			final ProcessDescriptions processDescriptions = (ProcessDescriptions) ObjectDeserializer
					.deserialize(is, ProcessDescriptions.class);
			return processDescriptions;
		} catch (final Exception e) {
			throw new RemoteException("", e);
		}
	}

	/**
	 * This is the generic Execute operation. As WPS does not currently support
	 * security, this call will fail if the current security context is needed
	 * for subsequent actions (e.g. job submission) by the called WPS
	 */
	public ExecuteResponseType execute(final Execute parameters)
			throws RemoteException {
		final Gaja3dResource resource = getResource();
		logSecurityInfo("execute", resource);

		try {
			// extract request as string
			final MessageElement execute = new MessageElement(WPSQNames.EXECUTE);
			execute.setObjectValue(parameters);
			final String requestString = execute.toString();

			// call service
			final String responseString = WPSUtilities.send(requestString,
					WPS_URL);

			// create response
			final InputSource is = new InputSource(new StringReader(
					responseString));
			final ExecuteResponseType executeResponse = (ExecuteResponseType) ObjectDeserializer
					.deserialize(is, ExecuteResponseType.class);
			return executeResponse;
		} catch (final Exception e) {
			throw new RemoteException("", e);
		}
	}

	/**
	 * @param parameters
	 * @return
	 * @throws RemoteException
	 */
	public CreateGridResponseType execute_createGrid(
			final CreateGridParametersType parameters) throws RemoteException {
		final Gaja3dResource resource = getResource();
		logSecurityInfo("execute_createGrid", resource);

		// required
		final Gaja3DResourcePropertyLinkType boundary = parameters
				.getBoundary();
		if (boundary != null)
			resource.setBoundary(boundary);

		// required
		final Gaja3DResourcePropertyLinkType demPoints = parameters
				.getDemPoints();
		if (demPoints != null)
			resource.setDemPoints(demPoints);

		// required
		final GridX gridX = parameters.getGridX();
		if (gridX != null)
			resource.setGridX(gridX);

		// required
		final GridY gridY = parameters.getGridY();
		if (gridY != null)
			resource.setGridY(gridY);

		resource.createGrid();

		final CreateGridResponseType createGridResponseType = new CreateGridResponseType();
		final DemGrid demGrid = resource.getDemGrid();
		createGridResponseType.setDemGrid(demGrid);
		return createGridResponseType;
	}

	/**
	 * @param parameters
	 * @return
	 * @throws java.rmi.RemoteException
	 */
	public DetectBreaklinesResponseType execute_detectBreaklines(
			final DetectBreaklinesParametersType parameters)
			throws RemoteException {
		final Gaja3dResource resource = getResource();
		logSecurityInfo("execute_detectBreaklines", resource);

		// required
		final Gaja3DResourcePropertyLinkType boundary = parameters
				.getBoundary();
		if (boundary != null)
			resource.setBoundary(boundary);

		// required
		final DemGrid demGrid = parameters.getDemGrid();
		if (demGrid != null)
			resource.setDemGrid(demGrid);

		// optional
		final EdgeFilter edgeFilter = parameters.getEdgeFilter();
		if (edgeFilter != null)
			resource.setEdgeFilter(edgeFilter);

		// optional
		final SmoothFilter smoothFilter = parameters.getSmoothFilter();
		if (smoothFilter != null)
			resource.setSmoothFilter(smoothFilter);

		// optional
		final FeatureDetector featureDetector = parameters.getFeatureDetector();
		if (featureDetector != null)
			resource.setFeatureDetector(featureDetector);

		// optional
		final DistanceTolerance distanceTolerance = parameters
				.getDistanceTolerance();
		if (distanceTolerance != null)
			resource.setDistanceTolerance(distanceTolerance);

		resource.detectBreaklines();

		final DetectBreaklinesResponseType detectBreaklinesResponseType = new DetectBreaklinesResponseType();
		final Breaklines breaklines = resource.getBreaklines();
		detectBreaklinesResponseType.setBreaklines(breaklines);
		return detectBreaklinesResponseType;
	}

	/**
	 * @param parameters
	 * @return
	 * @throws java.rmi.RemoteException
	 */
	public org.kalypso.gaja3d.service.stubs.CreateTinResponseType execute_createTin(
			org.kalypso.gaja3d.service.stubs.CreateTinParametersType parameters)
			throws RemoteException {
		final Gaja3dResource resource = getResource();
		logSecurityInfo("execute_createTin", resource);

		// required
		final Gaja3DResourcePropertyLinkType boundary = parameters
				.getBoundary();
		if (boundary != null)
			resource.setBoundary(boundary);

		// optional
		final DemGrid demGrid = parameters.getDemGrid();
		if (demGrid != null)
			resource.setDemGrid(demGrid);

		// required
		final Breaklines breaklines = parameters.getBreaklines();
		if (breaklines != null)
			resource.setBreaklines(breaklines);

		// optional
		final MaxArea maxArea = parameters.getMaxArea();
		if (maxArea != null)
			resource.setMaxArea(maxArea);

		// optional
		final MinAngle minAngle = parameters.getMinAngle();
		if (minAngle != null)
			resource.setMinAngle(minAngle);

		resource.createTin();

		final CreateTinResponseType createTinResponseType = new CreateTinResponseType();
		final ModelTin modelTin = resource.getModelTin();
		createTinResponseType.setModelTin(modelTin);
		return createTinResponseType;
	}

	public void logSecurityInfo(final String methodName, final Resource resource) {
		Subject subject;
		logger.debug("SECURITY INFO FOR METHOD '" + methodName + "'");

		// Print out the caller
		String identity = SecurityManager.getManager().getCaller();
		logger.debug("The caller is:" + identity);

		// Print out the invocation subject
		subject = JaasSubject.getCurrentSubject();
		logger.debug("INVOCATION SUBJECT");
		logger.debug(subject == null ? "NULL" : subject.toString());

		try {
			// Print out service subject
			logger.debug("SERVICE SUBJECT");
			subject = SecurityManager.getManager().getServiceSubject();
			logger.debug(subject == null ? "NULL" : subject.toString());
		} catch (Exception e) {
			logger.debug("Unable to obtain service subject");
		}

		try {
			// Print out system subject
			logger.debug("SYSTEM SUBJECT");
			subject = SecurityManager.getManager().getSystemSubject();
			logger.debug(subject == null ? "NULL" : subject.toString());
		} catch (Exception e) {
			logger.debug("Unable to obtain system subject");
		}

		logger.debug("RESOURCE SUBJECT");
		if (resource == null) {
			logger.debug("No resource");
		} else {
			try {
				subject = SecurityManager.getManager().getSubject(resource);
				logger.debug(subject == null ? "NULL" : subject.toString());
			} catch (Exception e) {
				logger.debug("Unable to obtain resource subject");
			}
		}
	}
}