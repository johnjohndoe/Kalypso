package org.kalypso.gaja3d.service.impl;

import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.rmi.RemoteException;
import java.util.concurrent.Semaphore;

import javax.xml.namespace.QName;

import org.apache.axis.AxisFault;
import org.apache.axis.components.uuid.UUIDGen;
import org.apache.axis.components.uuid.UUIDGenFactory;
import org.apache.axis.message.addressing.EndpointReference;
import org.apache.axis.types.URI;
import org.apache.axis.types.URI.MalformedURIException;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.globus.wsrf.RemoveCallback;
import org.globus.wsrf.ResourceException;
import org.globus.wsrf.ResourceIdentifier;
import org.globus.wsrf.ResourceLifetime;
import org.globus.wsrf.ResourceProperties;
import org.globus.wsrf.ResourceProperty;
import org.globus.wsrf.ResourcePropertyMetaData;
import org.globus.wsrf.ResourcePropertySet;
import org.globus.wsrf.TopicList;
import org.globus.wsrf.TopicListAccessor;
import org.globus.wsrf.config.ConfigException;
import org.globus.wsrf.impl.PersistentReflectionResource;
import org.globus.wsrf.impl.ResourcePropertyTopic;
import org.globus.wsrf.impl.SimpleTopicList;
import org.globus.wsrf.impl.security.descriptor.ResourceSecurityConfig;
import org.globus.wsrf.impl.security.descriptor.ResourceSecurityDescriptor;
import org.globus.wsrf.security.SecureResource;
import org.kalypso.gaja3d.service.internal.CodeTypeUtil;
import org.kalypso.gaja3d.service.internal.strategy.CreateGridStrategy;
import org.kalypso.gaja3d.service.internal.strategy.CreateTinStrategy;
import org.kalypso.gaja3d.service.internal.strategy.DetectBreaklinesStrategy;
import org.kalypso.gaja3d.service.internal.strategy.WPSCreateGridStrategy;
import org.kalypso.gaja3d.service.internal.strategy.WPSCreateTinStrategy;
import org.kalypso.gaja3d.service.internal.strategy.WPSDetectBreaklinesStrategy;
import org.kalypso.gaja3d.service.stubs.Breaklines;
import org.kalypso.gaja3d.service.stubs.DemGrid;
import org.kalypso.gaja3d.service.stubs.DistanceTolerance;
import org.kalypso.gaja3d.service.stubs.EdgeFilter;
import org.kalypso.gaja3d.service.stubs.EdgeFilterMethod;
import org.kalypso.gaja3d.service.stubs.FeatureDetector;
import org.kalypso.gaja3d.service.stubs.FeatureDetectorMethod;
import org.kalypso.gaja3d.service.stubs.Gaja3DResourceProperties;
import org.kalypso.gaja3d.service.stubs.Gaja3DResourcePropertyLinkType;
import org.kalypso.gaja3d.service.stubs.GridX;
import org.kalypso.gaja3d.service.stubs.GridY;
import org.kalypso.gaja3d.service.stubs.Identifier;
import org.kalypso.gaja3d.service.stubs.MaxArea;
import org.kalypso.gaja3d.service.stubs.MinAngle;
import org.kalypso.gaja3d.service.stubs.ModelTin;
import org.kalypso.gaja3d.service.stubs.SmoothFilter;
import org.kalypso.gaja3d.service.stubs.SmoothFilterMethod;
import org.osgi.framework.Bundle;

/**
 * A Gaja3d resource encapsulates the three service operations as well as the
 * WPS inputs and outputs for them. It also dynamically insert inputs where
 * required by an operation.
 * 
 * The operations are performed by internally calling a WPS (using
 * WPSRequest.SERVICE_LOCAL as endpoint).
 * 
 * Gaja3dResource is a persistent resource that saves its resource properties
 * bean to disk everytime a property is set.
 * 
 * @author kurzbach
 * 
 */
public class Gaja3dResource extends PersistentReflectionResource implements
		SecureResource, ResourceIdentifier, ResourceProperties,
		ResourceLifetime, TopicListAccessor, RemoveCallback {

	/* UUID generator to generate unique resource key */
	private static final UUIDGen UUID_GEN = UUIDGenFactory.getUUIDGen();

	/* My logger */
	private static final Log LOGGER = LogFactory.getLog(Gaja3dResource.class
			.getName());

	/* Notification */
	private TopicList m_topicList;

	/* Resource properties */
	private ResourcePropertySet m_resourceProperties;

	/* Security config (from file) */
	private ResourceSecurityConfig m_securityConfig;

	/* Lifetime */
	// private Calendar m_terminationTime;
	/* Strategy for grid creation */
	private final CreateGridStrategy m_createGridStrategy;

	/* Strategy for breakline detection */
	private final DetectBreaklinesStrategy m_detectBreaklinesStrategy;

	/* Strategy for tin creation */
	private final CreateTinStrategy m_createTinStrategy;

	/* Synchronize access to this resource */
	private final Semaphore m_lock = new Semaphore(1);

	/* If true the resource has been removed. */
	private boolean m_destroyed = false;

	/**
	 * Creates a new Gaja3d resource
	 */
	public Gaja3dResource() {
		m_createGridStrategy = new WPSCreateGridStrategy();
		m_detectBreaklinesStrategy = new WPSDetectBreaklinesStrategy();
		m_createTinStrategy = new WPSCreateTinStrategy();
	}

	/* Needed by PersistentReflectionResource */
	@SuppressWarnings("unchecked")
	protected Class getResourceBeanClass() {
		return Gaja3DResourceProperties.class;
	}

	/** Initializes RPs and returns a unique identifier for this resource */
	public Object initialize() throws ConfigException {
		final Gaja3DResourceProperties resourceBean = new Gaja3DResourceProperties();
		final String key = UUID_GEN.nextUUID();

		try {
			super.initialize(resourceBean, Gaja3dQNames.RESOURCE_PROPERTIES,
					key);
		} catch (final ResourceException e) {
			throw new ConfigException(e);
		}

		m_resourceProperties = super.getResourcePropertySet();

		// initialize security descriptor
		try {
			final Bundle gaja3dBundle = Platform
					.getBundle("org.kalypso.gaja3d.service");
			final Path pathToSecDesc = new Path(
					"service/security-config-resource.xml");
			final URL secDescBundleURL = FileLocator.find(gaja3dBundle,
					pathToSecDesc, null);
			final URL secDescFileURL = FileLocator.toFileURL(secDescBundleURL);
			m_securityConfig = new ResourceSecurityConfig(secDescFileURL
					.getFile());
			m_securityConfig.init();
		} catch (final Exception e) {
			throw new ConfigException(
					"Could not load resource security descriptor.", e);
		}
		return key;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.globus.wsrf.impl.ReflectionResource#createNewResourceProperty(org
	 * .globus.wsrf.ResourcePropertyMetaData, java.lang.Object)
	 */
	protected ResourceProperty createNewResourceProperty(
			final ResourcePropertyMetaData metaData, final Object resourceBean)
			throws Exception {
		final ResourceProperty property = super.createNewResourceProperty(
				metaData, resourceBean);

		// lazily create topic list
		if (m_topicList == null)
			m_topicList = new SimpleTopicList(this);

		final ResourcePropertyTopic topic = new ResourcePropertyTopic(property);
		topic.setSendOldValue(true);
		m_topicList.addTopic(topic);
		return topic;
	}

	/* accessors for resource properties */
	public Gaja3DResourcePropertyLinkType getBoundary() {
		return (Gaja3DResourcePropertyLinkType) getRPinternal(Gaja3dQNames.RP_BOUNDARY);
	}

	public void setBoundary(final Gaja3DResourcePropertyLinkType boundary) {
		setDemPoints(null);
		final QName qName = Gaja3dQNames.RP_BOUNDARY;
		setRPinternal(boundary, qName);
	}

	public Gaja3DResourcePropertyLinkType getDemPoints() {
		return (Gaja3DResourcePropertyLinkType) getRPinternal(Gaja3dQNames.RP_DEM_POINTS);
	}

	public void setDemPoints(final Gaja3DResourcePropertyLinkType demPoints) {
		setDemGrid(null);
		final QName qName = Gaja3dQNames.RP_DEM_POINTS;
		setRPinternal(demPoints, qName);
	}

	public DemGrid getDemGrid() {
		return (DemGrid) getRPinternal(Gaja3dQNames.RP_DEM_GRID);
	}

	public void setDemGrid(final DemGrid demGrid) {
		setBreaklines(null);
		final QName qName = Gaja3dQNames.RP_DEM_GRID;
		setRPinternal(demGrid, qName);
	}

	public Breaklines getBreaklines() {
		return (Breaklines) getRPinternal(Gaja3dQNames.RP_BREAKLINES);
	}

	public void setBreaklines(final Breaklines breaklines) {
		setModelTin(null);
		final QName qName = Gaja3dQNames.RP_BREAKLINES;
		setRPinternal(breaklines, qName);
	}

	public ModelTin getModelTin() {
		return (ModelTin) getRPinternal(Gaja3dQNames.RP_MODEL_TIN);
	}

	public void setModelTin(final ModelTin modelTin) {
		final QName qName = Gaja3dQNames.RP_MODEL_TIN;
		setRPinternal(modelTin, qName);
	}

	public GridX getGridX() {
		return (GridX) getRPinternal(Gaja3dQNames.RP_GRID_X);
	}

	public void setGridX(final GridX gridX) {
		final QName qName = Gaja3dQNames.RP_GRID_X;
		setRPinternal(gridX, qName);
	}

	public GridY getGridY() {
		return (GridY) getRPinternal(Gaja3dQNames.RP_GRID_Y);
	}

	public void setGridY(final GridY gridY) {
		final QName qName = Gaja3dQNames.RP_GRID_Y;
		setRPinternal(gridY, qName);
	}

	public MinAngle getMinAngle() {
		return (MinAngle) getRPinternal(Gaja3dQNames.RP_MIN_ANGLE);
	}

	public void setMinAngle(final MinAngle minAngle) {
		final QName qName = Gaja3dQNames.RP_MIN_ANGLE;
		setRPinternal(minAngle, qName);
	}

	public MaxArea getMaxArea() {
		return (MaxArea) getRPinternal(Gaja3dQNames.RP_MAX_AREA);
	}

	public void setMaxArea(final MaxArea maxArea) {
		final QName qName = Gaja3dQNames.RP_MAX_AREA;
		setRPinternal(maxArea, qName);
	}

	public EdgeFilter getEdgeFilter() {
		return (EdgeFilter) getRPinternal(Gaja3dQNames.RP_EDGE_FILTER);
	}

	public void setEdgeFilter(final EdgeFilter edgeFilter) {
		final QName qName = Gaja3dQNames.RP_EDGE_FILTER;
		setRPinternal(edgeFilter, qName);
	}

	public SmoothFilter getSmoothFilter() {
		return (SmoothFilter) getRPinternal(Gaja3dQNames.RP_SMOOTH_FILTER);
	}

	public void setSmoothFilter(final SmoothFilter smoothFilter) {
		final QName qName = Gaja3dQNames.RP_SMOOTH_FILTER;
		setRPinternal(smoothFilter, qName);
	}

	public FeatureDetector getFeatureDetector() {
		return (FeatureDetector) getRPinternal(Gaja3dQNames.RP_FEATURE_DETECTOR);
	}

	public void setFeatureDetector(final FeatureDetector featureDetector) {
		final QName qName = Gaja3dQNames.RP_FEATURE_DETECTOR;
		setRPinternal(featureDetector, qName);
	}

	public DistanceTolerance getDistanceTolerance() {
		return (DistanceTolerance) getRPinternal(Gaja3dQNames.RP_DISTANCE_TOLERANCE);
	}

	public void setDistanceTolerance(final DistanceTolerance distanceTolerance) {
		final QName qName = Gaja3dQNames.RP_DISTANCE_TOLERANCE;
		setRPinternal(distanceTolerance, qName);
	}

	public EndpointReference getGramEndpointReference() {
		return (EndpointReference) getRPinternal(Gaja3dQNames.RP_GRAM_ENDPOINT_REFERENCE);
	}

	public void setGramEndpointReference(
			final EndpointReference endpointReference) {
		final QName qName = Gaja3dQNames.RP_GRAM_ENDPOINT_REFERENCE;
		setRPinternal(endpointReference, qName);
	}

	/* Implementations of 3D Terrain Discretization Service methods */

	public void createGrid() throws RemoteException {
		try {
			m_lock.acquire();
		} catch (final InterruptedException e) {
			LOGGER.error("Thread was interrupted before tin creation.");
		}

		final DemGrid demGrid = new DemGrid();
		try {
			// required
			final Gaja3DResourcePropertyLinkType boundary = getBoundary();
			if (boundary == null) {
				throw new AxisFault("A boundary is required for grid creation.");
			}
			final URI boundaryURI = boundary.getHref();
			final java.net.URI boundaryLocation = new java.net.URI(boundaryURI
					.toString());

			// required
			final Gaja3DResourcePropertyLinkType demPoints = getDemPoints();
			if (demPoints == null) {
				throw new AxisFault(
						"A point cloud is required for grid creation.");
			}
			final URI demPointsURI = demPoints.getHref();
			final java.net.URI demPointsLocation = new java.net.URI(
					demPointsURI.toString());

			// required
			final GridX gridX = getGridX();
			if (gridX == null) {
				throw new AxisFault(
						"Grid resolution (X) is required for grid creation.");
			}
			final double dx = gridX.getDx();

			// required
			final GridY gridY = getGridY();
			if (gridY == null) {
				throw new AxisFault(
						"Grid resolution (Y) is required for grid creation.");
			}
			final double dy = gridY.getDy();

			// do the actual work
			final java.net.URI demGridLocation = m_createGridStrategy
					.createGrid(boundaryLocation, demPointsLocation, dx, dy);

			demGrid.setIdentifier(CodeTypeUtil.fillCodeType(new Identifier(),
					Gaja3dQNames.RP_DEM_GRID));
			demGrid.setHref(new URI(demGridLocation.toString()));
		} catch (final MalformedURIException e) {
			throw AxisFault.makeFault(e);
		} catch (final URISyntaxException e) {
			throw AxisFault.makeFault(e);
		} finally {
			m_lock.release();
			setDemGrid(demGrid);
		}
	}

	public void detectBreaklines() throws RemoteException {
		try {
			m_lock.acquire();
		} catch (final InterruptedException e) {
			LOGGER.error("Thread was interrupted before tin creation.");
		}

		final Breaklines breaklines = new Breaklines();
		try {
			// optional
			final EdgeFilter edgeFilter = getEdgeFilter();
			if (edgeFilter != null) {
				final EdgeFilterMethod method = edgeFilter.getMethod();
				final String edgeMethod = method.getValue();
				m_detectBreaklinesStrategy.setEdgeMethod(edgeMethod);
			}

			// optional
			final SmoothFilter smoothFilter = getSmoothFilter();
			if (smoothFilter != null) {
				final SmoothFilterMethod method = smoothFilter.getMethod();
				final String smoothMethod = method.getValue();
				final int smooth = smoothFilter.getSmooth();
				m_detectBreaklinesStrategy.setSmoothMethod(smoothMethod);
				m_detectBreaklinesStrategy.setSmooth(smooth);
			}

			// optional
			final FeatureDetector featureDetector = getFeatureDetector();
			if (featureDetector != null) {
				final FeatureDetectorMethod method = featureDetector
						.getMethod();
				final String featureMethod = method.getValue();
				final double lowThresh = featureDetector.getLowThresh();
				final double highThresh = featureDetector.getHighThresh();
				m_detectBreaklinesStrategy.setFeatureMethod(featureMethod);
				m_detectBreaklinesStrategy.setLowThresh(lowThresh);
				m_detectBreaklinesStrategy.setHighThresh(highThresh);
			}

			// optional
			final DistanceTolerance distanceToleranceParameter = getDistanceTolerance();
			if (distanceToleranceParameter != null) {
				final double distanceTolerance = distanceToleranceParameter
						.getTolerance();
				m_detectBreaklinesStrategy
						.setDistanceTolerance(distanceTolerance);
			}

			// required
			final Gaja3DResourcePropertyLinkType boundary = getBoundary();
			if (boundary == null) {
				throw new AxisFault(
						"A boundary is required for breakline detection.");
			}
			final URI boundaryURI = boundary.getHref();
			final java.net.URI boundaryLocation = new java.net.URI(boundaryURI
					.toString());

			// required
			final Gaja3DResourcePropertyLinkType demGrid = getDemGrid();
			if (demGrid == null) {
				throw new AxisFault(
						"A grid is required for breakline detection.");
			}
			final URI demGridURI = demGrid.getHref();
			final java.net.URI demGridLocation = new java.net.URI(demGridURI
					.toString());

			// do the actual work
			final java.net.URI breaklinesLocation = m_detectBreaklinesStrategy
					.detectBreaklines(boundaryLocation, demGridLocation);

			breaklines.setIdentifier(CodeTypeUtil.fillCodeType(
					new Identifier(), Gaja3dQNames.RP_BREAKLINES));
			breaklines.setHref(new URI(breaklinesLocation.toString()));
		} catch (final MalformedURIException e) {
			throw AxisFault.makeFault(e);
		} catch (final IOException e) {
			throw AxisFault.makeFault(e);
		} catch (final URISyntaxException e) {
			throw AxisFault.makeFault(e);
		} finally {
			m_lock.release();
			setBreaklines(breaklines);
		}
	}

	public void createTin() throws RemoteException {
		try {
			m_lock.acquire();
		} catch (final InterruptedException e) {
			LOGGER.error("Thread was interrupted before tin creation.");
		}

		final ModelTin modelTin = new ModelTin();
		try {
			// optional
			final MaxArea maxArea = getMaxArea();
			if (maxArea != null) {
				final double area = maxArea.getArea();
				m_createTinStrategy.setMaxArea(area);
			}

			// optional
			final MinAngle minAngle = getMinAngle();
			if (minAngle != null) {
				final double angle = minAngle.getAngle();
				m_createTinStrategy.setMinAngle(angle);
			}

			// optional
			final Gaja3DResourcePropertyLinkType demGrid = getDemGrid();
			if (demGrid != null) {
				final URI demGridURI = demGrid.getHref();
				final java.net.URI demGridLocation = new java.net.URI(
						demGridURI.toString());
				m_createTinStrategy.setDemGridLocation(demGridLocation);
			}

			// required
			final Gaja3DResourcePropertyLinkType boundary = getBoundary();
			if (boundary == null) {
				throw new AxisFault(
						"At least one boundary is required for tin creation.");
			}
			final URI boundaryURI = boundary.getHref();
			final java.net.URI boundaryLocation = new java.net.URI(boundaryURI
					.toString());

			// optional
			final Gaja3DResourcePropertyLinkType breaklines = getBreaklines();
			if (breaklines != null) {
				final URI breaklinesURI = breaklines.getHref();
				final java.net.URI breaklinesLocation = new java.net.URI(
						breaklinesURI.toString());
				m_createTinStrategy.setBreaklinesLocation(breaklinesLocation);
			}

			final java.net.URI modelTinLocation = m_createTinStrategy
					.createTin(boundaryLocation);

			modelTin.setIdentifier(CodeTypeUtil.fillCodeType(new Identifier(),
					Gaja3dQNames.RP_MODEL_TIN));
			modelTin.setHref(new URI(modelTinLocation.toString()));
		} catch (final MalformedURIException e) {
			throw AxisFault.makeFault(e);
		} catch (final URISyntaxException e) {
			throw AxisFault.makeFault(e);
		} finally {
			m_lock.release();
			setModelTin(modelTin);
		}
	}

	/* Required by interface TopicListAccessor */
	public TopicList getTopicList() {
		return m_topicList;
	}

	/* Required by interface SecureResource */
	public ResourceSecurityDescriptor getSecurityDescriptor() {
		return m_securityConfig.getSecurityDescriptor();
	}

	/* Required by interface RemoveCallback */
	public void remove() throws ResourceException {
		try {
			m_lock.acquire();
		} catch (final InterruptedException e) {
			LOGGER.error("Thread was interrupted before removing resource.", e);
		}

		try {
			super.remove();
		} finally {
			m_lock.release();
			m_destroyed = true;
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.globus.wsrf.impl.PersistentReflectionResource#store()
	 */
	public synchronized void store() throws ResourceException {
		// check if this resource has been destroyed
		if (!m_destroyed) {
			super.store();
		} else {
			LOGGER
					.warn("Tried to store resource that has already been destroyed.");
		}
	}

	/* Private methods */
	private Object getRPinternal(final QName qName) {
		final ResourceProperty property = m_resourceProperties.get(qName);
		if ((property == null) || property.isEmpty()) {
			return null;
		}
		final Object object = property.get(0);
		if (object == null) {
			return null;
		}
		return object;
	}

	private void setRPinternal(final Object newValue, final QName qName) {
		try {
			m_lock.acquire();
		} catch (final InterruptedException e) {
			LOGGER.error(
					"Thread was interrupted before setting resource property "
							+ qName, e);
		}

		try {
			final ResourceProperty property = m_resourceProperties.get(qName);
			final Object oldValue = property.get(0);
			if ((oldValue == null && newValue == null)
					|| (oldValue != null && newValue != null && oldValue
							.equals(newValue))) {
				// equal
				return;
			}
			setDirty(true);
			property.set(0, newValue);
			store();
		} catch (final ResourceException e) {
			LOGGER.error("Could not store resource.", e);
		} finally {
			m_lock.release();
		}
	}
}