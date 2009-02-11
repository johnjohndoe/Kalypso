package org.kalypso.gaja3d.service.impl;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.rmi.RemoteException;
import java.util.Calendar;

import javax.xml.namespace.QName;

import org.apache.axis.AxisFault;
import org.apache.axis.message.MessageElement;
import org.apache.axis.types.URI;
import org.apache.axis.types.URI.MalformedURIException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.globus.wsrf.ResourceIdentifier;
import org.globus.wsrf.ResourceLifetime;
import org.globus.wsrf.ResourceProperties;
import org.globus.wsrf.ResourceProperty;
import org.globus.wsrf.ResourcePropertySet;
import org.globus.wsrf.TopicList;
import org.globus.wsrf.TopicListAccessor;
import org.globus.wsrf.config.ConfigException;
import org.globus.wsrf.impl.ResourcePropertyTopic;
import org.globus.wsrf.impl.SimpleResourceProperty;
import org.globus.wsrf.impl.SimpleResourcePropertySet;
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
import org.kalypso.gaja3d.service.stubs.Gaja3DResourcePropertyLinkType;
import org.kalypso.gaja3d.service.stubs.Gaja3DResourcePropertyType;
import org.kalypso.gaja3d.service.stubs.GridX;
import org.kalypso.gaja3d.service.stubs.GridY;
import org.kalypso.gaja3d.service.stubs.Identifier;
import org.kalypso.gaja3d.service.stubs.MaxArea;
import org.kalypso.gaja3d.service.stubs.MinAngle;
import org.kalypso.gaja3d.service.stubs.ModelTin;
import org.kalypso.gaja3d.service.stubs.SmoothFilter;
import org.kalypso.gaja3d.service.stubs.SmoothFilterMethod;
import org.osgi.framework.Bundle;

public class Gaja3dResource implements SecureResource, ResourceIdentifier,
		ResourceProperties, ResourceLifetime, TopicListAccessor {

	/* Resource key. This uniquely identifies this resource. */
	private Object key;

	/* Notification */
	private TopicList topicList;

	/* Resource properties */
	private ResourcePropertySet gaja3dRPs;

	/* Lifetime */
	private Calendar terminationTime;

	/* Strategy for grid creation */
	private final CreateGridStrategy createGridStrategy;

	/* Strategy for breakline detection */
	private final DetectBreaklinesStrategy detectBreaklinesStrategy;

	/* Strategy for tin creation */
	private final CreateTinStrategy createTinStrategy;

	private ResourceSecurityConfig config;

	public Gaja3dResource() {
		createGridStrategy = new WPSCreateGridStrategy();
		detectBreaklinesStrategy = new WPSDetectBreaklinesStrategy();
		createTinStrategy = new WPSCreateTinStrategy();
	}

	/* Initializes RPs and returns a unique identifier for this resource */
	public Object initialize() throws ConfigException {
		this.key = new Integer(hashCode());
		this.gaja3dRPs = new SimpleResourcePropertySet(
				Gaja3dQNames.RESOURCE_PROPERTIES);
		this.topicList = new SimpleTopicList(this);
		for (final QName qName : Gaja3dQNames.ALL_RPS) {
			final SimpleResourceProperty property = new SimpleResourceProperty(
					qName);
			final ResourcePropertyTopic topic = new ResourcePropertyTopic(
					property);
			topic.setSendOldValue(true);
			this.gaja3dRPs.add(topic); // important: add topic, not RP
			this.topicList.addTopic(topic);
		}

		final Bundle gaja3dBundle = Platform
				.getBundle("org.kalypso.gaja3d.service");
		final URL secDesc = FileLocator.find(gaja3dBundle, new Path(
				"service/security-config-resource.xml"), null);
		URL fileURL;
		try {
			fileURL = FileLocator.toFileURL(secDesc);
			config = new ResourceSecurityConfig(fileURL.getFile());
			config.init();
		} catch (final IOException e) {
			throw new ConfigException(
					"Could not load resource security descriptor.", e);
		}
		return key;
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

	/* Implementations of 3D Terrain Discretization Service methods */

	public void createGrid() throws RemoteException {
		final Gaja3DResourcePropertyLinkType boundary = getBoundary();
		final URI boundaryURI = boundary.getHref();
		final Gaja3DResourcePropertyLinkType demPoints = getDemPoints();
		final URI demPointsURI = demPoints.getHref();
		final GridX gridX = getGridX();
		final double dx = gridX.getDx();
		final GridY gridY = getGridY();
		final double dy = gridY.getDy();
		try {
			final URL boundaryLocation = new URL(boundaryURI.toString());
			final URL demPointsLocation = new URL(demPointsURI.toString());
			final URL demGridLocation = createGridStrategy.createGrid(
					boundaryLocation, demPointsLocation, dx, dy);
			final DemGrid demGrid = new DemGrid();
			demGrid.setIdentifier(CodeTypeUtil.fillCodeType(new Identifier(),
					Gaja3dQNames.RP_DEM_GRID));
			demGrid.setHref(new URI(demGridLocation.toExternalForm()));
			setDemGrid(demGrid);
		} catch (final MalformedURIException e) {
			throw AxisFault.makeFault(e);
		} catch (final MalformedURLException e) {
			throw AxisFault.makeFault(e);
		}
	}

	public void detectBreaklines() throws RemoteException {
		final Gaja3DResourcePropertyLinkType boundary = getBoundary();
		final URI boundaryURI = boundary.getHref();
		final Gaja3DResourcePropertyLinkType demGrid = getDemGrid();
		final URI demGridURI = demGrid.getHref();

		final EdgeFilter edgeFilter = getEdgeFilter();
		if (edgeFilter != null) {
			final EdgeFilterMethod method = edgeFilter.getMethod();
			final String edgeMethod = method.getValue();
			detectBreaklinesStrategy.setEdgeMethod(edgeMethod);
		}

		final SmoothFilter smoothFilter = getSmoothFilter();
		if (smoothFilter != null) {
			final SmoothFilterMethod method = smoothFilter.getMethod();
			final String smoothMethod = method.getValue();
			final int smooth = smoothFilter.getSmooth();
			detectBreaklinesStrategy.setSmoothMethod(smoothMethod);
			detectBreaklinesStrategy.setSmooth(smooth);
		}

		final FeatureDetector featureDetector = getFeatureDetector();
		if (featureDetector != null) {
			final FeatureDetectorMethod method = featureDetector.getMethod();
			final String featureMethod = method.getValue();
			final double lowThresh = featureDetector.getLowThresh();
			final double highThresh = featureDetector.getHighThresh();
			detectBreaklinesStrategy.setFeatureMethod(featureMethod);
			detectBreaklinesStrategy.setLowThresh(lowThresh);
			detectBreaklinesStrategy.setHighThresh(highThresh);
		}

		final DistanceTolerance distanceToleranceParameter = getDistanceTolerance();
		if (distanceToleranceParameter != null) {
			final double distanceTolerance = distanceToleranceParameter
					.getTolerance();
			detectBreaklinesStrategy.setDistanceTolerance(distanceTolerance);
		}

		try {
			final URL boundaryLocation = new URL(boundaryURI.toString());
			final URL demGridLocation = new URL(demGridURI.toString());
			final URL breaklinesLocation = detectBreaklinesStrategy
					.detectBreaklines(boundaryLocation, demGridLocation);
			// final File shpTmp = FileUtilities.createNewTempDir("shptmp");
			// ZipUtilities.unzip(breaklinesLocation.openStream(), shpTmp);
			// final File[] files = shpTmp.listFiles((FileFilter)
			// FileFilterUtils
			// .suffixFileFilter(".shp"));
			// final File breaklinesGml = new File(shpTmp, "breaklines.gml");
			// if (files.length > 0) {
			// // process first file in list. this may change
			// final File file = files[0];
			// final String absolutePath = file.getAbsolutePath();
			// final String shapeBase = FileUtilities
			// .nameWithoutExtension(absolutePath);
			// final String crs = KalypsoDeegreePlugin.getDefault()
			// .getCoordinateSystem();
			// final GMLWorkspace shpWorkspace = ShapeSerializer.deserialize(
			// shapeBase, crs);
			// GmlSerializer.serializeWorkspace(breaklinesGml, shpWorkspace,
			// "UTF-8");
			// }
			final Breaklines breaklines = new Breaklines();
			breaklines.setIdentifier(CodeTypeUtil.fillCodeType(
					new Identifier(), Gaja3dQNames.RP_BREAKLINES));
			breaklines.setHref(new URI(breaklinesLocation.toExternalForm()));
			// breaklines.setHref(new
			// URI(breaklinesGml.toURI().toASCIIString()));
			setBreaklines(breaklines);
		} catch (final MalformedURIException e) {
			throw AxisFault.makeFault(e);
		} catch (final MalformedURLException e) {
			throw AxisFault.makeFault(e);
		} catch (final IOException e) {
			throw AxisFault.makeFault(e);
		}
		// catch (GmlSerializeException e) {
		// throw AxisFault.makeFault(e);
		// }
	}

	public void createTin() throws RemoteException {
		final Gaja3DResourcePropertyLinkType boundary = getBoundary();
		final URI boundaryURI = boundary.getHref();
		final Gaja3DResourcePropertyLinkType breaklines = getBreaklines();
		final URI breaklinesURI = breaklines.getHref();

		final MaxArea maxArea = getMaxArea();
		if (maxArea != null) {
			final double area = maxArea.getArea();
			createTinStrategy.setMaxArea(area);
		}

		final MinAngle minAngle = getMinAngle();
		if (minAngle != null) {
			final double angle = minAngle.getAngle();
			createTinStrategy.setMinAngle(angle);
		}

		try {
			final URL boundaryLocation = new URL(boundaryURI.toString());
			final URL breaklinesLocation = new URL(breaklinesURI.toString());
			final URL modelTinLocation = createTinStrategy.createTin(
					boundaryLocation, breaklinesLocation);
			final ModelTin modelTin = new ModelTin();
			modelTin.setIdentifier(CodeTypeUtil.fillCodeType(new Identifier(),
					Gaja3dQNames.RP_MODEL_TIN));
			modelTin.setHref(new URI(modelTinLocation.toExternalForm()));
			setModelTin(modelTin);
		} catch (final MalformedURIException e) {
			throw AxisFault.makeFault(e);
		} catch (final MalformedURLException e) {
			throw AxisFault.makeFault(e);
		}
	}

	/* Required by interface ResourceProperties */
	public ResourcePropertySet getResourcePropertySet() {
		return this.gaja3dRPs;
	}

	/* Required by interface ResourceIdentifier */
	public Object getID() {
		return this.key;
	}

	/* Required by interface ResourceLifetime */
	public Calendar getCurrentTime() {
		return Calendar.getInstance();
	}

	public Calendar getTerminationTime() {
		return this.terminationTime;
	}

	public void setTerminationTime(final Calendar terminationTime) {
		this.terminationTime = terminationTime;
	}

	/* Required by interface TopicListAccessor */
	public TopicList getTopicList() {
		return topicList;
	}

	/* Private methods */
	private Gaja3DResourcePropertyType getRPinternal(final QName qName) {
		final ResourceProperty property = gaja3dRPs.get(qName);
		if (property == null || property.isEmpty())
			return null;
		Object object = property.get(0);
		if (object == null)
			return null;
		if (object instanceof MessageElement) {
			try {
				object = ((MessageElement) object)
						.getObjectValue(Gaja3DResourcePropertyLinkType.class);
			} catch (Exception e) {
				try {
					object = ((MessageElement) object)
							.getObjectValue(Gaja3DResourcePropertyType.class);
				} catch (Exception e1) {
					e1.printStackTrace();
					return null;
				}
			}
		}
		return (Gaja3DResourcePropertyType) object;
	}

	private <T extends Gaja3DResourcePropertyType> void setRPinternal(
			T newValue, final QName qName) {
		final ResourceProperty property = this.gaja3dRPs.get(qName);
		property.clear();
		if (newValue == null)
			return;

		final Identifier valueIdentifier = newValue.getIdentifier();
		final URI namespaceURI = valueIdentifier.getCodeSpace();
		final boolean qNamesEqual = namespaceURI.toString().equals(
				qName.getNamespaceURI());
		final String localPart = valueIdentifier.get_value();
		final boolean localPartEqual = localPart.equals(qName.getLocalPart());
		if (!(localPartEqual && qNamesEqual)) {
			throw new IllegalArgumentException(
					String
							.format(
									"Trying to set resource property %s with a value with the identifier %s.",
									qName, valueIdentifier.toString()));
		}
		property.add(newValue);
	}

	@Override
	public ResourceSecurityDescriptor getSecurityDescriptor() {
		return config.getSecurityDescriptor();
	}
}