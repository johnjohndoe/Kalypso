package org.kalypso.gaja3d.service.internal.strategy;

import java.io.InputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.List;

import javax.xml.namespace.QName;

import net.opengeospatial.wps.ComplexValueType;

import org.apache.axis.AxisFault;
import org.apache.commons.io.IOUtils;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.service.wps.utils.simulation.WPSSimulationDataProvider;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

public class AbstractWPSStrategy {

	protected static final String BOUNDARIES_GML_TEMPLATE = "Boundaries.gml";

	public AbstractWPSStrategy() {
		super();
	}

	@SuppressWarnings("unchecked")
	protected URI[] parseLocations(final ComplexValueType complexValue,
			final QName propertyName) throws SimulationException,
			URISyntaxException {
		final GMLWorkspace demGridsWorkspace = (GMLWorkspace) WPSSimulationDataProvider
				.parseComplexValue(complexValue);
		final Feature demGridsFeature = demGridsWorkspace.getRootFeature();
		final List<String> demGrids = (List<String>) demGridsFeature
				.getProperty(propertyName);
		final URI[] demGridLocations = new URI[demGrids.size()];
		for (int i = 0; i < demGridLocations.length; i++) {
			final String demGridLocation = demGrids.get(i);
			demGridLocations[i] = new URI(demGridLocation);
		}
		return demGridLocations;
	}

	@SuppressWarnings("unchecked")
	protected GMLWorkspace buildGMLWorkspace(final URI[] boundaryLocations,
			final String templateName, final QName propertyName)
			throws AxisFault {
		// try to read boundaries
		GMLWorkspace gmlWorkspace = null;
		InputStream inputStream = null;
		try {
			final URL templateFile = getClass().getResource(templateName);
			inputStream = templateFile.openStream();
			gmlWorkspace = GmlSerializer.createGMLWorkspace(inputStream, null,
					null);
			final Feature boundariesFeature = gmlWorkspace.getRootFeature();
			final List<String> boundaries = (List) boundariesFeature
					.getProperty(propertyName);
			for (final URI uri : boundaryLocations) {
				boundaries.add(uri.toString());
			}
		} catch (final Exception e) {
			throw AxisFault.makeFault(e);
		} finally {
			IOUtils.closeQuietly(inputStream);
		}
		return gmlWorkspace;
	}

}