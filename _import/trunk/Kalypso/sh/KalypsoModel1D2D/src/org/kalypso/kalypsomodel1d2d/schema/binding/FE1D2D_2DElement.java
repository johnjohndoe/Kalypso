/**
 * 
 */
package org.kalypso.kalypsomodel1d2d.schema.binding;

import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfaceInterpolation;
import org.kalypsodeegree_impl.model.feature.binding.AbstractFeatureBinder;
import org.kalypsodeegree_impl.model.geometry.GM_SurfaceInterpolation_Impl;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author Gernot Belger
 */
public class FE1D2D_2DElement extends AbstractFeatureBinder {

	public final static QName QNAME_FE1D2D_2DElement = new QName(
			UrlCatalog1D2D.MODEL_1D2D_NS, "FE1D2D_2DElement");

	public final static QName QNAME_PROP_DIRECTEDEDGE = new QName(
			UrlCatalog1D2D.MODEL_1D2D_NS, "fe1d2dDirectedEdge");

	public FE1D2D_2DElement(final Feature featureToBind) {
		super(featureToBind, QNAME_FE1D2D_2DElement);
	}

	/**
	 * Returns the (dereferenced) nodes of this egde. Elements of the array may
	 * be null.
	 */
	public FE1D2DEdge[] getEdges() {
		final Feature feature = getFeature();
		final GMLWorkspace workspace = feature.getWorkspace();
		final List edgeList = (List) feature
				.getProperty(QNAME_PROP_DIRECTEDEDGE);

		final FE1D2DEdge[] edges = new FE1D2DEdge[edgeList.size()];
		for (int i = 0; i < edges.length; i++) {
			/*
			 * Accessing the list via index is ok here, because we should never
			 * have edges with more than 2 nodes.
			 */
			final String ref = (String) edgeList.get(i);
			if (ref == null)
				edges[i] = null;
			else
				edges[i] = new FE1D2DEdge(workspace.getFeature(ref));
		}

		return edges;
	}

	/* static helper functions */
	public GM_Surface recalculateElementGeometry() throws GM_Exception {

		final FE1D2DEdge[] edges = getEdges();

		if (edges.length < 3)
			return null;

		final FE1D2DNode[] nodes = new FE1D2DNode[edges.length + 1];
		for (int i = 0; i < edges.length; i++) {
			final FE1D2DEdge edge = edges[i];
			final FE1D2DNode[] edgeNodes = edge.getNodes();
			nodes[i] = edgeNodes[0];
		}

		nodes[edges.length] = edges[edges.length - 1].getNodes()[1];

		/* Positions from nodes */
		final GM_Position[] poses = new GM_Position[nodes.length];

		if (nodes.length < 2)
			return null;

		// REMARK: we assume here, that all nodes live in the same coordinate
		// system.
		final CS_CoordinateSystem crs = nodes[0].getPoint()
				.getCoordinateSystem();

		for (int i = 0; i < poses.length; i++) {
			final GM_Point point = nodes[i].getPoint();
			final GM_Position position = point.getPosition();
			poses[i] = GeometryFactory.createGM_Position(position.getX(),
					position.getY());
		}

		return GeometryFactory
				.createGM_Surface(poses, null,
						new GM_SurfaceInterpolation_Impl(
								GM_SurfaceInterpolation.PLANAR), crs);
	}

}
