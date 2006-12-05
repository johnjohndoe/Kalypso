/**
 * 
 */
package org.kalypso.kalypsomodel1d2d.schema.binding;

import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.feature.binding.AbstractFeatureBinder;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author Gernot Belger
 */
public class FE1D2DEdge extends AbstractFeatureBinder {

	public final static QName QNAME_FE1D2DEdge = new QName(
			UrlCatalog1D2D.MODEL_1D2D_NS, "FE1D2DEdge");

	public final static QName QNAME_PROP_DIRECTEDNODE = new QName(
			UrlCatalog1D2D.MODEL_1D2D_NS, "fe1d2dDirectedNode");

	public FE1D2DEdge(final Feature featureToBind) {
		super(featureToBind, QNAME_FE1D2DEdge);
	}

	/**
	 * Returns the (dereferenced) nodes of this egde. Elements of the array may
	 * be null.
	 */
	public FE1D2DNode[] getNodes() {
		final Feature feature = getFeature();
		final GMLWorkspace workspace = feature.getWorkspace();
		final List nodeList = (List) feature
				.getProperty(QNAME_PROP_DIRECTEDNODE);

		final FE1D2DNode[] nodes = new FE1D2DNode[nodeList.size()];
		for (int i = 0; i < nodes.length; i++) {
			/*
			 * Accessing the list via index is ok here, because we should never
			 * have edges with more than 2 nodes.
			 */
			final String ref = (String) nodeList.get(i);
			if (ref == null)
				nodes[i] = null;
			else
				nodes[i] = new FE1D2DNode(workspace.getFeature(ref));
		}

		return nodes;
	}

	/* static helper functions */
	public GM_Curve recalculateEgdeGeometry() throws GM_Exception {

		final FE1D2DNode[] nodes = getNodes();
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

		return GeometryFactory.createGM_Curve(poses, crs);
	}

}
