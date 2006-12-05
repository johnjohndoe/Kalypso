/**
 * 
 */
package org.kalypso.kalypsomodel1d2d.schema.binding;

import javax.xml.namespace.QName;

import org.kalypso.commons.xml.NS;
import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.feature.binding.AbstractFeatureBinder;

/**
 * @author Gernot Belger
 */
public class FE1D2DNode extends AbstractFeatureBinder {

	public final static QName QNAME_FE1D2DNode = new QName(
			UrlCatalog1D2D.MODEL_1D2D_NS, "FE1D2DNode");

	public final static QName QNAME_PROP_POINT = new QName(
			NS.GML3, "pointProperty");

	public FE1D2DNode(final Feature featureToBind) {
		super(featureToBind, QNAME_FE1D2DNode);
	}

	public GM_Point getPoint()
	{
		return (GM_Point) getFeature().getProperty(QNAME_PROP_POINT);
	}
}
