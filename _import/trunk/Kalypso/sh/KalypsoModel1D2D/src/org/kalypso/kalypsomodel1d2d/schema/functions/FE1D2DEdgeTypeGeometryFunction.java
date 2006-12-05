/**
 * 
 */
package org.kalypso.kalypsomodel1d2d.schema.functions;

import java.util.Map;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.FE1D2DEdge;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction;

/**
 * Creates the edge geometry from the two referenced nodes.
 * 
 * @author Gernot Belger
 */
public class FE1D2DEdgeTypeGeometryFunction extends FeaturePropertyFunction {

	/**
	 * @see org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction#init(java.util.Map)
	 */
	@Override
	public void init(final Map<String, String> properties) {
		// nothing to do
	}

	/**
	 * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#getValue(org.kalypsodeegree.model.feature.Feature,
	 *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
	 */
	public Object getValue(final Feature feature, final IPropertyType pt,
			final Object currentValue) {

		final FE1D2DEdge edge = new FE1D2DEdge(feature);
		try {
			return edge.recalculateEgdeGeometry();
		} catch (GM_Exception e) {
			final IStatus status = StatusUtilities.statusFromThrowable(e);
			KalypsoModel1D2DPlugin.getDefault().getLog().log(status);
			return null;
		}
	}

	/**
	 * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#setValue(org.kalypsodeegree.model.feature.Feature,
	 *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
	 */
	public Object setValue(Feature feature, IPropertyType pt, Object valueToSet) {
		// TODO: move the corresponding node
		return null;
	}

}
