package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.kalypsosimulationmodel.core.FeatureWrapperCollection;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Polygon;

/**
 * The default implementation of {@link IRoughnessPolygonCollection} 
 * based on {@link FeatureWrapperCollection}
 * 
 * @author Patrice Congo
 */
public class RoughnessPolygonCollection 
			extends FeatureWrapperCollection<IRoughnessPolygon>
			implements IRoughnessPolygonCollection
{
	
	public RoughnessPolygonCollection(Feature featureCol, Class<IRoughnessPolygon> fwClass, QName featureMemberProp)
	{
		super(featureCol, fwClass, featureMemberProp);
	}

	public RoughnessPolygonCollection(Feature parentFeature, QName childQName, QName featureMemberProp, Class<IRoughnessPolygon> fwClass) throws IllegalArgumentException
	{
		super(parentFeature, childQName, featureMemberProp, fwClass);
	}

	public List<IRoughnessPolygon[]> checksOverlapping()
	{
		return null;
	}

	public IRoughnessEstimateSpec getRoughnessEstimateSpec(GM_Polygon polygon)
	{
		return null;
	}

	public List<IRoughnessPolygon> getRoughnessPolygons()
	{
		return null;
	}

	public IRoughnessPolygon[] select(GM_Point location)
	{
		return null;
	}

	

}
