package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import java.util.List;

import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Polygon;



/**
 * The interface to be implmented by classes which instances 
 * represents a simBase:RougthnessCollectionElement
 * 
 * @author Dejan Antanaskovic, Patrice Congo
 */
public interface IRoughnessPolygonCollection extends IFeatureWrapperCollection<IRoughnessPolygon>
{
	/**
	 * Select the rougthness polygon containing this point. since the point
	 * may be on the border there can be several polygon containing the
	 * specified point.
	 * 
	 * @param location the location where to find the roughness polygon
	 * 
	 * @return the rougness polygons which at the given location
	 *  
	 */
	public IRoughnessPolygon[] select(GM_Point location);
	
	/**
	 * Get all rougthness polygons in this collection 
	 * @return
	 */
	public List<IRoughnessPolygon> getRoughnessPolygons();
	
	
	/**
	 * Build the estimation specification of the 
	 * @param polygon
	 * @return
	 */
	public IRoughnessEstimateSpec getRoughnessEstimateSpec(
												GM_Polygon polygon);
	
	/**
	 * Checks no overlapping roughness polygons
	 * @return
	 */
	public List<IRoughnessPolygon[]> checksOverlapping();
	
}
