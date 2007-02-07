package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import org.kalypsodeegree.model.feature.binding.IFeatureWrapper;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree.model.geometry.GM_Surface;

/**
 * The interface to be implemented by classes representing
 * a simBase:RoughnessPolygon element
 * 
 * @author Dejan Antanaskovic, Patrice Congo
 *
 */
public interface IRoughnessPolygon extends IFeatureWrapper
{
	/**
	 * Returns the rougthness ID for this polynom
	 * 
	 * @return the id of the rougthness associated to the polygon
	 */
	public String getRoughnessID();
	
	/**
	 * 
	 * @param id the new ID for the rougthness associated to the polygon
	 * 
	 * @throws IllegalArgumentException if id is null or empty
	 */
	public void setRoughnessID(String id)
				throws IllegalArgumentException;
	
	/**
	 * Returns the rougthness polygon
	 * @return the polygom 
	 */
	public GM_Polygon getPolygon();
	
	/**
	 * Sets the geometry of the roughness polygon
	 * 
	 * @param polygon the polygon to set
	 * 
	 */
	public void setPolygon(GM_Polygon polygon);
	
	/**
	 * Sets the geometry of the roughness polygon
	 * 
	 * @param polygon the polygon to set
	 * 
	 */
	public void setSurface(GM_Surface polygon);
}
