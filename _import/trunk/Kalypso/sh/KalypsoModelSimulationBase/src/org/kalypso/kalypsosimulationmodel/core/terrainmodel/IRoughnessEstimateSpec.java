package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import java.util.List;
import java.util.Map;

import org.kalypsodeegree.model.geometry.GM_Envelope;

/**
 * 
 * @author Dejan Antanaskovic, Patrice Congo
 *
 */
public interface IRoughnessEstimateSpec 
{
	/**
	 * returns the area ratio between the element geometry area
	 * and the brid cell area
	 *  
	 * @return the ratio between element geometry area and the
	 * 		the  
	 */
	public double getAreaRatio();
	
	public void setRatio(double ratio);
	
	/**
	 * Gets the rougness distribution within the given element geometry
	 * 
	 * @return a map representing the roughness distribution within the 
	 *  		element geometry: the name of the roughtness as key
	 *  		and the 
	 *  
	 * 
	 */
	public Map<String , Double> getHistogramm();	
	
	public String[] possibleRoughnesses();
	
	public GM_Envelope[] getCells();
	
	
	public String mostSpreadRoughness();
	
	public List<IRoughnessPolygon> getContributingRoughnessPolygons();
	
	
}
