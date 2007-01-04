/**
 * 
 */
package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Polygon;

/**
 * @author Dejan Antanaskovic, Patrice Congo
 *
 */
public class RoughnessPolygon implements IRoughnessPolygon 
{
	private final Feature feature;
	
	
	/**
	 * Create a RoughnessPolygon object base on an existing
	 * feature
	 * @param feature
	 * @throws IllegalArgumentException if feature is 
	 * 				null and not of the appopriate type
	 */
	public RoughnessPolygon(Feature feature) 
	{
		this.feature=null;
	}
	
	/**
	 * Creates a new RoughnessPolygon for the passed 
	 * work space.
	 * if the work space has root element of the type
	 * RoughnessPolynomCollection than the a new Roughness
	 * polygon should be created and added to that one
	 * otherwise an illegal argument exception should be thrown
	 *  
	 * @param workspace
	 */
	public RoughnessPolygon(
				GMLWorkspace workspace) 
	{
		this.feature=null;
	}
	
	/**
	 * Create a roughness layer polygon.
	 * A underlaying feature is created during 
	 * the construction 
	 *
	 */
	public RoughnessPolygon() 
	{
		this.feature=null;
	}
	/* (non-Javadoc)
	 * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon#getLinestring()
	 */
	public GM_Polygon getPolygon() 
	{
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon#getRoughnessID()
	 */
	public String getRoughnessID() 
	{
		// TODO Auto-generated method stub
		return null;
	}
	
	public void setPolygon(GM_Polygon polygon) 
	{
		// TODO Auto-generated method stub
		
	}

	/* (non-Javadoc)
	 * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon#setRougthnessID(java.lang.String)
	 */
	public void setRougthnessID(
						String id) 
						throws IllegalArgumentException 
	{
		// TODO Auto-generated method stub

	}
	
	public Feature getFeature() 
	{
		return null;
	}
	
	
	@Override
	public String toString() 
	{
		return super.toString();
	}
	
	@Override
	public boolean equals(Object obj) 
	{
		return super.equals(obj);
	}
	
	public Feature getWrappedFeature()
	{
		return feature;
	}

}
