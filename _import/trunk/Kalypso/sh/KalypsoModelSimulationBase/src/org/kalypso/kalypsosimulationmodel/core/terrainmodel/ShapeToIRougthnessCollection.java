/**
 * 
 */
package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import org.kalypso.kalypsosimulationmodel.core.ITransformAlgorithm;
import org.kalypsodeegree_impl.io.shpapi.ShapeFile;

/**
 * Implements the transformation algorithm from a shape file into a 
 * IRoughnessPolygonCollection
 * 
 * @author Dejan Antanaskovic, Patrice Congo
 */
public class ShapeToIRougthnessCollection 
		implements ITransformAlgorithm<IRoughnessPolygonCollection,ShapeFile> 
{

	//check gml workspace availability issue
	public IRoughnessPolygonCollection transform(
									ShapeFile toTransform) 
	{
		return null;
	}
}
