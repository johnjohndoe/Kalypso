package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import java.util.List;

import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Polygon;

/**
 * The interface implemented by classes representing simBase:RoughnessCollectionElement
 * 
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a> 
 * @author Patrice Congo
 */
public interface IRoughnessPolygonCollection extends IFeatureWrapperCollection<IRoughnessPolygon>
{
  /**
   * Selects all roughness polygons containing this point. 
   * Since the point may be on the border, there might be several polygons as a result.
   * 
   * @param point the location where to find the roughness polygon
   * @return array if rougness polygons containing given point
   */
  public IRoughnessPolygon[] getSelectedPolygons( GM_Point point );

  /**
   * Gets all roughness polygons in this collection
   */
  public List<IRoughnessPolygon> getRoughnessPolygons( );

  /**
   * Build the roughness estimation specification of the given polygon (eg. FE element)
   * 
   * @param polygon
   * @return
   */
  public IRoughnessEstimateSpec getRoughnessEstimateSpec( GM_Polygon object );
  
  /**
   * Build the roughness estimation specification of the given polygon (eg. FE element)
   * 
   * @param polygon
   * @return
   */
  public IRoughnessEstimateSpec getRoughnessEstimateSpec( GM_Object object );
  

  /**
   * Checks if there is overlapping between roughness polygons
   */
  public boolean checkOverlapping( );

  /**
   * Returns a list of overlapped polygons
   * 
   * @return List of overlapped polygons
   */
  public List<IRoughnessPolygon> getOverlappedPolygons( );

}
