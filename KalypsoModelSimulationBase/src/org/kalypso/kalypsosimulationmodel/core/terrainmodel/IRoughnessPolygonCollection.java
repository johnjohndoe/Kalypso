package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import java.util.List;

import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Polygon;

/**
 * The interface implemented by classes representing simBase:RoughnessCollectionElement
 * 
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 * @author Patrice Congo
 */
public interface IRoughnessPolygonCollection extends IFeatureBindingCollection<IRoughnessPolygon>
{
  /**
   * Selects all roughness polygons containing this point. Since the point may be on the border, there might be several
   * polygons as a result.
   * 
   * @param point
   *          the location where to find the roughness polygon
   * @return a list of roughness polygons that contain the given point
   */
  public List<IRoughnessPolygon> selectRoughnessPolygons( GM_Position point );

  /**
   * Selects all roughness polygons overlapping the provided polygon.
   * 
   * 
   * @param selectionZone
   *          the area where to select the roughness polygon
   * @return a set of rougness polygons overlaping the given zone.
   */
  public List<IRoughnessPolygon> selectRoughnessPolygons( GM_Polygon selectionZone );

  /**
   * Gets all roughness polygons in this collection
   */
  public List<IRoughnessPolygon> getRoughnessPolygons( );

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
