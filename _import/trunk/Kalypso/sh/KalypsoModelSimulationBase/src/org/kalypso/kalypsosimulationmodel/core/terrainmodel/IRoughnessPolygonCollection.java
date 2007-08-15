package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import java.util.List;

import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;

/**
 * The interface implemented by classes representing simBase:RoughnessCollectionElement
 * 
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 * @author Patrice Congo
 */
public interface IRoughnessPolygonCollection extends IFeatureWrapperCollection<IRoughnessPolygon>
{
  public void setActiveLayer( IRoughnessLayer layer );

  /**
   * Selects all roughness polygons containing this point. Since the point may be on the border, there might be several
   * polygons as a result.
   * 
   * @param point
   *            the location where to find the roughness polygon
   * @return a list of roughness polygons that contain the given point
   */
  public List<IRoughnessPolygon> selectRoughnessPolygons( GM_Position point );

  /**
   * Selects all roughness polygons overlapping the provided polygon.
   * 
   * 
   * @param selectionZone
   *            the area where to select the roughness polygon
   * @return a set of rougness polygons overlaping the given zone.
   */
  public List<IRoughnessPolygon> selectRoughnessPolygons( GM_Surface selectionZone );

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
