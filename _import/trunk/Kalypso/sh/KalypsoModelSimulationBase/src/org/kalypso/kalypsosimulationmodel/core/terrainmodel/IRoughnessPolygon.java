package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Object;

/**
 * The interface to be implemented by classes representing a simBase:RoughnessPolygon element
 * 
 * @author Dejan Antanaskovic, Patrice Congo
 */
public interface IRoughnessPolygon extends IFeatureWrapper2
{
  /**
   * Returns the rougthness ID for this polynom
   * 
   * @return the id of the rougthness associated to the polygon
   */
  public String getRoughnessStyle( );

  public void setRoughnessClassMember( Feature linkedFeature ) throws IllegalArgumentException;

  /**
   * Returns the roughness MultiPolygon
   * 
   * @return the surface
   */
  public GM_MultiSurface getSurface( );

  /**
   * Sets the geometry of the roughness polygon
   * 
   * @param polygon
   *          the polygon to set
   */
  public void setSurface( GM_MultiSurface polygon );

  /**
   * Sets the geometry of the roughness polygon
   * 
   * @param polygon
   *          the polygon to set
   * @throws IllegalArgumentException if parameter cannot be converted to GM_MultiSurface 
   */
  public void setSurface( GM_Object object ) throws IllegalArgumentException;
}
