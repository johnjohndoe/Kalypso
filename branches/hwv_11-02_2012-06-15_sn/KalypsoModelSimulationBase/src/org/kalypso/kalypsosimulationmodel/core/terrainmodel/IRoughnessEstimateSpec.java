package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import java.util.List;
import java.util.Map;

import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessCls;
import org.kalypsodeegree.model.geometry.GM_Envelope;

/**
 * 
 * @author Dejan Antanaskovic, Patrice Congo
 * 
 */
public interface IRoughnessEstimateSpec
{
  public static enum ERoughnessSelectionMechanism
  {
    /**
     * chrink the polygon area and make the selection afterward still one is most spread
     */
    SCHRINK_AND_SEL,

    /**
     * Select one of most spread randomly
     */
    RANDOM_SEL;
  }

  /**
   * returns the area ratio between the element geometry area and the brid cell area
   * 
   * @return the ratio between element geometry area and the the
   */
  public double getAreaRatio( );

  public void setRatio( double ratio );

  /**
   * Gets the rougness distribution within the given element geometry
   * 
   * @return a map representing the roughness distribution within the element geometry: the name of the roughtness as
   *         key and the
   * 
   * 
   */
  public Map<IRoughnessCls, Double> getHistogramm( );

  public IRoughnessCls[] possibleRoughnesses( );

  public GM_Envelope[] getCells( );

  /**
   * returns the most spread rougthness in the
   * 
   * @return
   */
  public IRoughnessCls[] mostSpreadRoughness( );

  /**
   * return the total part of most spread roughnesses in this estimate
   */
  public double getMostSpreadPart( );

  /**
   * returns the most spread roughness using the specified selection mechanism
   * 
   * @param rsm
   *          the sellection mechanism to use
   * @return the mostpread roughness
   */
  public IRoughnessCls mostSpreadRoughness( ERoughnessSelectionMechanism rsm );

  /**
   * Get all rougthness polygons that contribute roughnesses to this estimation e.i. containing the acessed polygon in
   * this area
   * 
   * @return all contibuting rougthness polygons
   */
  public List<IRoughnessPolygon> getContributingRoughnessPolygons( );

}
