package org.kalypso.kalypsomodel1d2d.conv;

import java.util.List;

import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;

/**
 * The handler interface for RMA·Kalypso model element. Class implementing this interface can be set to handle parsing
 * events from a {@link IRMA10SModelReader}
 * 
 * @author Thomas Jung
 */
public interface IFEModelElementHandler
{
  /**
   * Invoqued to signal the start the reading process
   */
  public void start( );

  /**
   * Invoke to signal the end of the reading process
   */
  public void end( );

  /**
   * Invoqued by the reader to signal that an node has been read and parsed
   * 
   * @param the
   *          complete line read
   * @param id
   *          the id of the node
   * @param the
   *          easting coordinate of the node
   * @param the
   *          northing coordinate of the node
   * @param the
   *          elevation of the node
   */
  public void handleNode( String lineString, int id, double easting, double northing, double elevation );

  // LineID, ID
  /**
   * Invoqued by the reader to signal that an element has been read and parsed
   */
  public void handleElement( String lineString, int id, Integer[] nodeIds, int rougthnessClassID );

  /**
   * Handle a line that the reader cannot interpret
   * 
   * @param lineString
   *          the line that cannot be interpreted by the ready
   */
  public void handlerUnIdentifyable( String lineString );

  /**
   * Handle error the Reader
   */
  public void handleError( String lineString, EReadError errorHints );

  public List<IFeatureWrapper2> getCreatedFeatures( );

  public void addImporter( IDiscModelImporter importer );

}
