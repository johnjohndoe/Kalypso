package org.kalypso.kalypsomodel1d2d.conv;

import java.util.Date;
import java.util.List;

import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;

/**
 * The handler interface for rma10s model element. Class implementing this interface can be set to handle parsing events
 * from a {@link IRMA10SModelReader}
 * 
 * @author Patrice Congo
 */
public interface IRMA10SModelElementHandler
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
   *            complete line read
   * @param id
   *            the id of the node
   * @param the
   *            easting coordinate of the node
   * @param the
   *            northing coordinate of the node
   * @param the
   *            elevation of the node
   */
  public void handleNode( String lineString, int id, double easting, double northing, double elevation );

  // edge LINEID, ID, node1, node2, ellinks, elrechts, mid-side node (or -1)
  /**
   * Invoqued by the reader to signal that an arc (edge) has been read and parsed
   * 
   * @param lineString
   *            whole parsed string
   * @param id
   *            ID of the edge
   * @param node1ID
   *            ID of the upsided node of the edge
   * @param node2ID
   *            ID of the downsided node of the edge
   * @param elementLeftID
   *            ID of the left-sided element of the edge
   * @param elementRigthID
   *            ID of the rigth-sided element of the edge
   * @param middleNodeID
   *            ID of the mid-side node of the edge (created while calculation) or -1 if there is non yet
   */
  public void handleArc( String lineString, int id, int node1ID, int node2ID, int elementLeftID, int elementRightID, int middleNodeID );

  // LineID, ID
  /**
   * Invoqued by the reader to signal that an element has been read and parsed
   */
  public void handleElement( String lineString, int id, int currentRougthnessClassID, int previousRoughnessClassID, int eleminationNumber );

  // LineID, ID
  /**
   * Invoqued by the reader to signal that a result (VA) has been read and parsed
   */
  public void handleResult( String lineString, int id, double vx, double vy, double depth, double waterlevel );

  /**
   * Handle a line that the reader cannot interprete
   * 
   * @param lineString
   *            the line that cannot be interpreted by the ready
   */
  public void handlerUnIdentifyable( String lineString );

  /**
   * Handle error the Reader
   */
  public void handleError( String lineString, EReadError errorHints );

  public void setIRoughnessIDProvider( IRoughnessIDProvider roughnessIDProvider ) throws IllegalArgumentException;

  public List<IFeatureWrapper2> getCreatedFeatures( );

  public void handleTime( final String line, final Date time );

  public void handleJunction( String line, int junctionID, int element1dID, int boundaryLine2dID, int node1dID );

  public void handleFlowResitance( final String line, final int id, final double combinedLambda, final double soilLambda, final double vegetationLambda );

  public void handleNodeInformation( String line, int id, int dry, double value1, double value2, double value3, double value4 );
}
