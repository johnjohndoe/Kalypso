package org.kalypso.kalypsomodel1d2d.conv;

import java.util.Date;
import java.util.List;

import org.kalypso.kalypsomodel1d2d.conv.RMA10S2GmlConv.RESULTLINES;

/**
 * The handler interface for RMA�Kalypso model element. Class implementing this interface can be set to handle parsing
 * events from a {@link IRMA10SModelReader}
 * 
 * @author Patrice Congo
 */
public interface IRMA10SModelElementHandler
{
  /**
   * Invoked to signal the start the reading process
   */
  void start( );

  /**
   * Invoke to signal the end of the reading process
   */
  void end( );

  /**
   * Invoked by the reader to signal that an node has been read and parsed
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
  void handleNode( String lineString, int id, double easting, double northing, double elevation );

  // edge LINEID, ID, node1, node2, ellinks, elrechts, mid-side node (or -1)
  /**
   * Invoqued by the reader to signal that an arc (edge) has been read and parsed
   * 
   * @param lineString
   *          whole parsed string
   * @param id
   *          ID of the edge
   * @param node1ID
   *          ID of the upside node of the edge
   * @param node2ID
   *          ID of the downside node of the edge
   * @param elementLeftID
   *          ID of the left-sided element of the edge
   * @param elementRigthID
   *          ID of the right-sided element of the edge
   * @param middleNodeID
   *          ID of the mid-side node of the edge (created while calculation) or -1 if there is non yet
   */
  void handleArc( String lineString, int id, int node1ID, int node2ID, int elementLeftID, int elementRightID, int middleNodeID );

  // LineID, ID
  /**
   * Invoked by the reader to signal that an element has been read and parsed
   */
  void handleElement( String lineString, int id, int currentRougthnessClassID, int previousRoughnessClassID, int eleminationNumber );

  // LineID, ID
  /**
   * Invoked by the reader to signal that a result (VA) has been read and parsed
   */
  void handleResult( String lineString, int id, double vx, double vy, double depth, double waterlevel );

  // LineID, ID
  /**
   * Invoked by the reader to signal that a result (VO, GA, GO) has been read and parsed
   */
  void handleTimeDependentAdditionalResult( String lineString, int id, double vx, double vy, double depth, RESULTLINES resultlines );

  /**
   * Handle error the Reader
   */
  void handleError( String lineString, EReadError errorHints );

  void handleTime( final String line, final Date time );

  void handleJunction( String line, int junctionID, int element1dID, int boundaryLine2dID, int node1dID );

  void handleFlowResistance( final String line, final int id, final double combinedLambda, final double soilLambda, final double vegetationLambda );

  void handleNodeInformation( String line, int id, int dry, double value1, double value2, double value3, double value4 );

  void handle1dJunctionInformation( final String line, final int junctionId, final List<Integer> junctionNodeIDList );

  void handle1dPolynomialRangesInformation( final String line, final String lStrPolyKind, final int lIntNodeId, final int lIntAmountRanges, final List<Double> lListPolyAreaMaxRanges );

  void handle1dPolynomeMinMax( final String line, final int id, final double min, final double max );

  void handle1dSplittedPolynomialsInformation( final String line, final String lStrPolyKind, final int lIntNodeId, final int lIntAmountRanges, final List<Double> lListPolyAreaMaxRanges, final Double lIntSlope );

  void handleNode( final String line, final int id, final double easting, final double northing, final double elevation, final double stationName );

  void handleRoughness( String id, String label );
}