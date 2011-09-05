package org.kalypso.kalypsomodel1d2d.internal.import2dm;

import java.util.List;

import org.kalypsodeegree.model.geometry.GM_Position;


/**
 * The handler interface for RMA·Kalypso model element. Class implementing this interface can be set to handle parsing
 * events from a {@link IRMA10SModelReader}
 * 
 * @author Thomas Jung
 */
interface ISMSModel
{
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
  void addNode( String lineString, int id, double easting, double northing, double elevation );

  /**
   * Invoked by the reader to signal that an element has been read and parsed
   */
  void addElement( String lineString, int id, Integer[] nodeIds, int rougthnessClassID );

  GM_Position getNode( Integer nodeId );

  /**
   * Returns a (unmodifiable) list of all elements of this model.
   */
  List<SmsElement> getElementList( );

  /**
   * Returns the coordinate systems all elements and nodes of this model are in.
   */
  String getSrs( );
}