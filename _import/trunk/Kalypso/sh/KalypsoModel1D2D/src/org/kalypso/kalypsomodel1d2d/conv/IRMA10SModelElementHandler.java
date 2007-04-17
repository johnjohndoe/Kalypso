package org.kalypso.kalypsomodel1d2d.conv;

import java.util.List;

import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;

  /**
   * The handler interface for rma10s model element.
   * Class implementing this interface can be set to handle
   * parsing events from a {@link IRMA10SModelReader}  
   * 
   *@author Patrice Congo
   */
  public interface IRMA10SModelElementHandler
  {
    /**
     * Invoqued to signal the start the reading process
     */
    public void start();
    
    /**
     * Invoke to signal the end of the reading process
     */
    public  void end();
    
    /**
     * Invoqued by the reader to signal that an node 
     * has been read and parsed
     * @param the complete line read
     * @param id the id of the node
     * @param the easting coordinate of the node
     * @param  the northing coordinate of the node
     * @param the elevation of the node
     */
    public void handleNode(
                      String lineString, 
                      int id,
                      double easting,
                      double northing,
                      double elevation);
      
//    edge LINEID, ID, node1, node2, ellinks, elrechts
    /**
     * Invoqued by the reader to signal that an arc (edge) 
     * has been read and parsed
     */  
    public void handleArc(
                      String lineString,
                      int id,
                      int node1ID,
                      int node2ID,
                      int elementLeftID,
                      int elementRightID,
                      int middleNodeID
                      );
//    LineID, ID
      /**
       * Invoqued by the reader to signal that an element 
       * has been read and parsed
       */
      public void handleElement(
                    String lineString,
                    int id,
                    int currentRougthnessClassID,
                    int previousRoughnessClassID,
                    int eleminationNumber);
      
      /**
       * Handle a line that the reader cannot interprete
       * @param lineString the line that cannot be interpreted
       *        by the ready
       */
      public void handlerUnIdentifyable(String lineString);
      
    /**
     * Handle error the Reader 
     */
      public void handlerError(
                        String lineString, 
                        EReadError errorHints);
      
      
      public void setModelElementIDProvider(
                        IModelElementIDProvider modelElementIDProvider)
                        throws IllegalArgumentException;
      
      
      public void setIRoughnessIDProvider(
                        IRoughnessIDProvider roughnessIDProvider)
                        throws IllegalArgumentException;
      
      public List<IFeatureWrapper2> getCreatedFeatures();
  }
  