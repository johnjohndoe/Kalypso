package org.kalypso.ogc.sensor;



/**
 * An axis is used to describe a 'column' of values within
 * a Tupple Model.
 * <p>
 * Two axes are said to be equal when:
 * <ol>
 * <li>the dataclass of their elements is the same</li>
 * <li>their types are identical</li>
 * <li>they have the same unit</li>
 * <li>the value of the key-property is identical</li>
 * </ol>
 * 
 * @author schlienger
 */
public interface IAxis
{
  /**
   * @return the class of the data in this axis
   */
  public Class getDataClass();
  
  /** 
   * @return the application dependent type of this axis
   */
  public String getType();
  
  /** 
   * @return the unit of this axis
   */
  public String getUnit();
  
  /**
   * @return the name of this axis
   */
  public String getName();
  
  /**
   * Returns true when this axis is part of the key of the TuppleModel. Key can be used 
   * to avoid duplicate entries in the model.
   * 
   * @return boolean
   */
  public boolean isKey();
  
  /**
   * Returns true when this axis is persistable when saving the observation
   * to some medium like a file. Some observations generate new axes depending
   * on their characteristics. For instance a WQ-ObservationFilter
   * either simulates a Q or a W axis, depending on the wrapped observation, 
   * and this simulated axis should not be saved along the others (since
   * it can be computed at any time).
   * 
   * @return boolean
   */
  public boolean isPersistable();
}
