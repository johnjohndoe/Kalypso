package org.kalypso.ogc.sensor;


/**
 * Data Model for the value elements of observations.
 * 
 * @author schlienger
 */
public interface ITuppleModel
{
  /** 
   * Returns the axis list for which this model delivers elements
   */
  public IAxis[] getAxisList();
  
  /**
   * Returns the amount of items in this observation's model 
   */
  public int getCount() throws SensorException;

  /**
   * Gets the element at index for axis
   * 
   * @throws SensorException
   */
  public Object getElement( final int index, final IAxis axis ) throws SensorException;
  
  /**
   * Sets the element at index for axis.
   * 
   * @throws SensorException 
   */
  public void setElement( final int index, final Object element, final IAxis axis ) throws SensorException;

  /**
   * Returns the index of the given element in the valueslist for the given axis.
   * @return index >= 0 if element is found. Returns -1 if element could not be found.
   * 
   * @throws SensorException when there are no axis
   */
  public int indexOf( final Object element, final IAxis axis ) throws SensorException;
}
