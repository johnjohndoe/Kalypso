package org.kalypso.ogc.sensor;


/**
 * Data Model for the value elements of observations.
 * 
 * @author schlienger
 */
public interface ITuppleModel
{
  /** 
   * @return axis list for which this model delivers elements
   */
  public IAxis[] getAxisList();
  
  /**
   * @return amount of items in this observation's model
   * @throws SensorException
   */
  public int getCount() throws SensorException;

  /**
   * @param axis
   * @return the range of the given axis for this tupple model
   * @throws SensorException
   */
  public IAxisRange getRangeFor( IAxis axis ) throws SensorException;
  
  /**
   * @param index
   * @param axis
   * @return element at index for axis
   * 
   * @throws SensorException
   */
  public Object getElement( final int index, final IAxis axis ) throws SensorException;
  
  /**
   * Sets the element at index for axis.
   * @param index
   * @param element
   * @param axis
   * 
   * @throws SensorException 
   */
  public void setElement( final int index, final Object element, final IAxis axis ) throws SensorException;

  /**
   * Returns the index of the given element in the valueslist for the given axis. Calling
   * this method makes only sense for key axes since other axes can have duplicates. In either
   * case it returns the index of the first element found.
   * 
   * @param element
   * @param axis
   * @return index >= 0 if element is found. Returns -1 if element could not be found.
   * 
   * @throws SensorException when there are no axis
   */
  public int indexOf( final Object element, final IAxis axis ) throws SensorException;
}
