package org.kalypso.ogc.sensor;


/**
 * @author schlienger
 */
public interface ITuppleModel
{
  public int getCount();
  
  public Object getElement( int index, int position );
  
  public void setElement( int index, Object element, int position );

  /**
   * Returns the index of the given element in the valueslist for the given axis.
   * @return index >= 0 if element is found. Returns -1 if element could not be found.
   */
  public int indexOf( Object element, IAxis axis );
}
