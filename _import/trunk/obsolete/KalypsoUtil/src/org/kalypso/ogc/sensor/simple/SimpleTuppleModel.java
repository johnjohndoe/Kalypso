package org.kalypso.ogc.sensor.simple;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.ITuppleModel;

/**
 * @author schlienger
 */
public class SimpleTuppleModel implements ITuppleModel
{
  /**
   *
   */
  public SimpleTuppleModel( final IAxis[] axes )
  {
    this( axes, new Object[0][axes.length] );
  }
  
  public SimpleTuppleModel( final IAxis[] axes, final Object[][] values )
  {
    
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getCount()
   */
  public int getCount()
  {
    return 0;
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getElement(int, int)
   */
  public Object getElement( int index, int position )
  {
    return null;
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#setElement(int, java.lang.Object, int)
   */
  public void setElement( int index, Object element, int position )
  {
    
  }

  /**
   * Adds a tupple at the end of the model.
   * @param tupple the 'row' to be added
   */
  public void addTupple( final Object[] tupple )
  {
    
  }
  
  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#indexOf(java.lang.Object, org.kalypso.ogc.sensor.IAxis)
   */
  public int indexOf( Object element, IAxis axis )
  {
    return 0;
  }
}
