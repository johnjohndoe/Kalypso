package org.kalypso.ogc.sensor;

/**
 * @author schlienger
 */
public interface ITuppleModel
{
  public int getCount();
  
  public Object getElement( int index, int position );
  
  public void setElement( int index, Object element, int position );
}
