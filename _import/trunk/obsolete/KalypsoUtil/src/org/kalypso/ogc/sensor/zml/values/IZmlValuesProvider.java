package org.kalypso.ogc.sensor.zml.values;


/**
 * Only used for Zml access to values.
 * 
 * @author schlienger
 */
public interface IZmlValuesProvider
{
  public int getCount();
  public Object getElement( int index );
  public void setElement( int index, Object element );
}
