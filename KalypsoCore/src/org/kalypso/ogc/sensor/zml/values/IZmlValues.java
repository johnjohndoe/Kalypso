package org.kalypso.ogc.sensor.zml.values;


/**
 * Only used for Zml access to values.
 * 
 * @author schlienger
 */
public interface IZmlValues
{
  public int getCount();
  public Object getElement( final int index );
  public void setElement( final int index, final Object element );
  public int indexOf( final Object obj );
}
