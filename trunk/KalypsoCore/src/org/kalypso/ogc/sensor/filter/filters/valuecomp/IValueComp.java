package org.kalypso.ogc.sensor.filter.filters.valuecomp;


/**
 * IValueComp
 * 
 * @author schlienger
 */
public interface IValueComp
{
  /**
   * @param element to compare
   * @return true if element validates to this value comparator
   */
  public boolean validates( final Object element );
}
