package org.kalypso.ogc.sensor.filter.filters.valuecomp;

import org.kalypso.ogc.sensor.IAxis;


/**
 * IValueComp
 * 
 * @author schlienger
 */
public interface IValueComp
{
  /**
   * @return the axis for which this value comp applies
   */
  public IAxis getAxis();
  
  /**
   * @param element to compare
   * @return true if element validates to this value comparator
   */
  public boolean validates( final Object element );
}
