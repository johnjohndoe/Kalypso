package org.kalypso.ogc.sensor.filter.filters;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.ITuppleModel;

/**
 * @author doemming
 */
public class FilterHelper
{
  public static IAxis getAxisByName(ITuppleModel model,String name)
  {
    IAxis[] axisList = model.getAxisList();
    for( int i = 0; i < axisList.length; i++ )
    {
      final IAxis axis = axisList[i];
      if(name.equals(axis.getName()))
          return axis;
    }
    return null;
  }
  
}
