package org.kalypso.ogc.sensor;

import java.util.ArrayList;
import java.util.NoSuchElementException;

/**
 * Utilities around IObservation.
 * 
 * @author schlienger
 */
public class ObservationUtilities
{
  private ObservationUtilities()
  {
    // not intended to be instanciated
  }
  
  /**
   * Finds the axis of the given observation that has the given name.
   * 
   * @param obs the observation to scan
   * @param axisName the name of the axis which is searched
   * @throws NoSuchElementException when no axis matches the name
   */
  public static IAxis findAxis( final IObservation obs, final String axisName ) throws NoSuchElementException
  {
    final IAxis[] axes = obs.getAxisList();
    
    for( int i = 0; i < axes.length; i++ )
    {
      if( axes[i].getLabel().equalsIgnoreCase( axisName ) )
        return axes[i];
    }
    
    throw new NoSuchElementException( "No axis found with name '" + axisName + "' in observation '" + obs.getName() + "'" );
  }
  
  /**
   * Helper that returns an axis which is compatible with specified Class of data
   */
  public static IAxis[] findAxis( final IObservation obs, final Class desired ) throws NoSuchElementException
  {
    final IAxis[] axes = obs.getAxisList();
    
    final ArrayList list = new ArrayList( axes.length );
    
    for( int i = 0; i < axes.length; i++ )
    {
      if( desired.isAssignableFrom( axes[i].getDataClass() ) )
        list.add( axes[i] );
    }
    
    if( list.size() == 0 )
      throw new NoSuchElementException( "No axis found with class '" + desired + "' in observation '" + obs.getName() + "'" );
    
    return (IAxis[])list.toArray( new IAxis[list.size()] );
  }
}
