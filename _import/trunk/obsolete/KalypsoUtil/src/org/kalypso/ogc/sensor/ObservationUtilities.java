package org.kalypso.ogc.sensor;

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
  public static IAxis findAxis( IObservation obs, String axisName ) throws NoSuchElementException
  {
    IAxis[] axes = obs.getAxisList();
    
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
  public static IAxis findAxis( IObservation obs, Class desired ) throws NoSuchElementException
  {
    IAxis[] axes = obs.getAxisList();
    
    for( int i = 0; i < axes.length; i++ )
    {
      if( desired.isAssignableFrom( axes[i].getDataClass() ) )
        return axes[i];
    }
    
    throw new NoSuchElementException( "No axis found with class '" + desired + "' in observation '" + obs.getName() + "'" );
  }
}
