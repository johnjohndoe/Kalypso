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
   * @param axes the list of axes to scan
   * @param axisName the name of the axis which is searched
   * @throws NoSuchElementException when no axis matches the name
   */
  public static IAxis findAxisByName( final IAxis[] axes, final String axisName ) throws NoSuchElementException
  {
    for( int i = 0; i < axes.length; i++ )
    {
      if( axes[i].getLabel().equalsIgnoreCase( axisName ) )
        return axes[i];
    }
    
    throw new NoSuchElementException( "No axis found with name: " + axisName );
  }
  
  /**
   * Finds the axis of the given observation that has the given type. Returns the first axis found.
   * 
   * @param axes the list of axes to scan
   * @param axisType the type of the axis which is searched
   * @throws NoSuchElementException when no axis matches the name
   */
  public static IAxis findAxisByType( final IAxis[] axes, final String axisType ) throws NoSuchElementException
  {
    for( int i = 0; i < axes.length; i++ )
    {
      if( axes[i].getType().equalsIgnoreCase( axisType ) )
        return axes[i];
    }
    
    throw new NoSuchElementException( "No axis found with type: " + axisType );
  }
  
  /**
   * Helper that returns an axis which is compatible with specified Class of data
   */
  public static IAxis[] findAxisByClass( final IAxis[] axes, final Class desired ) throws NoSuchElementException
  {
    final ArrayList list = new ArrayList( axes.length );
    
    for( int i = 0; i < axes.length; i++ )
    {
      if( desired.isAssignableFrom( axes[i].getDataClass() ) )
        list.add( axes[i] );
    }
    
    if( list.size() == 0 )
      throw new NoSuchElementException( "No axis found with class: " + desired );
    
    return (IAxis[])list.toArray( new IAxis[list.size()] );
  }
  
  /**
   * Returns the axes which are keys. Returns an empty array if no axis found.
   */
  public static IAxis[] findAxisByKey( final IAxis[] axes )
  {
    final ArrayList list = new ArrayList( axes.length );
    
    for( int i = 0; i < axes.length; i++ )
    {
      if( axes[i].isKey() )
        list.add( axes[i] );
    }
    
    return (IAxis[])list.toArray( new IAxis[list.size()] );
  }
}
