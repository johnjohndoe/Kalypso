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
   * @return first axis found
   * @throws NoSuchElementException when no axis matches the name
   */
  public static IAxis findAxisByName( final IAxis[] axes, final String axisName ) throws NoSuchElementException
  {
    for( int i = 0; i < axes.length; i++ )
    {
      if( axes[i].getName().equalsIgnoreCase( axisName ) )
        return axes[i];
    }
    
    throw new NoSuchElementException( "No axis found with name: " + axisName );
  }
  
  /**
   * Finds the axis of the given observation that has the given type.
   * 
   * @param axes the list of axes to scan
   * @param axisType the type of the axis which is searched
   * @return the first axis found
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
   * @param axes
   * @param desired
   * @return an axis which is compatible with specified Class of data
   * @throws NoSuchElementException
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
   * @param axes
   * @return the axes which are key-axes. Returns an empty array if no axis found.
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
  
  /**
   * @param model
   * @param sep string separator between elements
   * @return simple string representation of the given model
   * 
   * @throws SensorException
   */
  public static String dump( final ITuppleModel model, final String sep ) throws SensorException
  {
    final StringBuffer bf = new StringBuffer();
    
    final IAxis[] axes = model.getAxisList();
    
    for( int i = 0; i < model.getCount(); i++ )
    {
      for( int j = 0; j < axes.length; j++ )
      {
        final IAxis axis = axes[j];
        
        bf.append( model.getElement(i, axis).toString() ).append( sep );
      }
      
      bf.append('\n');
    }
    
    return bf.toString();
  }
}
