package org.kalypso.ogc.sensor;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.NoSuchElementException;

import org.kalypso.util.runtime.args.DateRangeArgument;

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
  public static IAxis findAxis( final IAxis[] axes, final String axisName ) throws NoSuchElementException
  {
    for( int i = 0; i < axes.length; i++ )
    {
      if( axes[i].getLabel().equalsIgnoreCase( axisName ) )
        return axes[i];
    }
    
    throw new NoSuchElementException( "No axis found with name: " + axisName );
  }
  
  /**
   * Helper that returns an axis which is compatible with specified Class of data
   */
  public static IAxis[] findAxis( final IAxis[] axes, final Class desired ) throws NoSuchElementException
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
  public static IAxis[] extractKeyAxis( final IAxis[] axes )
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
   * Creates a <code>DateRangeArgument</code> containing the range:
   * <pre>[now - pastDays, now]</pre>
   */
  public static DateRangeArgument createPastDaysArgument( final int pastDays )
  {
    final Calendar cal = Calendar.getInstance();
    
    final Date d1 = cal.getTime();
    
    cal.add( Calendar.DAY_OF_YEAR, -pastDays );
    
    final Date d2 = cal.getTime();
    
    return new DateRangeArgument( d1, d2 );
  }
}
