package org.kalypso.ogc.sensor.filter.filters;

import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.filter.filters.valuecomp.IValueComp;
import org.kalypso.ogc.sensor.impl.SimpleTuppleModel;
import org.kalypso.util.runtime.IVariableArguments;

/**
 * ValueFilter
 * 
 * @author schlienger
 */
public class ValueFilter extends AbstractObservationFilter
{
  private final Map m_axisMap = new Hashtable();

  /**
   * @see org.kalypso.ogc.sensor.filter.filters.AbstractObservationFilter#initFilter(java.lang.Object,
   *      org.kalypso.ogc.sensor.IObservation)
   */
  public void initFilter( final Object conf, final IObservation obs ) throws SensorException
  {
    super.initFilter( conf, obs );

    m_axisMap.clear();
    
    final Iterator it = ((List)conf).iterator();
    while( it.hasNext() )
    {
      final IValueComp vc = (IValueComp) it.next();
      
      m_axisMap.put( vc.getAxis(), vc );
    }
  }
  
  /**
   * @see org.kalypso.ogc.sensor.filter.filters.AbstractObservationFilter#getValues(org.kalypso.util.runtime.IVariableArguments)
   */
  public ITuppleModel getValues( final IVariableArguments args ) throws SensorException
  {
    final ITuppleModel values = super.getValues( args );
    final IAxis[] axes = values.getAxisList();
    
    final SimpleTuppleModel newValues = new SimpleTuppleModel( axes );
    
    for( int i = 0; i < values.getCount(); i++ )
    {
      final Vector tupple = new Vector( axes.length );
      
      boolean add = true;
      
      for( int j = 0; j < axes.length; j++ )
      {
        final IValueComp comp = (IValueComp) m_axisMap.get( axes[j] );
        
        final Object elt = values.getElement( i, axes[j] );
        
        if( comp == null || comp.validates( elt ) )
          tupple.add( axes[j].getPosition(), elt );
        else
        {
          add = false;
          continue;
        }
      }
      
      if( add )
        newValues.addTupple( tupple );
    }
    
    return newValues;
  }
}