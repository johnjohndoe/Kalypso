package org.kalypso.ogc.sensor.filter.creators;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.filter.IFilterCreator;
import org.kalypso.ogc.sensor.filter.IObservationFilter;
import org.kalypso.ogc.sensor.filter.filters.DataHoleFilter;
import org.kalypso.zml.filters.AbstractFilterType;
import org.kalypso.zml.filters.DataholeFilterType;

/**
 * DataHoleFilterCreator
 * 
 * @author schlienger
 */
public class DataHoleFilterCreator implements IFilterCreator
{
  /**
   * @see org.kalypso.ogc.sensor.filter.IFilterCreator#createFilter(org.kalypso.zml.filters.AbstractFilterType, org.kalypso.ogc.sensor.IObservation)
   */
  public IObservationFilter createFilter( AbstractFilterType aft, IObservation baseObs ) throws SensorException
  {
    if( !(aft instanceof DataholeFilterType) )
      throw new IllegalArgumentException( "Not a " + DataholeFilterType.class.getName() );
    
    final DataholeFilterType ft = (DataholeFilterType) aft;

    final IObservation filteredObs = FilterCreatorHelper.resolveFilter( ft.getFilter(), baseObs );
	
    Double replaceWith = null;
    if( ft.isReplace() )
      replaceWith = new Double( ft.getReplaceWith() );
    
    final DataHoleFilter filter = new DataHoleFilter( ft.getValue(), ft.getStatus(), replaceWith );
    
    filter.initFilter( null, filteredObs );
    
    return filter;  
  }
}
