package org.kalypso.ogc.sensor.filter.creators;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.filter.IFilterCreator;
import org.kalypso.ogc.sensor.filter.IObservationFilter;
import org.kalypso.ogc.sensor.filter.filters.ValueFilter;
import org.kalypso.ogc.sensor.filter.filters.valuecomp.CompBetween;
import org.kalypso.ogc.sensor.filter.filters.valuecomp.CompBigger;
import org.kalypso.ogc.sensor.filter.filters.valuecomp.CompSmaller;
import org.kalypso.ogc.sensor.filter.filters.valuecomp.IValueComp;
import org.kalypso.util.parser.ParserException;
import org.kalypso.zml.filters.AbstractFilterType;
import org.kalypso.zml.filters.ValueFilterType;
import org.kalypso.zml.filters.valuecomp.AbstractValueCompType;
import org.kalypso.zml.filters.valuecomp.BetweenValueCompType;
import org.kalypso.zml.filters.valuecomp.BiggerValueCompType;
import org.kalypso.zml.filters.valuecomp.SmallerValueCompType;

/**
 * ValueFilterCreator
 * 
 * @author schlienger
 */
public class ValueFilterCreator implements IFilterCreator
{
  /**
   * @see org.kalypso.ogc.sensor.filter.IFilterCreator#createFilter(org.kalypso.zml.filters.AbstractFilterType, org.kalypso.ogc.sensor.IObservation)
   */
  public IObservationFilter createFilter( final AbstractFilterType aft,
      final IObservation baseObs ) throws SensorException
  {
    if( !(aft instanceof ValueFilterType) )
      throw new IllegalArgumentException( "Not a " + ValueFilterType.class.getName() );
    
    final ValueFilterType ft = (ValueFilterType) aft;

    final IObservation filteredObs = FilterCreatorHelper.resolveFilter( ft.getFilter(), baseObs );
	
    final ValueFilter filter = new ValueFilter();
    
    try
    {
      filter.initFilter( createComparators( ft.getValueComp(), filteredObs.getAxisList() ), filteredObs );
    }
    catch( ParserException e )
    {
      throw new SensorException( e );
    }
    
    return filter;
  }

  /**
   * Creates the comparators
   * 
   * @param comps
   * @param axes
   * @return list of comparators
   * @throws ParserException
   */
  private final static List createComparators( final List comps, final IAxis[] axes ) throws ParserException
  {
    final List fc = new ArrayList( comps.size() );
    
    for( final Iterator it = comps.iterator(); it.hasNext(); )
    {
      final AbstractValueCompType vc = (AbstractValueCompType) it.next();
      
      fc.add( createComp( vc, axes ) );
    }
    
    return fc;
  }
  
  /**
   * Creates the comparator for the given binding type
   * 
   * @param avc
   * @param axes
   * @return
   * @throws ParserException
   */
  private static IValueComp createComp( final AbstractValueCompType avc, final IAxis[] axes ) throws ParserException
  {
    if( avc instanceof SmallerValueCompType )
    {
      final SmallerValueCompType comp = (SmallerValueCompType) avc;
      return new CompSmaller( axes, avc.getAxisType(), comp.getValue(), comp.isModeIncl() );
    }

    if( avc instanceof BetweenValueCompType )
    {
      final BetweenValueCompType comp = (BetweenValueCompType) avc;
      return new CompBetween( axes, avc.getAxisType(), comp.getFrom(), comp.isModeInclFrom(), comp.getTo(), comp.isModeInclTo() );
    }

    if( avc instanceof BiggerValueCompType )
    {
      final BiggerValueCompType comp = (BiggerValueCompType) avc;
      return new CompBigger( axes, avc.getAxisType(), comp.getValue(), comp.isModeIncl() );
    }

    return null;
  }
}
