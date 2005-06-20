/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.sensor.filter.creators;

import java.net.URL;
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
  public IObservationFilter createFilter( final AbstractFilterType aft, final IObservation baseObs, final URL context )
      throws SensorException
  {
    if( !( aft instanceof ValueFilterType ) )
      throw new IllegalArgumentException( "Not a " + ValueFilterType.class.getName() );

    final ValueFilterType ft = (ValueFilterType)aft;

    final IObservation filteredObs = FilterCreatorHelper.resolveFilter( ft.getFilter(), baseObs, context );

    final ValueFilter filter = new ValueFilter();

    try
    {
      filter.initFilter( createComparators( ft.getValueComp(), filteredObs.getAxisList() ), filteredObs, context );
    }
    catch( ParserException e )
    {
      throw new SensorException( e );
    }

    return filter;
  }

  /**
   * Creates the comparators
   */
  private final static List createComparators( final List comps, final IAxis[] axes ) throws ParserException
  {
    final List fc = new ArrayList( comps.size() );

    for( final Iterator it = comps.iterator(); it.hasNext(); )
    {
      final AbstractValueCompType vc = (AbstractValueCompType)it.next();

      fc.add( createComp( vc, axes ) );
    }

    return fc;
  }

  /**
   * Creates the comparator for the given binding type
   * 
   * @param avc
   * @param axes
   * @throws ParserException
   */
  private static IValueComp createComp( final AbstractValueCompType avc, final IAxis[] axes ) throws ParserException
  {
    if( avc instanceof SmallerValueCompType )
    {
      final SmallerValueCompType comp = (SmallerValueCompType)avc;
      return new CompSmaller( axes, avc.getAxisType(), comp.getValue(), comp.isModeIncl() );
    }

    if( avc instanceof BetweenValueCompType )
    {
      final BetweenValueCompType comp = (BetweenValueCompType)avc;
      return new CompBetween( axes, avc.getAxisType(), comp.getFrom(), comp.isModeInclFrom(), comp.getTo(), comp
          .isModeInclTo() );
    }

    if( avc instanceof BiggerValueCompType )
    {
      final BiggerValueCompType comp = (BiggerValueCompType)avc;
      return new CompBigger( axes, avc.getAxisType(), comp.getValue(), comp.isModeIncl() );
    }

    return null;
  }
}
