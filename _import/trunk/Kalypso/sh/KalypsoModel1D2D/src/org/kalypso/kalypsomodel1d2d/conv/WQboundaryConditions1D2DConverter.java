/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 *
 *  and
 *
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Contact:
 *
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *
 *  ---------------------------------------------------------------------------*/
package org.kalypso.kalypsomodel1d2d.conv;

import java.math.BigDecimal;
import java.util.Formatter;
import java.util.Iterator;
import java.util.LinkedHashMap;

import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition;
import org.kalypso.kalypsomodel1d2d.schema.dict.Kalypso1D2DDictConstants;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.observation.result.TupleResultUtilities;
import org.kalypso.observation.util.TupleResultIndex;

public class WQboundaryConditions1D2DConverter
{
  private final LinkedHashMap<Integer, IBoundaryCondition> m_boundaryConditionsIDProvider;

  public WQboundaryConditions1D2DConverter( final LinkedHashMap<Integer, IBoundaryCondition> boundaryConditionsIDProvider )
  {
    m_boundaryConditionsIDProvider = boundaryConditionsIDProvider;
  }

  public void writeWQbcFile( final Formatter format )
  {
    format.format( "TIT     %s%n", "stage discharge data file" );
    final Iterator<Integer> iterator = m_boundaryConditionsIDProvider.keySet().iterator();
    while( iterator.hasNext() )
    {
      final Integer bcParentID = iterator.next();
      final IBoundaryCondition boundaryCondition = m_boundaryConditionsIDProvider.get( bcParentID );
      final IObservation<TupleResult> obs = boundaryCondition.getObservation();
      final TupleResult obsResult = obs.getResult();
      final IComponent wComponent = TupleResultUtilities.findComponentById( obsResult, Kalypso1D2DDictConstants.DICT_COMPONENT_WATERLEVEL );
      final IComponent qComponent = TupleResultUtilities.findComponentById( obsResult, Kalypso1D2DDictConstants.DICT_COMPONENT_DISCHARGE );
      if( wComponent == null || qComponent == null )
        continue;
      final TupleResultIndex tupleResultIndex = new TupleResultIndex( obsResult, wComponent );
      final Iterator<IRecord> tupleIterator = tupleResultIndex.getIterator();
      format.format( "CTL%13d%n", bcParentID );
      while( tupleIterator.hasNext() )
      {
        final IRecord record = tupleIterator.next();
        final BigDecimal w = (BigDecimal) record.getValue( wComponent );
        final BigDecimal q = (BigDecimal) record.getValue( qComponent );
        format.format( "STD%13.4f%8.3f%n", w, q );
      }
    }
    format.format( "ENDDATA" );
  }

}
