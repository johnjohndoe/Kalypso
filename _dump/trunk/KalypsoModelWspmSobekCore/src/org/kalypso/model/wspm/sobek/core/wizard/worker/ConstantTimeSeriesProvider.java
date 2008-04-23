/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.model.wspm.sobek.core.wizard.worker;

import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.Map;

import javax.xml.namespace.QName;

import org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNodeLastfallCondition;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekConstants;
import org.kalypso.model.wspm.sobek.core.wizard.pages.IBoundaryConditionGeneral;
import org.kalypso.model.wspm.sobek.core.wizard.pages.PageEditBoundaryConditionTimeSeries;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;

import com.sun.org.apache.xerces.internal.jaxp.datatype.XMLGregorianCalendarImpl;

/**
 * @author kuch
 */
public class ConstantTimeSeriesProvider extends AbstractTimeSeriesProvider
{
  public ConstantTimeSeriesProvider( final IBoundaryConditionGeneral settings, final PageEditBoundaryConditionTimeSeries pageTS )
  {
    super( settings, pageTS );
  }

  private void addResult( final TupleResult result, final GregorianCalendar calendar, final Double value )
  {
    /* if wq-relation -> components must have the order date, w, q otherwise -> date, w or q */
    final IComponent[] components = result.getComponents();

    final IRecord record = result.createRecord();
    record.setValue( components[0], new XMLGregorianCalendarImpl( calendar ) );

    for( int i = 1; i < components.length; i++ )
      record.setValue( components[i], value );

    result.add( record );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.wizard.worker.ITimeSeriesProvider#fillTupleResult(org.kalypso.observation.result.TupleResult)
   */
  public void fillTupleResult( final TupleResult result )
  {
    final GregorianCalendar cStart = getStartDate();
    final GregorianCalendar cEnd = getEndDate();
    final Double value = getPageTS().getConstValue();
    final Integer intervall = getPageTS().getConstValueIntervall();

    /* set starting value */
    addResult( result, cStart, value );

    final GregorianCalendar base = (GregorianCalendar) cStart.clone();

    while( base.before( cEnd ) )
    {
      base.add( Calendar.MINUTE, intervall );

      addResult( result, base, value );
    }

    /* set end value */
    addResult( result, cEnd, value );

  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.wizard.worker.ITimeSeriesProvider#getBasicChanges()
   */
  @Override
  public Map<QName, Object> getBasicChanges( )
  {
    /* set unused values null */
    final Map<QName, Object> changes = super.getBasicChanges();

    changes.put( ISobekConstants.QN_HYDRAULIC_BOUNDARY_NODE_CONDITION_CONST_VALUE, getPageTS().getConstValue() );
    changes.put( ISobekConstants.QN_HYDRAULIC_BOUNDARY_NODE_CONDITION_CONST_VALUE_INTERVALL, getPageTS().getConstValueIntervall() );
    changes.put( ISobekConstants.QN_HYDRAULIC_BOUNDARY_NODE_CONDITION_TYPE, IBoundaryNodeLastfallCondition.BOUNDARY_CONDITION_TYPE.eConstant.toGmlString() );

    return changes;
  }
}
