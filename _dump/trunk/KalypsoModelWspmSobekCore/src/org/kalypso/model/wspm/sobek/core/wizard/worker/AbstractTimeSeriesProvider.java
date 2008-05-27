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
package org.kalypso.model.wspm.sobek.core.wizard.worker;

import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.Map;

import javax.xml.namespace.QName;

import org.apache.commons.lang.NotImplementedException;
import org.kalypso.model.wspm.sobek.core.interfaces.ILastfall;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekConstants;
import org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNode.BOUNDARY_TYPE;
import org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNodeLastfallCondition.BOUNDARY_CONDITION_TYPE;
import org.kalypso.model.wspm.sobek.core.wizard.pages.IBoundaryConditionGeneral;
import org.kalypso.model.wspm.sobek.core.wizard.pages.PageEditBoundaryConditionGeneral;
import org.kalypso.model.wspm.sobek.core.wizard.pages.PageEditBoundaryConditionTimeSeries;

import com.sun.org.apache.xerces.internal.jaxp.datatype.XMLGregorianCalendarImpl;

/**
 * @author kuch
 */
public abstract class AbstractTimeSeriesProvider implements ITimeSeriesProvider
{
  public static ITimeSeriesProvider createProvider( final BOUNDARY_CONDITION_TYPE type, final PageEditBoundaryConditionGeneral general, final PageEditBoundaryConditionTimeSeries timeSeries )
  {

    if( BOUNDARY_CONDITION_TYPE.eConstant.equals( type ) )
      return new ConstantTimeSeriesProvider( general, timeSeries );
    else if( BOUNDARY_CONDITION_TYPE.eZml.equals( type ) )
    {
      final BOUNDARY_TYPE node_type = general.getBoundaryNodeType();

      if( BOUNDARY_TYPE.eWQ.equals( node_type ) )
        return new ZmlWQRelationTimeSeriesProvider( general, timeSeries );
      else
        return new ZmlTimeSeriesProvider( general, timeSeries );
    }
    else
      throw new NotImplementedException();
  }

  private final IBoundaryConditionGeneral m_general;

  private final PageEditBoundaryConditionTimeSeries m_pageTS;

  public AbstractTimeSeriesProvider( final IBoundaryConditionGeneral general, final PageEditBoundaryConditionTimeSeries pageTS )
  {
    m_general = general;
    m_pageTS = pageTS;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.wizard.worker.ITimeSeriesProvider#getBasicChanges()
   */
  public Map<QName, Object> getBasicChanges( )
  {
    final Map<QName, Object> changes = new HashMap<QName, Object>();

    final BOUNDARY_TYPE type = m_general.getBoundaryNodeType();
    if( !BOUNDARY_TYPE.eWQ.equals( type ) )
    {
      /* start date */
      final GregorianCalendar grStart = getStartDate();
      changes.put( ISobekConstants.QN_HYDRAULIC_BOUNDARY_NODE_CONDITION_BEGINS, new XMLGregorianCalendarImpl( grStart ) );

      /* end date */
      final GregorianCalendar grEnd = getEndDate();
      changes.put( ISobekConstants.QN_HYDRAULIC_BOUNDARY_NODE_CONDITION_ENDS, new XMLGregorianCalendarImpl( grEnd ) );
    }

    return changes;
  }

  protected BOUNDARY_TYPE getBoundaryNodeType( )
  {
    return m_general.getBoundaryNodeType();
  }

  protected GregorianCalendar getEndDate( )
  {
    return m_general.getEndDate();
  }

  protected ILastfall getLastfall( )
  {
    return m_pageTS.getLastfall();
  }

  protected PageEditBoundaryConditionTimeSeries getPageTS( )
  {
    return m_pageTS;
  }

  protected GregorianCalendar getStartDate( )
  {
    return m_general.getStartDate();
  }
}
