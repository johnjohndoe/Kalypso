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
package org.kalypso.model.wspm.sobek.core.interfaces;

import java.util.GregorianCalendar;

import org.apache.commons.lang.NotImplementedException;
import org.kalypso.model.wspm.sobek.core.Messages;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author kuch
 */
public interface IBoundaryNodeLastfallCondition
{
  public static final String OBS_DATE = "urn:ogc:gml:dict:kalypso:wspm:sobek:boundaryConditionObservationDefs#DATE"; //$NON-NLS-1$

  public static final String OBS_W = "urn:ogc:gml:dict:kalypso:wspm:sobek:boundaryConditionObservationDefs#W"; //$NON-NLS-1$

  public static final String OBS_Q = "urn:ogc:gml:dict:kalypso:wspm:sobek:boundaryConditionObservationDefs#Q"; //$NON-NLS-1$

  public enum BOUNDARY_CONDITION_TYPE
  {
    eZml,
    eConstant;

    public static BOUNDARY_CONDITION_TYPE getType( final String gml )
    {
      if( "zml".equals( gml ) ) //$NON-NLS-1$
        return BOUNDARY_CONDITION_TYPE.eZml;
      else if( "const".equals( gml ) ) //$NON-NLS-1$
        return BOUNDARY_CONDITION_TYPE.eConstant;

      return BOUNDARY_CONDITION_TYPE.eZml;
    }

    public String toGmlString( )
    {
      final BOUNDARY_CONDITION_TYPE type = BOUNDARY_CONDITION_TYPE.valueOf( name() );
      switch( type )
      {
        case eConstant:
          return "const"; //$NON-NLS-1$

        case eZml:
          return "zml"; //$NON-NLS-1$

        default:
          throw new NotImplementedException();
      }
    }

    @Override
    public String toString( )
    {
      final BOUNDARY_CONDITION_TYPE type = BOUNDARY_CONDITION_TYPE.valueOf( name() );
      switch( type )
      {
        case eConstant:
          return Messages.IBoundaryNodeLastfallCondition_4;

        case eZml:
          return Messages.IBoundaryNodeLastfallCondition_5;

        default:
          throw new NotImplementedException();
      }
    }
  }

  IBoundaryNode getBoundaryNode( );

  public Double getConstantValue( );

  public Integer getConstantValueInterveal( );

  Feature getFeature( );

  ILastfall getLastfall( );

  public BOUNDARY_CONDITION_TYPE getLastUsedType( );

  public GregorianCalendar getObservationEnd( );

  public GregorianCalendar getObservationStart( );

  TimeseriesLinkType getTimeseriesLink( );

  public IObservation<TupleResult> getTimeSeriesObservation( );

  public Feature getTimeSeriesObservationFeature( );

  public Boolean hasTimeSeriesObservation( );

  boolean isConstantValueNode( );

  boolean isTimeSeriesNode( );

  void setTimeSeriesLink( TimeseriesLinkType lnk );
}
