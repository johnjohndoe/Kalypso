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
package org.kalypso.model.hydrology.internal.timeseries.binding;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.joda.time.Period;
import org.kalypso.commons.time.PeriodUtils;
import org.kalypso.contribs.java.util.CalendarUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.timeseries.binding.IStation;
import org.kalypso.model.hydrology.timeseries.binding.ITimeseries;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

/**
 * @author Gernot Belger
 */
public class Timeseries extends Feature_Impl implements ITimeseries
{
  public Timeseries( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  @Override
  public String getQuality( )
  {
    return getProperty( PROPERTY_QUALITY, String.class );
  }

  @Override
  public String getParameterType( )
  {
    return getProperty( PROPERTY_PARAMETER_TYPE, String.class );
  }

  private Integer getTimestepAmount( )
  {
    return getProperty( PROPERTY_TIMESTEP_AMOUNT, Integer.class );
  }

  private String getTimestepField( )
  {
    return getProperty( PROPERTY_TIMESTEP_FIELD, String.class );
  }

  @Override
  public Period getTimestep( )
  {
    final Integer amount = getTimestepAmount();
    final String fieldName = getTimestepField();

    final int field = CalendarUtilities.getCalendarField( fieldName );

    return PeriodUtils.getPeriod( field, amount );
  }

  @Override
  public ZmlLink getDataLink( )
  {
    return new ZmlLink( this, PROPERTY_DATA );
  }

  @Override
  public IStation getStation( )
  {
    return (IStation) getParent();
  }

  @Override
  public void deleteDataFile( ) throws CoreException
  {
    final ZmlLink dataLink = getDataLink();
    final IFile file = dataLink.getFile();
    if( file != null && file.exists() )
      file.delete( false, true, null );
  }
}