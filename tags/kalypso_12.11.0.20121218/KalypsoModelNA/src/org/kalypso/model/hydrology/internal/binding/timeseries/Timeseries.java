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
package org.kalypso.model.hydrology.internal.binding.timeseries;

import java.util.Date;

import javax.xml.datatype.XMLGregorianCalendar;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.joda.time.Period;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.commons.time.PeriodUtils;
import org.kalypso.contribs.java.util.CalendarUtilities;
import org.kalypso.contribs.java.util.CalendarUtilities.FIELD;
import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.binding.timeseries.IStation;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.provider.IObsProvider;
import org.kalypso.ogc.sensor.provider.PlainObsProvider;
import org.kalypso.ogc.sensor.util.Observations;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.zml.core.base.IZmlSourceElement;
import org.kalypso.zml.obslink.TimeseriesLinkType;
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
  public void setQuality( final String quality )
  {
    setProperty( PROPERTY_QUALITY, quality );
  }

  @Override
  public String getParameterType( )
  {
    return getProperty( PROPERTY_PARAMETER_TYPE, String.class );
  }

  @Override
  public void setParameterType( final String parameterType )
  {
    setProperty( PROPERTY_PARAMETER_TYPE, parameterType );
  }

  private Integer getTimestepAmount( )
  {
    return getProperty( PROPERTY_TIMESTEP_AMOUNT, Integer.class );
  }

  private void setTimestepAmount( final Integer amount )
  {
    setProperty( PROPERTY_TIMESTEP_AMOUNT, amount );
  }

  private String getTimestepField( )
  {
    return getProperty( PROPERTY_TIMESTEP_FIELD, String.class );
  }

  private void setTimestepField( final String field )
  {
    setProperty( PROPERTY_TIMESTEP_FIELD, field );
  }

  @Override
  public Period getTimestep( )
  {
    final Integer amount = getTimestepAmount();
    final String fieldName = getTimestepField();

    if( amount == null || StringUtils.isBlank( fieldName ) )
      return null;

    final int field = CalendarUtilities.getCalendarField( fieldName );

    return PeriodUtils.getPeriod( field, amount );
  }

  @Override
  public void setTimestep( final Period period )
  {
    if( period == null )
    {
      setTimestepAmount( null );
      setTimestepField( null );
      return;
    }

    final int amount = PeriodUtils.findCalendarAmount( period );
    final FIELD field = PeriodUtils.findCalendarField( period );

    setTimestepAmount( amount );
    setTimestepField( field.name() );
  }

  @Override
  public ZmlLink getDataLink( )
  {
    return new ZmlLink( this, PROPERTY_DATA );
  }

// @Override
// public void setDataLink( final String href )
// {
// final TimeseriesLinkType link = new TimeseriesLinkType();
// link.setHref( href );
// setProperty( PROPERTY_DATA, link );
// }

  @Override
  public IStation getStation( )
  {
    return (IStation) getOwner();
  }

  @Override
  public void deleteDataFile( ) throws CoreException
  {
    final ZmlLink dataLink = getDataLink();
    final IFile file = dataLink.getFile();
    if( file != null && file.exists() )
    {
      /* Delete the data file. */
      file.delete( false, true, new NullProgressMonitor() );

      /* ZML import status file? so delete it, too. */
      final IContainer parent = file.getParent();
      final IFile statusFile = parent.getFile( new Path( file.getName() + ".status" ) ); //$NON-NLS-1$
      if( statusFile.exists() )
        statusFile.delete( false, new NullProgressMonitor() );
    }
  }

  @Override
  public Object getAdapter( final Class adapter )
  {
    if( adapter.isAssignableFrom( IZmlSourceElement.class ) )
    {
      return new TimeseriesSource( this );
    }
    else if( adapter.isAssignableFrom( TimeseriesLinkType.class ) )
    {
      final ZmlLink link = getDataLink();

      return link.getTimeseriesLink();
    }
    else if( adapter.isAssignableFrom( IObsProvider.class ) )
    {
      final ZmlLink link = getDataLink();
      final IObservation observation = link.getObservationFromPool();

      return new PlainObsProvider( observation, null );

    }
    else if( adapter.isAssignableFrom( IObservation.class ) )
    {
      final ZmlLink link = getDataLink();
      return link.getObservationFromPool();
    }

    return super.getAdapter( adapter );
  }

  @Override
  public Date getMeasurementStart( )
  {
    final Object date = getProperty( PROPERTY_MEASUREMENT_START );
    if( date instanceof XMLGregorianCalendar )
      return DateUtilities.toDate( date );

    /** fallback to supported already existing projects */
    final Date from = determineDateRangeByObservation().getFrom();
    setMeasurementStart( from );

    return from;
  }

  @Override
  public Date getMeasurementEnd( )
  {
    final Object date = getProperty( PROPERTY_MEASUREMENT_END );
    if( date instanceof XMLGregorianCalendar )
      return DateUtilities.toDate( date );

    /** fallback to supported already existing projects */
    final Date to = determineDateRangeByObservation().getTo();
    setMeasurementEnd( to );

    return to;
  }

  @Override
  public DateRange getDateRange( )
  {
    final Date start = getMeasurementStart();
    final Date end = getMeasurementEnd();

    if( Objects.allNotNull( start, end ) )
      return new DateRange( start, end );

    final DateRange daterange = determineDateRangeByObservation();
    setMeasurementStart( daterange.getFrom() );
    setMeasurementEnd( daterange.getTo() );

    return daterange;
  }

  /**
   * fallback to supported already existing projects
   */
  private DateRange determineDateRangeByObservation( )
  {
    final ZmlLink link = getDataLink();
    final IObservation observation = link.getObservationFromPool();

    return Observations.findDateRange( observation );
  }

  @Override
  public void setMeasurementStart( final Date date )
  {
    setProperty( PROPERTY_MEASUREMENT_START, DateUtilities.toXMLGregorianCalendar( date ) );
  }

  /**
   * @see org.kalypso.model.hydrology.timeseries.binding.ITimeseries#setMeasurementEnd(java.util.Date)
   */
  @Override
  public void setMeasurementEnd( final Date date )
  {
    setProperty( PROPERTY_MEASUREMENT_END, DateUtilities.toXMLGregorianCalendar( date ) );
  }
}