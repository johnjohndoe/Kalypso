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
package org.kalypso.model.rcm.binding;

import java.util.Date;
import java.util.List;

import javax.xml.datatype.XMLGregorianCalendar;

import org.apache.commons.lang3.StringUtils;
import org.kalypso.commons.tokenreplace.IStringResolver;
import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.zml.core.filter.binding.IZmlFilter;
import org.kalypso.zml.core.filter.binding.InterpolationZmlFilter;
import org.kalypso.zml.core.filter.binding.IntervalZmlFilter;
import org.kalypsodeegree_impl.model.feature.FeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

/**
 * The base implementation of the rainfall generator.
 *
 * @author Gernot Belger
 */
public abstract class AbstractRainfallGenerator extends Feature_Impl implements IRainfallGenerator
{
  /**
   * Filters, which will be applied to the source timeseries.
   */
  private final FeatureBindingCollection<IZmlFilter> m_filters;

  /**
   * The constructor.
   *
   * @param parent
   *          The parent.
   * @param parentRelation
   *          The parent relation.
   * @param ft
   *          The feature type.
   * @param id
   *          The id.
   * @param propValues
   *          The property values.
   */
  protected AbstractRainfallGenerator( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );

    m_filters = new FeatureBindingCollection<>( this, IZmlFilter.class, MEMBER_FILTER, true );
  }

  @Override
  public IDateRange getPeriod( )
  {
    return getProperty( PROPERTY_PERIOD, IDateRange.class );
  }

  @Override
  public DateRange getPeriod( final IStringResolver variables )
  {
    final IDateRange property = getProperty( PROPERTY_PERIOD, IDateRange.class );
    if( property == null )
      return null;

    return property.asDateRange( variables );
  }

  @Override
  public void setPeriod( final DateRange period )
  {
    final IRelationType relation = (IRelationType) getFeatureType().getProperty( PROPERTY_PERIOD );
    final IFeatureType type = GMLSchemaUtilities.getFeatureTypeQuiet( IDateRange.FEATURE_DATE_RANGE );
    final org.kalypso.model.rcm.internal.binding.DateRange periodFeature = (org.kalypso.model.rcm.internal.binding.DateRange) getWorkspace().createFeature( this, relation, type );

    periodFeature.setFrom( period.getFrom() );
    periodFeature.setTo( period.getTo() );

    setProperty( PROPERTY_PERIOD, periodFeature );
  }

  @Override
  public String[] getModels( )
  {
    final String property = getProperty( PROPERTY_MODEL, String.class );
    if( StringUtils.isBlank( property ) )
      return new String[] {};

    return StringUtils.split( property, ";" ); //$NON-NLS-1$
  }

  @Override
  public Date getValidFrom( )
  {
    /* Get the property. */
    final XMLGregorianCalendar validFrom = getProperty( PROPERTY_VALID_FROM, XMLGregorianCalendar.class );

    return DateUtilities.toDate( validFrom );
  }

  @Override
  public void setValidFrom( final Date validFrom )
  {
    setProperty( PROPERTY_VALID_FROM, DateUtilities.toXMLGregorianCalendar( validFrom ) );
  }

  @Override
  public Date getValidTo( )
  {
    /* Get the property. */
    final XMLGregorianCalendar validTo = getProperty( PROPERTY_VALID_TO, XMLGregorianCalendar.class );

    return DateUtilities.toDate( validTo );
  }

  @Override
  public void setValidTo( final Date validTo )
  {
    setProperty( PROPERTY_VALID_TO, DateUtilities.toXMLGregorianCalendar( validTo ) );
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

  @Override
  public Date getLastModified( )
  {
    final XMLGregorianCalendar lastModified = getProperty( PROPERTY_LAST_MODIFIED, XMLGregorianCalendar.class );

    return DateUtilities.toDate( lastModified );
  }

  @Override
  public void setLastModified( final Date lastModified )
  {
    setProperty( PROPERTY_LAST_MODIFIED, DateUtilities.toXMLGregorianCalendar( lastModified ) );
  }

  @Override
  public List<IZmlFilter> getFilters( )
  {
    return m_filters;
  }

  @Override
  public void addFilter( final IZmlFilter filter )
  {
    m_filters.add( filter );
  }

  @Override
  public void addInterpolationFilter( final String calendarField, final int amount, final boolean forceFill, final String defaultValue, final int defaultStatus )
  {
    final InterpolationZmlFilter filter = (InterpolationZmlFilter) m_filters.addNew( InterpolationZmlFilter.QNAME_INTERPOLATION_ZML_FILTER );
    filter.setProperty( InterpolationZmlFilter.QNAME_CALENDAR_FIELD, calendarField );
    filter.setProperty( InterpolationZmlFilter.QNAME_CALENDAR_AMOUNT, new Integer( amount ) );
    filter.setProperty( InterpolationZmlFilter.QNAME_FORCE_FILL, new Boolean( forceFill ) );
    filter.setProperty( InterpolationZmlFilter.QNAME_DEFAULT_VALUE, defaultValue );
    filter.setProperty( InterpolationZmlFilter.QNAME_DEFAULT_STATUS, new Integer( defaultStatus ) );
  }

  @Override
  public void addIntervalFilter( final String calendarField, final int amount, final double defaultValue, final int defaultStatus )
  {
    final IntervalZmlFilter filter = (IntervalZmlFilter) m_filters.addNew( IntervalZmlFilter.FEATURE_INTERVAL_ZML_FILTER );
    filter.setProperty( IntervalZmlFilter.PROPERTY_CALENDAR_FIELD, calendarField );
    filter.setProperty( IntervalZmlFilter.PROPERTY_CALENDAR_AMOUNT, new Integer( amount ) );
    filter.setProperty( IntervalZmlFilter.PROPERTY_DEFAULT_VALUE, new Double( defaultValue ) );
    filter.setProperty( IntervalZmlFilter.PROPERTY_DEFAULT_STATUS, new Integer( defaultStatus ) );
  }
}