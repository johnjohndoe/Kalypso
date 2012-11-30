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
package org.kalypso.model.hydrology.internal.binding.timeseriesMappings;

import java.util.Date;

import org.apache.commons.lang3.StringUtils;
import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.binding.timeseriesMappings.IMappingElement;
import org.kalypso.model.hydrology.binding.timeseriesMappings.ITimeseriesMapping;
import org.kalypso.model.hydrology.binding.timeseriesMappings.TimeseriesMappingType;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.FeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

/**
 * Binding class for tmrrm:TimeseriesMapping
 *
 * @author Gernot Belger
 */
public class TimeseriesMapping extends Feature_Impl implements ITimeseriesMapping
{
  private final IFeatureBindingCollection<IMappingElement> m_mappingMembers;

  public TimeseriesMapping( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );

    m_mappingMembers = new FeatureBindingCollection<>( this, IMappingElement.class, MEMBER_MAPPING );
  }

  @Override
  public IFeatureBindingCollection<IMappingElement> getMappings( )
  {
    return m_mappingMembers;
  }

  @Override
  public String getComment( )
  {
    return getStringProperty( PROPERTY_COMMENT, StringUtils.EMPTY );
  }

  @Override
  public TimeseriesMappingType getType( )
  {
    return getEnumProperty( PROPERTY_TYPE, TimeseriesMappingType.class, TimeseriesMappingType.gaugeMeasurement );
  }

  @Override
  public Date getLastModified( )
  {
    return DateUtilities.toDate( getProperty( PROPERTY_LAST_MODIFIED ) );
  }

  @Override
  public void setLastModified( final Date lastModified )
  {
    setProperty( PROPERTY_LAST_MODIFIED, DateUtilities.toXMLGregorianCalendar( lastModified ) );
  }

  @Override
  public void setComment( final String comment )
  {
    setProperty( PROPERTY_COMMENT, comment );
  }

  @Override
  public void setType( final TimeseriesMappingType type )
  {
    setProperty( PROPERTY_TYPE, type.name() );
  }
}