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
package org.kalypso.model.hydrology.binding.timeseriesMappings;

import java.util.Date;

import javax.xml.namespace.QName;

import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * Binding class for tmrrm:TimeseriesMapping
 * 
 * @author Gernot Belger
 */
public interface ITimeseriesMapping extends Feature
{
  QName FEATURE_TIMESERIES_MAPPING = new QName( NaModelConstants.NS_TIMESERIES_MAPPING, "TimeseriesMapping" ); //$NON-NLS-1$

  QName PROPERTY_COMMENT = new QName( NaModelConstants.NS_TIMESERIES_MAPPING, "comment" ); //$NON-NLS-1$

  QName PROPERTY_TYPE = new QName( NaModelConstants.NS_TIMESERIES_MAPPING, "type" ); //$NON-NLS-1$

  QName PROPERTY_LAST_MODIFIED = new QName( NaModelConstants.NS_TIMESERIES_MAPPING, "lastModified" ); //$NON-NLS-1$

  QName MEMBER_MAPPING = new QName( NaModelConstants.NS_TIMESERIES_MAPPING, "mappingMember" ); //$NON-NLS-1$

  IFeatureBindingCollection<IMappingElement> getMappings( );

  String getComment( );

  void setComment( String comment );

  TimeseriesMappingType getType( );

  void setType( TimeseriesMappingType type );

  Date getLastModified( );

  void setLastModified( Date date );
}