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
package org.kalypso.ui.rrm.internal.timeseries.binding;

import java.net.URISyntaxException;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.URIUtil;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.FeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

/**
 * @author Gernot Belger
 */
public abstract class Station extends Feature_Impl
{
  final static QName FEATURE_STATION = new QName( NaModelConstants.NS_TIMESERIES_MANAGEMENT, "Station" ); //$NON-NLS-1$

  public static final QName MEMBER_TIMESERIES = new QName( NaModelConstants.NS_TIMESERIES_MANAGEMENT, "timseriesMember" ); //$NON-NLS-1$

  public static final QName PROPERTY_COMMENT = new QName( NaModelConstants.NS_TIMESERIES_MANAGEMENT, "comment" ); //$NON-NLS-1$

  private final IFeatureBindingCollection<Timeseries> m_timeseries = new FeatureBindingCollection<Timeseries>( this, Timeseries.class, MEMBER_TIMESERIES );

  public Station( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  public IFeatureBindingCollection<Timeseries> getTimeseries( )
  {
    return m_timeseries;
  }

  public String getComment( )
  {
    return getProperty( PROPERTY_COMMENT, String.class );
  }

  public String getTimeseriesFoldername( )
  {
    try
    {
      final String dirtyFoldername = String.format( "%s_%s", getName(), getDescription() );
      return URIUtil.fromString( dirtyFoldername ).toASCIIString();
    }
    catch( final URISyntaxException e )
    {
      // can this ever happen?
      e.printStackTrace();
      throw new RuntimeException( e );
    }
  }
}