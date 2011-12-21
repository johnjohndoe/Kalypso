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
package org.kalypso.model.rcm.internal.binding;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.rcm.binding.ICatchment;
import org.kalypso.model.rcm.binding.IFactorizedTimeseries;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.FeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;

/**
 * The catchment.
 *
 * @author Holger Albert
 */
public class Catchment extends Feature_Impl implements ICatchment
{
  private final IFeatureBindingCollection<IFactorizedTimeseries> m_timeseries;

  public Catchment( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );

    m_timeseries = new FeatureBindingCollection<IFactorizedTimeseries>( this, IFactorizedTimeseries.class, MEMBER_FACTORIZED_TIMESERIES );
  }

  @Override
  public Feature getAreaLink( )
  {
    return getProperty( PROPERTY_AREA_LINK, Feature.class );
  }

  @Override
  public void setAreaLink( final String href )
  {
    final IFeatureType featureType = getFeatureType();

    final IRelationType linkProperty = (IRelationType) featureType.getProperty( PROPERTY_AREA_LINK );
    final IFeatureType areaLinkFeatureType = linkProperty.getTargetFeatureType();

    final XLinkedFeature_Impl link = new XLinkedFeature_Impl( this, linkProperty, areaLinkFeatureType, href );
    setProperty( PROPERTY_AREA_LINK, link );
  }


  @Override
  public IFeatureBindingCollection<IFactorizedTimeseries> getFactorizedTimeseries( )
  {
    return m_timeseries;
  }
}