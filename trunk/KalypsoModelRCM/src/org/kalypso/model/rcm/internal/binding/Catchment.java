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

import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.rcm.RcmConstants;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;

/**
 * The catchment.
 * 
 * @author Holger Albert
 */
public class Catchment extends Feature_Impl
{
  /**
   * The qname of the area link.
   */
  public static final QName QNAME_AREA_LINK = new QName( RcmConstants.NS_CM, "areaLink" );

  /**
   * The qname of the factorized timeseries member.
   */
  public static final QName QNAME_FACTORIZED_TIMESERIES_MEMBER = new QName( RcmConstants.NS_CM, "factorizedTimeseriesMember" );

  /**
   * The qname of the factorized timeseries.
   */
  public static final QName QNAME_FACTORIZED_TIMESERIES = new QName( RcmConstants.NS_CM, "FactorizedTimeseries" );

  /**
   * The constructor.
   * 
   * @param parent
   * @param parentRelation
   * @param ft
   * @param id
   * @param propValues
   */
  public Catchment( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  /**
   * This function returns the area link.
   * 
   * @return The area link.
   */
  public Feature getAreaLink( )
  {
    /* The xlinked feature. */
    final XLinkedFeature_Impl xlink = (XLinkedFeature_Impl) getProperty( QNAME_AREA_LINK );

    /* Resolve the feature. */
    return FeatureHelper.resolveLinkedFeature( null, xlink );
  }

  /**
   * This function returns factorized timeseries.
   * 
   * @return All factorized timeseries.
   */
  public FactorizedTimeseries[] getFactorizedTimeseries( )
  {
    /* Memory for the results. */
    final List<FactorizedTimeseries> results = new ArrayList<FactorizedTimeseries>();

    /* Get all factorized timeseries. */
    final FeatureList factorizedTimeseries = (FeatureList) getProperty( QNAME_FACTORIZED_TIMESERIES_MEMBER );
    for( int i = 0; i < factorizedTimeseries.size(); i++ )
      results.add( (FactorizedTimeseries) factorizedTimeseries.get( i ) );

    return results.toArray( new FactorizedTimeseries[] {} );
  }
}