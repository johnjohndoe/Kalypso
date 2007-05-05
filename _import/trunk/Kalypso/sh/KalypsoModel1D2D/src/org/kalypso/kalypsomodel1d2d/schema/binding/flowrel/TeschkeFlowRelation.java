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
package org.kalypso.kalypsomodel1d2d.schema.binding.flowrel;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import org.kalypso.kalypsosimulationmodel.core.flowrel.FlowRelationship;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.gml.binding.math.IPolynomial1D;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author Gernot Belger
 */
public class TeschkeFlowRelation extends FlowRelationship implements ITeschkeFlowRelation
{
  public TeschkeFlowRelation( final Feature featureToBind )
  {
    super( featureToBind, ITeschkeFlowRelation.QNAME );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.ITeschkeFlowRelation#getStation()
   */
  public BigDecimal getStation( )
  {
    return (BigDecimal) getFeature().getProperty( QNAME_PROP_STATION );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.ITeschkeFlowRelation#setStation(java.math.BigDecimal)
   */
  public void setStation( final BigDecimal station )
  {
    getFeature().setProperty( QNAME_PROP_STATION, station );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.ITeschkeFlowRelation#getPolynomials()
   */
  public IPolynomial1D[] getPolynomials( )
  {
    final Feature feature = getFeature();
    final GMLWorkspace workspace = feature.getWorkspace();
    final FeatureList polynomeFeatures = (FeatureList) feature.getProperty( QNAME_PROP_POLYNOMES );

    final List<IPolynomial1D> resultList = new ArrayList<IPolynomial1D>( polynomeFeatures.size() );

    for( final Object object : polynomeFeatures )
    {
      final Feature polyFeature = FeatureHelper.getFeature( workspace, object );
      resultList.add( (IPolynomial1D) polyFeature.getAdapter( IPolynomial1D.class ) );
    }

    return resultList.toArray( new IPolynomial1D[resultList.size()] );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.ITeschkeFlowRelation#getSlope()
   */
  public double getSlope( )
  {
    final BigDecimal slope = (BigDecimal) getFeature().getProperty( QNAME_PROP_SLOPE );
    return slope.doubleValue();
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.ITeschkeFlowRelation#setSlope(double)
   */
  public void setSlope( final double slope )
  {
    final BigDecimal slopeDec = new BigDecimal( slope ).setScale( 5, BigDecimal.ROUND_HALF_UP );
    getFeature().setProperty( QNAME_PROP_SLOPE, slopeDec );
  }

}
