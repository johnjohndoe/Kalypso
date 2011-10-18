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

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsosimulationmodel.core.flowrel.FlowRelationship;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author Gernot Belger, ig
 */
public abstract class AbstractFlowRelation2D extends FlowRelationship implements IFlowRelation2D
{
  public AbstractFlowRelation2D( final Feature featureToBind, final QName qname )
  {
    super( featureToBind, qname );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IFlowRelation1D#getCalculation()
   */
  @Override
  public TuhhCalculation getCalculation( )
  {
    final Feature calcFeature = getProperty( QNAME_PROP_TUHH_CALCULATION, Feature.class );
    if( calcFeature instanceof TuhhCalculation )
      return (TuhhCalculation) calcFeature;

    return null;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IFlowRelation1D#setCalculation(org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation)
   */
  @Override
  public void setCalculation( final TuhhCalculation calculation )
  {
    final Feature flowRelFeature = getFeature();
    final GMLWorkspace flowRelworkspace = flowRelFeature.getWorkspace();
    final IFeatureType flowRelFT = flowRelworkspace.getGMLSchema().getFeatureType( IFlowRelation2D.QNAME );
    final IRelationType calcRT = (IRelationType) flowRelFT.getProperty( IFlowRelation2D.QNAME_PROP_TUHH_CALCULATION );
    try
    {
      FeatureHelper.cloneFeature( flowRelFeature, calcRT, calculation );
    }
    catch( final Exception e )
    {
      // should never happen
      e.printStackTrace();
    }
  }
}
