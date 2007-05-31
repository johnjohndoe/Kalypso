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

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.kalypsomodel1d2d.schema.dict.Kalypso1D2DDictConstants;
import org.kalypso.kalypsosimulationmodel.core.flowrel.FlowRelationship;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author Gernot Belger
 */
public class BoundaryCondition extends FlowRelationship implements IBoundaryCondition
{
  public BoundaryCondition( final Feature featureToBind )
  {
    super( featureToBind, IBoundaryCondition.QNAME );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition#getTimeserie1DFeature()
   */
  private Feature getTimeserieFeature( )
  {
    return (Feature) getWrappedFeature().getProperty( QNAME_P_TIMESERIE );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition#initializeObservation(java.lang.String,
   *      java.lang.String)
   */
  public IObservation<TupleResult> initializeObservation( final String domainComponentUrn, final String valueComponentUrn )
  {
    final Feature currentObsFeature = getTimeserieFeature();

    /* If we have a discharge-timeserie, we have a directed timeserie. */
    final Feature obsFeature;
    if( domainComponentUrn.equals( Kalypso1D2DDictConstants.DICT_COMPONENT_TIME ) && valueComponentUrn.equals( Kalypso1D2DDictConstants.DICT_COMPONENT_DISCHARGE ) )
    {
      final Feature parentFeature = currentObsFeature.getParent();
      final GMLWorkspace workspace = parentFeature.getWorkspace();
      final IFeatureType directedFT = workspace.getGMLSchema().getFeatureType( QNAME_DIRECTED_TIMESERIE );
      final Feature newObsFeature = workspace.createFeature( parentFeature, currentObsFeature.getParentRelation(), directedFT );
      parentFeature.setProperty( QNAME_P_TIMESERIE, newObsFeature );
      newObsFeature.setProperty( QNAME_P_DIRECTION, 180 );
      obsFeature = newObsFeature;
    }
    else
      obsFeature = currentObsFeature;

    final String[] componentUrns = new String[] { domainComponentUrn, valueComponentUrn };
    final IComponent[] components = new IComponent[componentUrns.length];

    for( int i = 0; i < components.length; i++ )
      components[i] = ObservationFeatureFactory.createDictionaryComponent( obsFeature, componentUrns[i] );

    final IObservation<TupleResult> obs = ObservationFeatureFactory.toObservation( obsFeature );

    final TupleResult result = obs.getResult();
    for( final IComponent component : components )
      result.addComponent( component );

    return obs;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition#setObservation(org.kalypso.observation.IObservation)
   */
  public void setObservation( final IObservation<TupleResult> obs )
  {
    ObservationFeatureFactory.toFeature( obs, getTimeserieFeature() );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition#getObservation()
   */
  public IObservation<TupleResult> getObservation( )
  {
    return ObservationFeatureFactory.toObservation( getTimeserieFeature() );
  }

}
