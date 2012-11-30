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
import java.math.BigInteger;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.schema.dict.Kalypso1D2DDictConstants;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author Gernot Belger
 */
public abstract class BuildingFlowRelation extends AbstractFlowRelation1D implements IBuildingFlowRelation
{

  public BuildingFlowRelation( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBuildingFlowRelation#getKind()
   */
  @Override
  public KIND getKind( )
  {
    final Integer kindInt = (Integer) getProperty( QNAME_PROP_KIND );
    if( kindInt == null )
      return null;

    switch( kindInt )
    {
      case 1:
      case 2:
      case 3:
      case 4:
      case 5:
      case 6:
      case 7:
      case 8:
      case 9:
        // Kinds 1-9 are not supported
        throw new UnsupportedOperationException();

      case 10:
        return KIND.TABULAR;

      default:
        throw new IllegalStateException();
    }
  }

  private Feature getObservationFeature( )
  {
    return (Feature) getProperty( QNAME_PROP_OBSERVATION );
  }

  /**
   * Returns the building-observation.
   * <p>
   * If it does not exist yet or is not yet initialized, both is done.
   * </p>
   *
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBuildingFlowRelation#getBuildingObservation()
   */
  @Override
  public IObservation<TupleResult> getBuildingObservation( )
  {
    final Feature obsFeatureIfPresent = getObservationFeature();

    /* If observation does not exist, create it. */
    final Feature obsFeature;
    if( obsFeatureIfPresent == null )
    {
      final Feature feature = this;
      final GMLWorkspace workspace = feature.getWorkspace();
      final IRelationType parentRelation = (IRelationType) feature.getFeatureType().getProperty( QNAME_PROP_OBSERVATION );
      obsFeature = workspace.createFeature( feature, parentRelation, parentRelation.getTargetFeatureType(), -1 );
      feature.setProperty( QNAME_PROP_OBSERVATION, obsFeature );
    }
    else
      obsFeature = getObservationFeature();

    /* Create an observation from it. */
    final IObservation<TupleResult> obs = ObservationFeatureFactory.toObservation( obsFeature );
    final TupleResult result = obs.getResult();
    if( result.getComponents().length > 0 )
      return obs;

    /* If not yet initialized, create components and write obs back to feature. */
    final String[] componentUrns = new String[] { Kalypso1D2DDictConstants.DICT_COMPONENT_DISCHARGE, Kalypso1D2DDictConstants.DICT_COMPONENT_WATERLEVEL_DOWNSTREAM,
        Kalypso1D2DDictConstants.DICT_COMPONENT_WATERLEVEL_UPSTREAM };
    final IComponent[] components = new IComponent[componentUrns.length];

    for( int i = 0; i < components.length; i++ )
      components[i] = ObservationFeatureFactory.createDictionaryComponent( obsFeature, componentUrns[i] );

    for( final IComponent component : components )
      result.addComponent( component );

    // Sort by discharge and then downstream waterlevel
    result.setSortComponents( new IComponent[] { components[0], components[1] } );

    ObservationFeatureFactory.toFeature( obs, obsFeature );
    return obs;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBuildingFlowRelation#setBuildingObservation(org.kalypso.observation.IObservation)
   */
  @Override
  public void setBuildingObservation( final IObservation<TupleResult> observation )
  {
    final Feature obsFeature = getObservationFeature();
    ObservationFeatureFactory.toFeature( observation, obsFeature );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBuildingFlowRelation#getDirection()
   */
  @Override
  public int getDirection( )
  {
    return ((BigInteger) getProperty( QNAME_PROP_DIRECTION )).intValue();
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBuildingFlowRelation#setDirection(int)
   */
  @Override
  public void setDirection( final int degrees )
  {
    setProperty( QNAME_PROP_DIRECTION, BigInteger.valueOf( degrees ) );
  }

  /**
   * Returns the building values (hOW, hUW, discharge) as some kind of table.
   * <p>
   * The building parameters are NOT backed by the underlying featre, so changed to the feature are not refelcted in the
   * building parameters.
   * </p>
   *
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBuildingFlowRelation#getBuildingParameters()
   */
  @Override
  public BuildingParameters getBuildingParameters( )
  {
    return new BuildingParameters( getBuildingObservation() );
  }

  @Override
  public IProfileFeature getProfile( )
  {
    final IProfileFeature profileFeature = (IProfileFeature) FeatureHelper.resolveLink( this, QNAME_PROP_PROFILE, true );
    return profileFeature;
  }

  @Override
  public void setProfileLink( final String profileRef )
  {
    setLink( QNAME_PROP_PROFILE, profileRef );
  }

  @Override
  public BigDecimal getStation( )
  {
    final IProfileFeature profile = getProfile();
    if( profile == null )
      return null;

    return profile.getBigStation();
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IFlowRelation1D#setStation(java.math.BigDecimal)
   */
  @Override
  public void setStation( final BigDecimal station )
  {
    final IProfileFeature profile = getProfile();
    if( profile == null )
      return;

    profile.setBigStation( station );
  }

}
