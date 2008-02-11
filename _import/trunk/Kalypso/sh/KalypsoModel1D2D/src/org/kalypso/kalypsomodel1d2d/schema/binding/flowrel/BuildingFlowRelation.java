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

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.schema.dict.Kalypso1D2DDictConstants;
import org.kalypso.model.wspm.core.gml.WspmProfile;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;

/**
 * @author Gernot Belger
 */
public abstract class BuildingFlowRelation extends AbstractFlowRelation1D implements IBuildingFlowRelation
{
  public BuildingFlowRelation( final Feature featureToBind, final QName qname )
  {
    super( featureToBind, qname );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBuildingFlowRelation#getKind()
   */
  public KIND getKind( )
  {
    final Integer kindInt = (Integer) getFeature().getProperty( QNAME_PROP_KIND );
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
    return (Feature) getFeature().getProperty( QNAME_PROP_OBSERVATION );
  }

  /**
   * Returns the building-observation.
   * <p>
   * If it does not exist yet or is not yet initialized, both is done.
   * </p>
   * 
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBuildingFlowRelation#getBuildingObservation()
   */
  public IObservation<TupleResult> getBuildingObservation( )
  {
    final Feature obsFeatureIfPresent = getObservationFeature();

    /* If observation does not exist, create it. */
    final Feature obsFeature;
    if( obsFeatureIfPresent == null )
    {
      final Feature feature = getFeature();
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
    final String[] componentUrns = new String[] { Kalypso1D2DDictConstants.DICT_COMPONENT_WATERLEVEL_UPSTREAM, Kalypso1D2DDictConstants.DICT_COMPONENT_WATERLEVEL_DOWNSTREAM,
        Kalypso1D2DDictConstants.DICT_COMPONENT_DISCHARGE };
    final IComponent[] components = new IComponent[componentUrns.length];

    for( int i = 0; i < components.length; i++ )
      components[i] = ObservationFeatureFactory.createDictionaryComponent( obsFeature, componentUrns[i] );

    for( final IComponent component : components )
      result.addComponent( component );

    ObservationFeatureFactory.toFeature( obs, obsFeature );
    return obs;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBuildingFlowRelation#setBuildingObservation(org.kalypso.observation.IObservation)
   */
  public void setBuildingObservation( final IObservation<TupleResult> observation )
  {
    final Feature obsFeature = getObservationFeature();
    ObservationFeatureFactory.toFeature( observation, obsFeature );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBuildingFlowRelation#getDirection()
   */
  public int getDirection( )
  {
    return ((BigInteger) getFeature().getProperty( QNAME_PROP_DIRECTION )).intValue();
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBuildingFlowRelation#setDirection(int)
   */
  public void setDirection( final int degrees )
  {
    getFeature().setProperty( QNAME_PROP_DIRECTION, BigInteger.valueOf( degrees ) );
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
  public BuildingParameters getBuildingParameters( )
  {
    return new BuildingParameters( getBuildingObservation() );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.ITeschkeFlowRelation#getProfile()
   */
  public WspmProfile getProfile( )
  {
    final Feature profileFeature = FeatureHelper.resolveLink( getFeature(), QNAME_PROP_PROFILE, true );
    return profileFeature == null ? null : new WspmProfile( profileFeature );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.ITeschkeFlowRelation#setProfileLink(java.lang.String)
   */
  public void setProfileLink( final String profileRef )
  {
    final Feature feature = getFeature();

    final IRelationType profileRelation = (IRelationType) feature.getFeatureType().getProperty( QNAME_PROP_PROFILE );
    final IFeatureType profileFT = profileRelation.getTargetFeatureType();
    final Feature profileLinkFeature = new XLinkedFeature_Impl( feature, profileRelation, profileFT, profileRef, "", "", "", "", "" );
    feature.setProperty( profileRelation, profileLinkFeature );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IFlowRelation1D#getStation()
   */
  public BigDecimal getStation( )
  {
    final WspmProfile profile = getProfile();
    if( profile == null )
      return null;

    return profile.getBigStation();
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IFlowRelation1D#setStation(java.math.BigDecimal)
   */
  public void setStation( final BigDecimal station )
  {
    final WspmProfile profile = getProfile();
    if( profile == null )
      return;

    profile.setBigStation( station );
  }

}
