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
package org.kalypso.model.wspm.tuhh.schema.gml;

import java.math.BigDecimal;
import java.util.List;

import javax.xml.namespace.QName;

import org.apache.commons.lang3.ArrayUtils;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.IProfileBuilding;
import org.kalypso.model.wspm.tuhh.schema.schemata.IWspmTuhhQIntervallConstants;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.Observation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.gml.binding.math.IPolynomial1D;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

/**
 * @author Gernot Belger
 */
public class QIntervallResult extends Feature_Impl
{
  public static final QName QNAME_F_QIntervallResult = new QName( IWspmTuhhConstants.NS_WSPM_TUHH, "QIntervallResult" ); //$NON-NLS-1$

  public static final QName QNAME_P_QIntervallResult_station = new QName( IWspmTuhhConstants.NS_WSPM_TUHH, "station" ); //$NON-NLS-1$

  public static final QName QNAME_P_QIntervallResult_slope = new QName( IWspmTuhhConstants.NS_WSPM_TUHH, "slope" ); //$NON-NLS-1$

  public static final QName QNAME_P_QIntervallResult_bankfull = new QName( IWspmTuhhConstants.NS_WSPM_TUHH, "bankfullHeight" ); //$NON-NLS-1$

  private static final QName QNAME_P_QIntervallResult_buildingId = new QName( IWspmTuhhConstants.NS_WSPM_TUHH, "buildingId" ); //$NON-NLS-1$

  public static final QName QNAME_P_QIntervallResult_pointsMember = new QName( IWspmTuhhConstants.NS_WSPM_TUHH, "pointsMember" ); //$NON-NLS-1$

  public static final QName QNAME_P_QIntervallResult_buildingMember = new QName( IWspmTuhhConstants.NS_WSPM_TUHH, "buildingParameterMember" ); //$NON-NLS-1$

  public static final QName QNAME_P_QIntervallResult_polynomialMember = new QName( IWspmTuhhConstants.NS_WSPM_TUHH, "polynomialMember" ); //$NON-NLS-1$

  public static final QName QNAME_P_QIntervallResult_profileMember = new QName( IWspmTuhhConstants.NS_WSPM_TUHH, "profileMember" ); //$NON-NLS-1$

  public static final QName QNAME_F_WPointsObservation = new QName( IWspmTuhhConstants.NS_WSPM_TUHH, "WPointsObservation" ); //$NON-NLS-1$

  public static final QName QNAME_F_BuildingObservation = IObservation.QNAME_OBSERVATION;

  public QIntervallResult( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  public void setStation( final BigDecimal station )
  {
    setProperty( QNAME_P_QIntervallResult_station, station );
  }

  public void setSlope( final BigDecimal slope )
  {
    setProperty( QNAME_P_QIntervallResult_slope, slope );
  }

  /**
   * Creates the points-observation for the polynome case.<br/>
   * If the observation is changed afterwards, {@link #setPointsObservation(IObservation) has to be called.
   */
  public IObservation<TupleResult> getOrCreatePointsObservation( )
  {
    final Feature propertyValue = getProperty( QNAME_P_QIntervallResult_pointsMember, Feature.class );
    if( propertyValue != null )
      return ObservationFeatureFactory.toObservation( propertyValue );

    final GMLWorkspace workspace = getWorkspace();
    final IFeatureType ftQIntervallResult = GMLSchemaUtilities.getFeatureTypeQuiet( QIntervallResult.QNAME_F_QIntervallResult );
    final IFeatureType ftObservation = GMLSchemaUtilities.getFeatureTypeQuiet( QNAME_F_WPointsObservation );
    final IRelationType pointsObsRelation = (IRelationType) ftQIntervallResult.getProperty( QNAME_P_QIntervallResult_pointsMember );

    final Feature obsFeature = workspace.createFeature( this, pointsObsRelation, ftObservation );
    setProperty( QNAME_P_QIntervallResult_pointsMember, obsFeature );

    final TupleResult tupleResult = new TupleResult();

    return new Observation<>( "", "", tupleResult ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  public void setPointsObservation( final IObservation<TupleResult> observation )
  {
    final Feature obsFeature = getProperty( QNAME_P_QIntervallResult_pointsMember, Feature.class );

    ObservationFeatureFactory.toFeature( observation, obsFeature );
  }

  /**
   * Sets a link to the given profile into the modell-gml which must reside at project:/modell.gml.
   * <p>
   * Also sets the building-phenomenon
   * </p>
   */
  public void setProfileLink( final IProfileFeature profile )
  {
    final IFeatureType ftProfile = GMLSchemaUtilities.getFeatureTypeQuiet( IProfileFeature.FEATURE_PROFILE );

    final String href = "project:/modell.gml#" + profile.getId(); //$NON-NLS-1$
    setLink( QNAME_P_QIntervallResult_profileMember, href, ftProfile );

    final IProfileObject[] buildings = profile.getProfile().getProfileObjects( IProfileBuilding.class );
    if( ArrayUtils.isEmpty( buildings ) )
      return;

    final IProfileObject building = buildings[0];
    setProperty( QNAME_P_QIntervallResult_buildingId, building.getType() );
  }

  public String getBuildingId( )
  {
    return getProperty( QNAME_P_QIntervallResult_buildingId, String.class );
  }

  /** Creates and sets the polynomial. */
  public IPolynomial1D createPolynomial( ) throws Exception
  {
    final GMLWorkspace workspace = getWorkspace();

    final IFeatureType resultFT = GMLSchemaUtilities.getFeatureTypeQuiet( QNAME_F_QIntervallResult );
    final IRelationType polynomialRelation = (IRelationType) resultFT.getProperty( QNAME_P_QIntervallResult_polynomialMember );

    final IFeatureType polynomialFT = GMLSchemaUtilities.getFeatureTypeQuiet( IPolynomial1D.QNAME );

    final Feature polynomialFeature = workspace.createFeature( this, polynomialRelation, polynomialFT );
    workspace.addFeatureAsComposition( this, polynomialRelation, -1, polynomialFeature );
    return (IPolynomial1D) polynomialFeature;
  }

  /**
   * Returns the weir-observation.
   * <p>
   * If it does not yet exists, it is created and initialized (if <code>createIfEmpty</code> is <code>true</code>).
   * </p>
   * <p>
   * If the observation is changed afterwards, {@link #setWeirObservation(IObservation) has to be called.
   */
  public IObservation<TupleResult> getBuildingObservation( final boolean createIfEmpty )
  {
    final Object propertyValue = getProperty( QNAME_P_QIntervallResult_buildingMember );
    if( propertyValue != null )
      return ObservationFeatureFactory.toObservation( (Feature) propertyValue );

    if( !createIfEmpty )
      return null;

    final GMLWorkspace workspace = getWorkspace();
    final IFeatureType ftQIntervallResult = GMLSchemaUtilities.getFeatureTypeQuiet( QIntervallResult.QNAME_F_QIntervallResult );
    final IFeatureType ftObservation = GMLSchemaUtilities.getFeatureTypeQuiet( QNAME_F_BuildingObservation );
    final IRelationType pointsObsRelation = (IRelationType) ftQIntervallResult.getProperty( QNAME_P_QIntervallResult_buildingMember );

    final Feature obsFeature = workspace.createFeature( this, pointsObsRelation, ftObservation );
    setProperty( QNAME_P_QIntervallResult_buildingMember, obsFeature );

    final IComponent[] pointsComponents = new IComponent[3];
    pointsComponents[0] = ObservationFeatureFactory.createDictionaryComponent( obsFeature, IWspmTuhhQIntervallConstants.DICT_COMPONENT_RUNOFF );
    pointsComponents[1] = ObservationFeatureFactory.createDictionaryComponent( obsFeature, IWspmTuhhQIntervallConstants.DICT_COMPONENT_WATERLEVEL_DOWNSTREAM );
    pointsComponents[2] = ObservationFeatureFactory.createDictionaryComponent( obsFeature, IWspmTuhhQIntervallConstants.DICT_COMPONENT_WATERLEVEL_UPSTREAM );

    final TupleResult tupleResult = new TupleResult( pointsComponents );

    // Sort by discharge and then downstream waterlevel
    tupleResult.setSortComponents( new IComponent[] { pointsComponents[0], pointsComponents[1] } );

    return new Observation<>( "", "", tupleResult ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  public void setBuildingObservation( final IObservation<TupleResult> observation )
  {
    final Feature obsFeature = getProperty( QNAME_P_QIntervallResult_buildingMember, Feature.class );

    ObservationFeatureFactory.toFeature( observation, obsFeature );
  }

  public BigDecimal getSlope( )
  {
    return getProperty( QNAME_P_QIntervallResult_slope, BigDecimal.class );
  }

  public BigDecimal getStation( )
  {
    return getProperty( QNAME_P_QIntervallResult_station, BigDecimal.class );
  }

  public BigDecimal getBankfull( )
  {
    return getProperty( QNAME_P_QIntervallResult_bankfull, BigDecimal.class );
  }

  public void setBankfull( final BigDecimal bankfull )
  {
    setProperty( QNAME_P_QIntervallResult_bankfull, bankfull );
  }

  public List< ? > getPolynomialFeatures( )
  {
    return getProperty( QNAME_P_QIntervallResult_polynomialMember, List.class );
  }

  /** Create a component for the points-observation. */
  public IComponent createPointsComponent( final String componentID )
  {
    final Feature obsFeature = getProperty( QNAME_P_QIntervallResult_pointsMember, Feature.class );
    return ObservationFeatureFactory.createDictionaryComponent( obsFeature, componentID );
  }
}