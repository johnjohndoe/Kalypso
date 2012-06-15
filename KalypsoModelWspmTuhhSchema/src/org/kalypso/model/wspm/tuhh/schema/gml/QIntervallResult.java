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

import org.apache.commons.lang.ArrayUtils;
import org.kalypso.commons.xml.NS;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.IProfileBuilding;
import org.kalypso.model.wspm.tuhh.schema.schemata.IWspmTuhhQIntervallConstants;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.Observation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.gml.binding.commons.AbstractFeatureBinder;
import org.kalypsodeegree_impl.gml.binding.math.IPolynomial1D;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;

/**
 * @author Gernot Belger
 */
public class QIntervallResult extends AbstractFeatureBinder
{
  public static final QName QNAME_F_QIntervallResult = new QName( IWspmTuhhConstants.NS_WSPM_TUHH, "QIntervallResult" ); //$NON-NLS-1$

  public static final QName QNAME_P_QIntervallResult_station = new QName( IWspmTuhhConstants.NS_WSPM_TUHH, "station" ); //$NON-NLS-1$

  public static final QName QNAME_P_QIntervallResult_slope = new QName( IWspmTuhhConstants.NS_WSPM_TUHH, "slope" ); //$NON-NLS-1$

  private static final QName QNAME_P_QIntervallResult_buildingId = new QName( IWspmTuhhConstants.NS_WSPM_TUHH, "buildingId" ); //$NON-NLS-1$

  public static final QName QNAME_P_QIntervallResult_pointsMember = new QName( IWspmTuhhConstants.NS_WSPM_TUHH, "pointsMember" ); //$NON-NLS-1$

  public static final QName QNAME_P_QIntervallResult_buildingMember = new QName( IWspmTuhhConstants.NS_WSPM_TUHH, "buildingParameterMember" ); //$NON-NLS-1$

  public static final QName QNAME_P_QIntervallResult_polynomialMember = new QName( IWspmTuhhConstants.NS_WSPM_TUHH, "polynomialMember" ); //$NON-NLS-1$

  public static final QName QNAME_P_QIntervallResult_profileMember = new QName( IWspmTuhhConstants.NS_WSPM_TUHH, "profileMember" ); //$NON-NLS-1$

  public static final QName QNAME_F_WPointsObservation = new QName( IWspmTuhhConstants.NS_WSPM_TUHH, "WPointsObservation" ); //$NON-NLS-1$

  public static final QName QNAME_F_BuildingObservation = new QName( NS.OM, "Observation" ); //$NON-NLS-1$

  public QIntervallResult( final Feature featureToBind )
  {
    super( featureToBind, QNAME_F_QIntervallResult );
  }

  public void setStation( final BigDecimal station )
  {
    getFeature().setProperty( QNAME_P_QIntervallResult_station, station );
  }

  public void setSlope( final BigDecimal slope )
  {
    getFeature().setProperty( QNAME_P_QIntervallResult_slope, slope );
  }

  /**
   * Returns the points-observation.
   * <p>
   * If it does not yet exists, it is created and initialized.
   * </p>
   * <p>
   * If the observation is changed afterwards, {@link #setPointsObservation(IObservation) has to be called.
   */
  public IObservation<TupleResult> getPointsObservation( )
  {
    final Feature feature = getFeature();
    final Object propertyValue = feature.getProperty( QNAME_P_QIntervallResult_pointsMember );
    if( propertyValue != null )
      return ObservationFeatureFactory.toObservation( (Feature) propertyValue );

    final GMLWorkspace workspace = feature.getWorkspace();
    final IGMLSchema schema = workspace.getGMLSchema();
    final IFeatureType ftQIntervallResult = schema.getFeatureType( QIntervallResult.QNAME_F_QIntervallResult );
    final IFeatureType ftObservation = schema.getFeatureType( QNAME_F_WPointsObservation );
    final IRelationType pointsObsRelation = (IRelationType) ftQIntervallResult.getProperty( QNAME_P_QIntervallResult_pointsMember );

    final Feature obsFeature = workspace.createFeature( feature, pointsObsRelation, ftObservation );
    feature.setProperty( QNAME_P_QIntervallResult_pointsMember, obsFeature );

    final IComponent[] pointsComponents = createPointsComponents( obsFeature );

    final TupleResult tupleResult = new TupleResult( pointsComponents );
    return new Observation<TupleResult>( "", "", tupleResult ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  public void setPointsObservation( final IObservation<TupleResult> observation )
  {
    final Feature obsFeature = (Feature) getFeature().getProperty( QNAME_P_QIntervallResult_pointsMember );

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
    final Feature feature = getFeature();
    final IGMLSchema schema = feature.getWorkspace().getGMLSchema();
    final IFeatureType ftQIntervallResult = schema.getFeatureType( QIntervallResult.QNAME_F_QIntervallResult );

    final IFeatureType ftProfile = schema.getFeatureType( IProfileFeature.QN_PROFILE );
    final IRelationType profileRelation = (IRelationType) ftQIntervallResult.getProperty( QNAME_P_QIntervallResult_profileMember );

    final String href = "project:/modell.gml#" + profile.getId(); //$NON-NLS-1$
    final Feature profileFeatureRef = new XLinkedFeature_Impl( feature, profileRelation, ftProfile, href, "", "", "", "", "" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
    feature.setProperty( profileRelation, profileFeatureRef );

    final IProfileObject[] buildings = profile.getProfil().getProfileObjects( IProfileBuilding.class );
    if( ArrayUtils.isEmpty( buildings ) )
      return;

    final IProfileObject building = buildings[0];
    feature.setProperty( QNAME_P_QIntervallResult_buildingId, building.getId() );
  }

  public String getBuildingId( )
  {
    return (String) getFeature().getProperty( QNAME_P_QIntervallResult_buildingId );
  }

  /** Creates and sets the polynomial. */
  public IPolynomial1D createPolynomial( ) throws Exception
  {
    final Feature feature = getFeature();
    final GMLWorkspace workspace = feature.getWorkspace();

    final IGMLSchema schema = workspace.getGMLSchema();
    final IFeatureType resultFT = schema.getFeatureType( QNAME_F_QIntervallResult );
    final IRelationType polynomialRelation = (IRelationType) resultFT.getProperty( QNAME_P_QIntervallResult_polynomialMember );

    final IFeatureType polynomialFT = schema.getFeatureType( IPolynomial1D.QNAME );

    final Feature polynomialFeature = workspace.createFeature( feature, polynomialRelation, polynomialFT );
    workspace.addFeatureAsComposition( feature, polynomialRelation, -1, polynomialFeature );
    return (IPolynomial1D) polynomialFeature.getAdapter( IPolynomial1D.class );
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
    final Feature feature = getFeature();
    final Object propertyValue = feature.getProperty( QNAME_P_QIntervallResult_buildingMember );
    if( propertyValue != null )
      return ObservationFeatureFactory.toObservation( (Feature) propertyValue );

    if( !createIfEmpty )
      return null;

    final GMLWorkspace workspace = feature.getWorkspace();
    final IGMLSchema schema = workspace.getGMLSchema();
    final IFeatureType ftQIntervallResult = schema.getFeatureType( QIntervallResult.QNAME_F_QIntervallResult );
    final IFeatureType ftObservation = schema.getFeatureType( QNAME_F_BuildingObservation );
    final IRelationType pointsObsRelation = (IRelationType) ftQIntervallResult.getProperty( QNAME_P_QIntervallResult_buildingMember );

    final Feature obsFeature = workspace.createFeature( feature, pointsObsRelation, ftObservation );
    feature.setProperty( QNAME_P_QIntervallResult_buildingMember, obsFeature );

    final IComponent[] pointsComponents = new IComponent[3];
    pointsComponents[0] = ObservationFeatureFactory.createDictionaryComponent( obsFeature, IWspmTuhhQIntervallConstants.DICT_COMPONENT_RUNOFF );
    pointsComponents[1] = ObservationFeatureFactory.createDictionaryComponent( obsFeature, IWspmTuhhQIntervallConstants.DICT_COMPONENT_WATERLEVEL_DOWNSTREAM );
    pointsComponents[2] = ObservationFeatureFactory.createDictionaryComponent( obsFeature, IWspmTuhhQIntervallConstants.DICT_COMPONENT_WATERLEVEL_UPSTREAM );

    final TupleResult tupleResult = new TupleResult( pointsComponents );

    // Sort by discharge and then downstream waterlevel
    tupleResult.setSortComponents( new IComponent[] { pointsComponents[0], pointsComponents[1] } );

    return new Observation<TupleResult>( "", "", tupleResult ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  public void setWeirObservation( final IObservation<TupleResult> observation )
  {
    final Feature obsFeature = (Feature) getFeature().getProperty( QNAME_P_QIntervallResult_buildingMember );

    ObservationFeatureFactory.toFeature( observation, obsFeature );
  }

  private static IComponent[] createPointsComponents( final Feature obsFeature )
  {
    final IComponent[] components = new IComponent[8];

    components[0] = ObservationFeatureFactory.createDictionaryComponent( obsFeature, IWspmTuhhQIntervallConstants.DICT_COMPONENT_WATERLEVEL );
    components[1] = ObservationFeatureFactory.createDictionaryComponent( obsFeature, IWspmTuhhQIntervallConstants.DICT_COMPONENT_DEPTH );
    components[2] = ObservationFeatureFactory.createDictionaryComponent( obsFeature, IWspmTuhhQIntervallConstants.DICT_COMPONENT_AREA );
    components[3] = ObservationFeatureFactory.createDictionaryComponent( obsFeature, IWspmTuhhQIntervallConstants.DICT_COMPONENT_RUNOFF );
    components[4] = ObservationFeatureFactory.createDictionaryComponent( obsFeature, IWspmTuhhQIntervallConstants.DICT_COMPONENT_ALPHA );
    components[5] = ObservationFeatureFactory.createDictionaryComponent( obsFeature, IWspmTuhhQIntervallConstants.DICT_COMPONENT_DELTA_AREA );
    components[6] = ObservationFeatureFactory.createDictionaryComponent( obsFeature, IWspmTuhhQIntervallConstants.DICT_COMPONENT_DELTA_RUNOFF );
    components[7] = ObservationFeatureFactory.createDictionaryComponent( obsFeature, IWspmTuhhQIntervallConstants.DICT_COMPONENT_DELTA_ALPHA );

    return components;
  }

  public BigDecimal getSlope( )
  {
    return (BigDecimal) getFeature().getProperty( QNAME_P_QIntervallResult_slope );
  }

  public BigDecimal getStation( )
  {
    return (BigDecimal) getFeature().getProperty( QNAME_P_QIntervallResult_station );
  }

  public List< ? > getPolynomialFeatures( )
  {
    return (List< ? >) getFeature().getProperty( QNAME_P_QIntervallResult_polynomialMember );
  }

}
