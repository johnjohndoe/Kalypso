/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.model.wspm.core.gml;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.commons.xml.NS;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.phenomenon.Phenomenon;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.gml.binding.commons.NamedFeatureHelper;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * Intermediates between the {@link IProfil} interface and Features of QName {org.kalypso.model.wspm.profile}profile
 * 
 * @author Gernot Belger
 */
public class ProfileFeatureFactory implements IWspmConstants
{
  public static final QName QNAME_STATION = new QName( IWspmConstants.NS_WSPMPROF, "station" );

  public static final QName QNAME_TYPE = new QName( IWspmConstants.NS_WSPMPROF, "type" );

  public final static QName QN_PROF_PROFILE = new QName( IWspmConstants.NS_WSPMPROF, "Profile" );

  public static final String DICT_COMP_PROFILE_PREFIX = "urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#";

  private final static ProfileFeatureManager m_profileManager = new ProfileFeatureManager();

  private ProfileFeatureFactory( )
  {
    // private: never instatiate
  }

  /**
   * Writes the contents of a profile into a feature. The feature must substitute
   * {org.kalypso.model.wspm.profile}profile.
   * <p>
   * Assumes, that the given feature is empty.
   * </p>
   */
  public static void toFeature( final IProfil profile, final Feature targetFeature )
  {
    final FeatureChange[] changes = ProfileFeatureFactory.toFeatureAsChanges( profile, targetFeature );
    for( final FeatureChange change : changes )
      change.getFeature().setProperty( change.getProperty(), change.getNewValue() );
  }

  public static BigDecimal getProfileStation( final Feature profileFeature )
  {
    return (BigDecimal) profileFeature.getProperty( ProfileFeatureFactory.QNAME_STATION );
  }

  public static void setProfileStation( final Feature profileFeature, final BigDecimal decimal )
  {
    profileFeature.setProperty( ProfileFeatureFactory.QNAME_STATION, decimal );
  }

  /**
   * Converts a profile to a feature. The feature is not yet changed but the needed changes are returned as feature
   * changes.
   */
  @SuppressWarnings("unchecked")
  public static FeatureChange[] toFeatureAsChanges( final IProfil profile, final Feature targetFeature )
  {
    final IFeatureType featureType = targetFeature.getFeatureType();

    if( !GMLSchemaUtilities.substitutes( featureType, ProfileFeatureFactory.QN_PROF_PROFILE ) )
      throw new IllegalArgumentException( "Feature ist not a profile: " + targetFeature );

    final List<FeatureChange> changes = new ArrayList<FeatureChange>();

    /* name and description */
    final String name = profile.getName();
    final String description = profile.getComment();

    final List<String> namelist = new ArrayList<String>();
    namelist.add( name );

    changes.add( new FeatureChange( targetFeature, featureType.getProperty( NamedFeatureHelper.GML_NAME ), namelist ) );
    changes.add( new FeatureChange( targetFeature, featureType.getProperty( NamedFeatureHelper.GML_DESCRIPTION ), description ) );

    /* station */
    final double station = profile.getStation();
    if( Double.isNaN( station ) || Double.isInfinite( station ) )
      changes.add( new FeatureChange( targetFeature, featureType.getProperty( ProfileFeatureFactory.QNAME_STATION ), null ) );
    else
    {
      final BigDecimal bigStation = WspmProfile.stationToBigDecimal( station );
      changes.add( new FeatureChange( targetFeature, featureType.getProperty( ProfileFeatureFactory.QNAME_STATION ), bigStation ) );
    }

    /* type */
    final String profiletype = profile.getType();
    changes.add( new FeatureChange( targetFeature, featureType.getProperty( ProfileFeatureFactory.QNAME_TYPE ), profiletype ) );

    /* Ensure that record-definition is there */
    final Feature recordDefinition = FeatureHelper.resolveLink( targetFeature, ObservationFeatureFactory.OM_RESULTDEFINITION );
    if( recordDefinition == null )
    {
      final GMLWorkspace workspace = targetFeature.getWorkspace();
      final IRelationType rdParentRelation = (IRelationType) featureType.getProperty( ObservationFeatureFactory.OM_RESULTDEFINITION );
      final Feature rd = workspace.createFeature( targetFeature, rdParentRelation, workspace.getGMLSchema().getFeatureType( ObservationFeatureFactory.SWE_RECORDDEFINITIONTYPE ) );

      changes.add( new FeatureChange( targetFeature, rdParentRelation, rd ) );
    }

    final FeatureChange[] obsChanges = ObservationFeatureFactory.toFeatureAsChanges( profile, targetFeature );
    Collections.addAll( changes, obsChanges );

    /* Building */
    final QName memberQName = new QName( IWspmConstants.NS_WSPMPROF, "member" );
    final IRelationType buildingRT = (IRelationType) featureType.getProperty( memberQName );
    final FeatureList buildingList = FeatureFactory.createFeatureList( targetFeature, buildingRT, new Feature[] {} );

    final IProfileObject[] buildings = profile.getProfileObjects();
    for( final IProfileObject profileObject : buildings )
    {
      final IFeatureType buildingType = featureType.getGMLSchema().getFeatureType( new QName( NS.OM, "Observation" ) );
      final IRelationType buildingParentRelation = buildingList.getParentFeatureTypeProperty();
      final Feature buildingFeature = targetFeature.getWorkspace().createFeature( targetFeature, buildingParentRelation, buildingType );
      buildingList.add( buildingFeature );
      final IObservation<TupleResult> buildingObs = ProfileFeatureFactory.observationFromBuilding( profileObject, buildingFeature );
      final FeatureChange[] featureAsChanges = ObservationFeatureFactory.toFeatureAsChanges( buildingObs, buildingFeature );

      Collections.addAll( changes, featureAsChanges );
    }

    /* Always to set the building, even if null */
    changes.add( new FeatureChange( targetFeature, buildingRT, buildingList ) );

    return changes.toArray( new FeatureChange[changes.size()] );
  }

  private static IObservation<TupleResult> observationFromBuilding( final IProfileObject building, final Feature obsFeature )
  {
    final IComponent[] buildingProperties = building.getObjectProperties();

    final TupleResult result = new TupleResult();
    final IRecord record = result.createRecord();
    result.add( record );

    for( final IComponent bp : buildingProperties )
    {
      final IComponent component = ObservationFeatureFactory.createDictionaryComponent( obsFeature, bp.getId() );
      result.addComponent( component );
      final Object value = building.getValue( component );
      record.setValue( component, value );
    }

    final IObservation<TupleResult> observation = building.getObservation();
    observation.setPhenomenon( new Phenomenon( observation.getName(), null, null ) );

    return observation;
  }

  public synchronized static IProfil toProfile( final Feature profileFeature )
  {
    return m_profileManager.getProfile( profileFeature );
  }

  public static String getProfileType( final Feature profileFeature )
  {
    return (String) profileFeature.getProperty( ProfileFeatureFactory.QNAME_TYPE );
  }

  public static void setProfileType( final Feature profileFeature, final String type )
  {
    profileFeature.setProperty( ProfileFeatureFactory.QNAME_TYPE, type );
  }

}