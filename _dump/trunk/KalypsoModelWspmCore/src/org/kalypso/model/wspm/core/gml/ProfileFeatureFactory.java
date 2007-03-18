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
package org.kalypso.model.wspm.core.gml;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.metadata.MetadataObject;
import org.kalypso.commons.xml.NS;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.KalypsoModelWspmCorePlugin;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.IProfilPointMarkerProvider;
import org.kalypso.model.wspm.core.profil.IProfilPointProperty;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.IProfileObjectProvider;
import org.kalypso.model.wspm.core.profil.ProfilFactory;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.IPhenomenon;
import org.kalypso.observation.Observation;
import org.kalypso.observation.Phenomenon;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.binding.NamedFeatureHelper;

/**
 * Intermediates between the {@link IProfil} interface and Featurees of QName {org.kalypso.model.wspm.profile}profile
 * 
 * @author Gernot Belger
 */
public class ProfileFeatureFactory implements IWspmConstants
{
  public final static QName QN_PROF_PROFILE = new QName( NS_WSPMPROF, "Profile" );

  public static final QName QNAME_STATION = new QName( NS_WSPMPROF, "station" );

  public static final QName QNAME_TYPE = new QName( NS_WSPMPROF, "type" );

  public static final String DICT_COMP_PROFILE_PREFIX = "urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#";

  // private static final String DICT_COMP_PROFILE_DEVIDER_PREFIX =
  // "urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#";

  // private static final String DICT_COMP_PROFILE_BUILDING_PREFIX =
  // "urn:ogc:gml:dict:kalypso:model:wspm:profileBuildingComponents#";

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
    final FeatureChange[] changes = toFeatureAsChanges( profile, targetFeature );
    for( final FeatureChange change : changes )
      change.getFeature().setProperty( change.getProperty(), change.getNewValue() );
  }

  /**
   * Converts a profile to a feature. The feature is not yet changed but the needed changes are returned as feature
   * changes.
   */
  @SuppressWarnings("unchecked")
  public static FeatureChange[] toFeatureAsChanges( final IProfil profile, final Feature targetFeature )
  {
    final IFeatureType featureType = targetFeature.getFeatureType();

    if( !GMLSchemaUtilities.substitutes( featureType, QN_PROF_PROFILE ) )
      throw new IllegalArgumentException( "Feature ist not a profile: " + targetFeature );

    final List<FeatureChange> changes = new ArrayList<FeatureChange>();

    //
    // Name + Description
    //
    final String name = profile.getName();
    final List<String> namelist = new ArrayList<String>( 1 );
    namelist.add( name );
    changes.add( new FeatureChange( targetFeature, featureType.getProperty( NamedFeatureHelper.GML_NAME ), namelist ) );
    final String description = profile.getComment().toString();
    changes.add( new FeatureChange( targetFeature, featureType.getProperty( NamedFeatureHelper.GML_DESCRIPTION ), description ) );

    //
    // Station
    //
    final double station = profile.getStation();
    if( Double.isNaN( station ) || Double.isInfinite( station ) )
      changes.add( new FeatureChange( targetFeature, featureType.getProperty( QNAME_STATION ), null ) );
    else
      changes.add( new FeatureChange( targetFeature, featureType.getProperty( QNAME_STATION ), new BigDecimal( station, IWspmConstants.STATION_MATH_CONTEXT ) ) );

    //
    // Type
    //
    final String profiletype = profile.getType();
    changes.add( new FeatureChange( targetFeature, featureType.getProperty( QNAME_TYPE ), profiletype ) );

    /* Ensure that record-definition is there */
    final Feature recordDefinition = FeatureHelper.resolveLink( targetFeature, ObservationFeatureFactory.OM_RESULTDEFINITION );
    if( recordDefinition == null )
    {
      final GMLWorkspace workspace = targetFeature.getWorkspace();
      final IRelationType rdParentRelation = (IRelationType) featureType.getProperty( ObservationFeatureFactory.OM_RESULTDEFINITION );
      final Feature rd = workspace.createFeature( targetFeature, rdParentRelation, workspace.getGMLSchema().getFeatureType( ObservationFeatureFactory.SWE_RECORDDEFINITIONTYPE ) );

      changes.add( new FeatureChange( targetFeature, rdParentRelation, rd ) );
    }

    //
    // read tuple data into tuple-observation
    //
    final TupleResult result = new TupleResult();
    final IProfilPointProperty[] pointProperties = profile.getPointProperties();
    final Map<String, IComponent> compMap = new HashMap<String, IComponent>( pointProperties.length );

    // add all devider types which have deviders
    // if we don't do it here, we get later null entries in the result
    final Map<String, IComponent> deviderComponents = new HashMap<String, IComponent>();
    final String[] markerTypes = profile.getPointMarkerTypes();
    for( final String markerType : markerTypes )
    {
      final IProfilPointMarker[] markers = profile.getPointMarkerFor( markerType );
      for( final IProfilPointMarker marker : markers )
      {
        final String dTyp = marker.getMarkerId();
        final IComponent deviderComponent = ObservationFeatureFactory.createDictionaryComponent( targetFeature, dTyp );
        deviderComponents.put( dTyp, deviderComponent );
        result.addComponent( deviderComponents.get( dTyp ) );
        // the first one is enough!
        break;
      }
    }
    for( final IProfilPoint point : profile.getPoints() )
    {
      final IRecord record = result.createRecord();
      result.add( record );

      for( final IProfilPointProperty pp : pointProperties )
      {
        final String ppName = pp.getId();
        if( !compMap.containsKey( ppName ) )
        {
          final IComponent component = ObservationFeatureFactory.createDictionaryComponent( targetFeature, ppName );
          compMap.put( ppName, component );
          result.addComponent( component );
        }

        final IComponent comp = compMap.get( ppName );
        final double value = point.getValueFor( pp.toString() );
        result.setValue( record, comp, new Double( value ) );
      }

      // Handle devider
      final IProfilPointMarker[] markers = profile.getPointMarkerFor( point );

      for( final IProfilPointMarker marker : markers )
      {
        final IComponent component = deviderComponents.get( marker.getMarkerId() );
        // don't need to add it, because it should already has been added before
        final Object value = marker.getGmlObject();

        if( value == null )
          throw new NullPointerException();
        record.setValue( component, value );
      }
    }

    // write the table into the main feature
    final List<MetadataObject> metadata = new ArrayList<MetadataObject>();
    final Observation<TupleResult> tableObs = new Observation<TupleResult>( name, description, result, metadata );
    final FeatureChange[] obsChanges = ObservationFeatureFactory.toFeatureAsChanges( tableObs, targetFeature );
    Collections.addAll( changes, obsChanges );

    //
    // Building
    //
    final QName memberQName = new QName( NS_WSPMPROF, "member" );
    final IRelationType buildingRT = (IRelationType) featureType.getProperty( memberQName );

    final IProfileObject building = profile.getProfileObject();
    final FeatureList buildingList = FeatureFactory.createFeatureList( targetFeature, buildingRT, new Feature[] {} );
    if( building != null )
    {
      final IFeatureType buildingType = featureType.getGMLSchema().getFeatureType( new QName( NS.OM, "Observation" ) );
      final IRelationType buildingParentRelation = buildingList.getParentFeatureTypeProperty();
      final Feature buildingFeature = targetFeature.getWorkspace().createFeature( targetFeature, buildingParentRelation, buildingType );
      buildingList.add( buildingFeature );
      final IObservation<TupleResult> buildingObs = observationFromBuilding( building, buildingFeature );
      ObservationFeatureFactory.toFeature( buildingObs, buildingFeature );
    }

    /* Always to set the building, even if null */
    changes.add( new FeatureChange( targetFeature, buildingRT, buildingList ) );

    return changes.toArray( new FeatureChange[changes.size()] );
  }

  private static IObservation<TupleResult> observationFromBuilding( final IProfileObject building, final Feature obsFeature )
  {
    final String[] buildingProperties = building.getObjectProperties();

    final TupleResult result = new TupleResult();
    final IRecord record = result.createRecord();
    result.add( record );

    for( final String bp : buildingProperties )
    {
      final IComponent component = ObservationFeatureFactory.createDictionaryComponent( obsFeature, bp );
      result.addComponent( component );
      final Object value = building.getValueFor( bp );
      record.setValue( component, value );
    }

    final List<MetadataObject> metaList = new ArrayList<MetadataObject>();

    final String typ = building.getId();

    final IObservation<TupleResult> observation = new Observation<TupleResult>( typ, "Bauwerk-Observation", result, metaList );

    observation.setPhenomenon( new Phenomenon( typ, null, null ) );

    return observation;
  }

  public static IProfil toProfile( final Feature profileFeature )
  {
    final IFeatureType featureType = profileFeature.getFeatureType();

    if( !GMLSchemaUtilities.substitutes( featureType, QN_PROF_PROFILE ) )
      throw new IllegalArgumentException( "Feature ist not a profile: " + profileFeature );

    final IObservation<TupleResult> observation = ObservationFeatureFactory.toObservation( profileFeature );

    final String profiletype = (String) profileFeature.getProperty( QNAME_TYPE );
    final IProfil profil = ProfilFactory.createProfil( profiletype );

    //
    // Metadaten
    //
    // final ArrayList<Object> metastrings = new ArrayList<Object>( 14 );
    // metastrings.add( profileFeature.getWorkspace().getContext().toString() );
    // metastrings.add( "" );
    // metastrings.add( "" );
    // metastrings.add( "" );
    // metastrings.add( "" );
    // metastrings.add( "" );
    // metastrings.add( "Gew‰sser Zustand" );
    // metastrings.add( "QUERPROFIL 000" );
    // metastrings.add( "STATION KM 000.0000" );
    // metastrings.add( "" );
    // metastrings.add( "01.01.1900" );
    // metastrings.add( "B-1 0 0 0 0 0 0" );
    // metastrings.add( "" );
    // profil.setProperty( IWspmConstants.PROFIL_PROPERTY_METASTRINGS, metastrings );
    // profil.setProperty( IWspmConstants.PROFIL_PROPERTY_STATUS, "Zustand" );
    // profil.setProperty( IWspmConstants.PROFIL_PROPERTY_VERZWEIGUNGSKENNUNG, "0" );
    // profil.setProperty( IWspmConstants.PROFIL_PROPERTY_WASSERSPIEGEL, "Gewaesser" );
    // profil.setProperty( IWspmConstants.PROFIL_PROPERTY_MEHRFELDBRUECKE, "0" );

    // observation.getMetadataList().add( new Metadataobject(name, type, value) );

    
    profil.setName(observation.getName());
    profil.addComment( observation.getDescription() );
    final BigDecimal station = getProfileStation( profileFeature );
    profil.setStation( station == null ? Double.NaN : station.doubleValue() );

    //
    // Building
    //
    // REMARK: handle buildings before table, because the setBuilding method resets the
    // corresponding table properties.
    final Feature buildingFeature = (Feature) FeatureHelper.getFirstProperty( profileFeature, new QName( NS_WSPMPROF, "member" ) );
    
    final IProfileObject po = buildingFromFeature( profil, buildingFeature );
    if( po != null )
      profil.setProfileObject( po );

    //
    // Tabelle
    //
    final TupleResult result = observation.getResult();
    final IComponent[] components = result.getComponents();
    for( final IComponent component : components )
    {
      final String pp = component.getId();

      /* Test, if this component describes a pointProperty, if yes add it to the profile. */
      if( profil.getPropertyProviderFor( pp ) != null )
      {
        // TODO: Kim muss weg, besser w‰ren zwie verschiedene Rauheits-Componenten bzw. Point-Properties
        // if( pp == IWspmConstants.POINT_PROPERTY_RAUHEIT )
        // {
        // if( "ks".equals( component.getUnit() ) )
        // profil.setProperty( IWspmConstants.RAUHEIT_TYP, IWspmConstants.RAUHEIT_TYP_KS );
        // else
        // profil.setProperty( IWspmConstants.RAUHEIT_TYP, IWspmConstants.RAUHEIT_TYP_KST );
        // }

        profil.addPointProperty( pp );
      }
    }

    for( final IRecord record : result )
    {
      final IProfilPoint point = profil.createProfilPoint();

      for( final IComponent component : components )
      {
        final String compId = component.getId();
        final Object value = record.getValue( component );

        if( profil.getPropertyProviderFor( compId ) != null )
        {
          point.setValueFor( compId, (Double) value );
          continue;
        }

        final IProfilPointMarkerProvider markerProvider = profil.getMarkerProviderFor( compId );
        if( markerProvider != null )
        {
          final IProfilPointMarker marker = markerProvider.createMarkerFromGml( compId, value );
          if( marker != null )
          {
            marker.setPoint( point );
            profil.addPointMarker( marker );
          }
        }
        else
        {
          final IStatus status = StatusUtilities.createWarningStatus( "Unknown component in profile-observation: " + compId );
          KalypsoModelWspmCorePlugin.getDefault().getLog().log( status );
        }
      }
      profil.addPoint( point );
    }
    return profil;
  }

  public static BigDecimal getProfileStation( final Feature profileFeature )
  {
    return (BigDecimal) profileFeature.getProperty( QNAME_STATION );
  }

  public static void setProfileStation( final Feature profileFeature, final BigDecimal decimal )
  {
    profileFeature.setProperty( QNAME_STATION, decimal );
  }

  private static IProfileObject buildingFromFeature( final IProfil profil, final Feature buildingFeature )
  {
    final IObservation<TupleResult> buildingObs = buildingFeature == null ? null : ObservationFeatureFactory.toObservation( buildingFeature );
    
    final IPhenomenon phenomenon = buildingObs == null ? null : buildingObs.getPhenomenon();
    if( phenomenon == null )
      return null;

    final IProfileObjectProvider pop = buildingObs == null ? null : profil.getObjectProviderFor( phenomenon.getID() );
    final IProfileObject building = pop == null ? null : pop.createProfileObject( buildingObs.getName() );

    final TupleResult result = buildingObs.getResult();
    if( building == null || result == null )
      return null;

    if( result.isEmpty() )
      return building;
    
    /* transfrom building properties */
    final IRecord record = result.get( 0 );
    for( final IComponent component : result.getComponents() )
    {
      final String id = component.getId();
      final Object value = record.getValue( component );
      building.setValue( id, value );
    }

    return building;
  }
}