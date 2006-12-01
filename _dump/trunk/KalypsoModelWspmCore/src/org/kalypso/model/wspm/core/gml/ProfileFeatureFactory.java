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
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
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
import org.kalypso.model.wspm.core.profil.IProfilBuilding;
import org.kalypso.model.wspm.core.profil.IProfilDevider;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.ProfilBuildingFactory;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.core.profil.ProfilDeviderFactory;
import org.kalypso.model.wspm.core.profil.ProfilDeviderMap;
import org.kalypso.model.wspm.core.profil.ProfilFactory;
import org.kalypso.model.wspm.core.profil.IProfilBuilding.BUILDING_PROPERTY;
import org.kalypso.model.wspm.core.profil.IProfilDevider.DEVIDER_PROPERTY;
import org.kalypso.model.wspm.core.profil.IProfilDevider.DEVIDER_TYP;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.Observation;
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

/**
 * Intermediates between the {@link IProfil} interface and Featurees of QName {org.kalypso.model.wspm.profile}profile
 * 
 * @author Gernot Belger
 */
public class ProfileFeatureFactory implements IWspmConstants
{
  public final static QName QN_PROF_PROFILE = new QName( NS_WSPMPROF, "Profile" );

  public static final String DICT_COMP_PROFILE_PREFIX = "urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#";

  private static final String DICT_COMP_PROFILE_DEVIDER_PREFIX = "urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#";

  private static final String DICT_COMP_PROFILE_BUILDING_PREFIX = "urn:ogc:gml:dict:kalypso:model:wspm:profileBuildingComponents#";

  public static final QName QNAME_STATION = new QName( NS_WSPMPROF, "station" );

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
  public static FeatureChange[] toFeatureAsChanges( final IProfil profile, final Feature targetFeature )
  {
    final IFeatureType featureType = targetFeature.getFeatureType();

    if( !GMLSchemaUtilities.substitutes( featureType, QN_PROF_PROFILE ) )
      throw new IllegalArgumentException( "Feature ist not a profile: " + targetFeature );

    final List<FeatureChange> changes = new ArrayList<FeatureChange>();

    try
    {
      //
      // Station
      //
      final double station = profile.getStation();
      changes.add( new FeatureChange( targetFeature, featureType.getProperty( QNAME_STATION ), new BigDecimal( station, IWspmConstants.STATION_MATH_CONTEXT ) ) );

      /* Ensure that record-definition is there */
      final Feature recordDefinition = FeatureHelper.resolveLink( targetFeature, ObservationFeatureFactory.OM_RESULTDEFINITION );
      if( recordDefinition == null )
      {
        final GMLWorkspace workspace = targetFeature.getWorkspace();
        final Feature rd = workspace.createFeature( targetFeature, workspace.getGMLSchema().getFeatureType( ObservationFeatureFactory.SWE_RECORDDEFINITIONTYPE ) );

        changes.add( new FeatureChange( targetFeature, featureType.getProperty( ObservationFeatureFactory.OM_RESULTDEFINITION ), rd ) );
      }

      //
      // read tuple data into tuple-observation
      //
      final TupleResult result = new TupleResult();
      final LinkedList<POINT_PROPERTY> pointProperties = profile.getPointProperties( false );
      final Map<POINT_PROPERTY, IComponent> compMap = new HashMap<POINT_PROPERTY, IComponent>( pointProperties.size() );
      final ProfilDeviderMap deviderMap = new ProfilDeviderMap( profile );

      // add all devider types which have deviders
      // if we don't do it here, we get later null entries in the result
      final Map<DEVIDER_TYP, IComponent> deviderComponents = new HashMap<DEVIDER_TYP, IComponent>();
      for( final DEVIDER_TYP dTyp : DEVIDER_TYP.values() )
      {
        final IComponent deviderComponent = ObservationFeatureFactory.createDictionaryComponent( targetFeature, DICT_COMP_PROFILE_DEVIDER_PREFIX + dTyp.name() );
        deviderComponents.put( dTyp, deviderComponent );

        final IProfilDevider[] devider = profile.getDevider( dTyp );
        if( devider != null && devider.length > 0 )
          result.addComponent( deviderComponents.get( dTyp ) );
      }

      for( final IProfilPoint point : profile.getPoints() )
      {
        final IRecord record = result.createRecord();
        result.add( record );

        for( final POINT_PROPERTY pp : pointProperties )
        {
          if( !compMap.containsKey( pp ) )
          {
            final IComponent component = ObservationFeatureFactory.createDictionaryComponent( targetFeature, DICT_COMP_PROFILE_PREFIX + pp.name() );
            compMap.put( pp, component );
            result.addComponent( component );
          }
          final IComponent comp = compMap.get( pp );
          final double value = point.getValueFor( pp );
          result.setValue( record, comp, new Double( value ) );
        }

        // Handle devider
        final Collection<IProfilDevider> deviders = deviderMap.get( point );
        for( final IProfilDevider devider : deviders )
        {
          final IComponent component = deviderComponents.get( devider.getTyp() );
          // don't need to add it, because it should already has been added before
          final Object value;
          switch( devider.getTyp() )
          {
            case DURCHSTROEMTE:
              value = Boolean.TRUE;
              break;

            case TRENNFLAECHE:
              final Boolean pos = (Boolean) devider.getValueFor( DEVIDER_PROPERTY.BOESCHUNG );
              value = pos == null || pos.booleanValue() ? "high" : "low";
              break;

            case BORDVOLL:
              value = Boolean.TRUE;
              break;

            case WEHR:
              value = devider.getValueFor( DEVIDER_PROPERTY.BEIWERT );
              break;

            default:
              throw new UnsupportedOperationException( "Unknown devider type: " + devider );
          }

          if( value == null )
            throw new NullPointerException();
          record.setValue( component, value );
        }
      }

      // write the table into the main feature
      final List<MetadataObject> metadata = new ArrayList<MetadataObject>();
      final Observation<TupleResult> tableObs = new Observation<TupleResult>( "Profil", "", result, metadata );
      final FeatureChange[] obsChanges = ObservationFeatureFactory.toFeatureAsChanges( tableObs, targetFeature );
      Collections.addAll( changes, obsChanges );

      //
      // Building
      //
      final QName memberQName = new QName( NS_WSPMPROF, "member" );
      final IRelationType buildingRT = (IRelationType) featureType.getProperty( memberQName );

      final IProfilBuilding building = profile.getBuilding();
      if( building != null )
      {
        final FeatureList buildingList = FeatureFactory.createFeatureList( targetFeature, buildingRT, new Feature[] {} );
        final IFeatureType buildingType = featureType.getGMLSchema().getFeatureType( new QName( NS.OM, "Observation" ) );
        final Feature buildingFeature = targetFeature.getWorkspace().createFeature( targetFeature, buildingType );
        buildingList.add( buildingFeature );
        final IObservation<TupleResult> buildingObs = observationFromBuilding( building, buildingFeature );
        ObservationFeatureFactory.toFeature( buildingObs, buildingFeature );

        changes.add( new FeatureChange( targetFeature, buildingRT, buildingList ) );
      }
    }
    catch( final ProfilDataException e )
    {
      e.printStackTrace();
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      KalypsoModelWspmCorePlugin.getDefault().getLog().log( status );
    }

    return changes.toArray( new FeatureChange[changes.size()] );
  }

  private static IObservation<TupleResult> observationFromBuilding( final IProfilBuilding building, final Feature obsFeature ) throws ProfilDataException
  {
    final Collection<BUILDING_PROPERTY> buildingProperties = building.getBuildingProperties();

    final TupleResult result = new TupleResult();
    final IRecord record = result.createRecord();
    result.add( record );

    for( final BUILDING_PROPERTY bp : buildingProperties )
    {
      final IComponent component = ObservationFeatureFactory.createDictionaryComponent( obsFeature, DICT_COMP_PROFILE_BUILDING_PREFIX + bp.name() );
      result.addComponent( component );
      final Object value = building.getValueFor( bp );
      record.setValue( component, value );
    }

    final List<MetadataObject> metaList = new ArrayList<MetadataObject>();

    final String typ = building.getTyp();

    final IObservation<TupleResult> observation = new Observation<TupleResult>( typ, "Bauwerk-Observation", result, metaList );

    observation.setPhenomenon( typ );

    return observation;
  }

  public static IProfil toProfile( final Feature profileFeature ) throws ProfilDataException
  {
    final IFeatureType featureType = profileFeature.getFeatureType();

    if( !GMLSchemaUtilities.substitutes( featureType, QN_PROF_PROFILE ) )
      throw new IllegalArgumentException( "Feature ist not a profile: " + profileFeature );

    final IObservation<TupleResult> observation = ObservationFeatureFactory.toObservation( profileFeature );

    final IProfil profil = ProfilFactory.createProfil();

    //
    // Metadaten
    //
    // TODO: we now create the needed metastrings and such for the prf serializer
    // this should not be necessary
    final ArrayList<Object> metastrings = new ArrayList<Object>( 14 );
    metastrings.add( profileFeature.getWorkspace().getContext().toString() );
    metastrings.add( "" );
    metastrings.add( "" );
    metastrings.add( "" );
    metastrings.add( "" );
    metastrings.add( "" );
    metastrings.add( "Gew‰sser Zustand" );
    metastrings.add( "QUERPROFIL 000" );
    metastrings.add( "STATION KM 000.0000" );
    metastrings.add( "" );
    metastrings.add( "01.01.1900" );
    metastrings.add( "B-1 0 0 0 0 0 0" );
    metastrings.add( "" );
    profil.setProperty( "IProfil.PROFIL_PROPERTY.METASTRINGS", metastrings );
    profil.setProperty( IProfil.PROFIL_PROPERTY.STATUS, "Zustand" );
    profil.setProperty( IProfil.PROFIL_PROPERTY.VERZWEIGUNGSKENNUNG, "0" );
    profil.setProperty( IProfil.PROFIL_PROPERTY.WASSERSPIEGEL, "Gewaesser" );
    profil.setProperty( IProfil.PROFIL_PROPERTY.MEHRFELDBRUECKE, "0" );

    final BigDecimal station = getProfileStation( profileFeature );
    profil.setStation( station == null ? Double.NaN : station.doubleValue() );

    //
    // Building
    //
    // REMARK: handle buildings before table, because the setBuilding method resets the
    // corresponding table properties.
    final Feature buildingFeature = (Feature) FeatureHelper.getFirstProperty( profileFeature, new QName( NS_WSPMPROF, "member" ) );
    if( buildingFeature != null )
      profil.setBuilding( buildingFromFeature( buildingFeature ) );

    //
    // Tabelle
    //
    final TupleResult result = observation.getResult();
    final IComponent[] components = result.getComponents();
    final Map<IComponent, IProfilPoint.POINT_PROPERTY> compMap = new HashMap<IComponent, IProfilPoint.POINT_PROPERTY>();
    for( final IComponent component : components )
    {
      final String id = component.getId();

      final POINT_PROPERTY pp = pointPropertyFromComponentId( id );
      if( pp != null )
      {
        if( pp == POINT_PROPERTY.RAUHEIT )
        {
         // final String rauheit_typ = IProfil.RAUHEIT_TYP.valueOf( component.getUnit() );
          profil.setProperty( IProfil.PROFIL_PROPERTY.RAUHEIT_TYP, component.getUnit() );
        }

        profil.addPointProperty( pp );
        compMap.put( component, pp );
      }
    }

    for( final IRecord record : result )
    {
      final IProfilPoint point = profil.addPoint( 0.0, 0.0 );

      for( final IComponent component : components )
      {
        final String compId = component.getId();
        final Object value = record.getValue( component );

        final POINT_PROPERTY pp = compMap.get( component );
        if( pp != null )
        {
          point.setValueFor( pp, (Double) value );
          continue;
        }
        // Devider
        else if( (DICT_COMP_PROFILE_DEVIDER_PREFIX + DEVIDER_TYP.DURCHSTROEMTE.name()).equals( compId ) )
        {
          final Boolean hasDevider = (Boolean) value;
          if( hasDevider != null && hasDevider.booleanValue() )
          {
            final IProfilDevider devider = ProfilDeviderFactory.createDevider( DEVIDER_TYP.DURCHSTROEMTE, point );
            profil.addDevider( devider );
          }
        }
        else if( (DICT_COMP_PROFILE_DEVIDER_PREFIX + DEVIDER_TYP.BORDVOLL.name()).equals( compId ) )
        {
          final Boolean hasDevider = (Boolean) value;
          if( hasDevider != null && hasDevider.booleanValue() )
          {
            final IProfilDevider devider = ProfilDeviderFactory.createDevider( DEVIDER_TYP.BORDVOLL, point );
            profil.addDevider( devider );
          }
        }
        else if( (DICT_COMP_PROFILE_DEVIDER_PREFIX + DEVIDER_TYP.TRENNFLAECHE.name()).equals( compId ) )
        {
          final String kind = (String) value;
          if( "low".equals( kind ) || "high".equals( kind ) )
          {
            final IProfilDevider devider = ProfilDeviderFactory.createDevider( DEVIDER_TYP.TRENNFLAECHE, point );
            final Boolean high = Boolean.valueOf( "high".equals( kind ) );
            devider.setValueFor( DEVIDER_PROPERTY.BOESCHUNG, high );
            profil.addDevider( devider );
          }
        }
        else if( (DICT_COMP_PROFILE_DEVIDER_PREFIX + DEVIDER_TYP.WEHR.name()).equals( compId ) )
        {
          final IProfilDevider devider = ProfilDeviderFactory.createDevider( DEVIDER_TYP.TRENNFLAECHE, point );
          devider.setValueFor( DEVIDER_PROPERTY.BOESCHUNG, value );
          profil.addDevider( devider );
        }

      }
    }

    return profil;
  }

  public static POINT_PROPERTY pointPropertyFromComponentId( final String id )
  {
    final String name;
    final boolean devider;
    if( id.startsWith( DICT_COMP_PROFILE_DEVIDER_PREFIX ) )
    {
      name = id.substring( DICT_COMP_PROFILE_DEVIDER_PREFIX.length() );
      devider = true;
    }
    else if( id.startsWith( DICT_COMP_PROFILE_PREFIX ) )
    {
      name = id.substring( DICT_COMP_PROFILE_PREFIX.length() );
      devider = false;
    }
    else
    {
      name = id;
      devider = false;
    }

    if( devider == false )
      return IProfilPoint.POINT_PROPERTY.valueOf( name );

    return null;
  }

  public static BigDecimal getProfileStation( final Feature profileFeature )
  {
    return (BigDecimal) profileFeature.getProperty( QNAME_STATION );
  }

  public static void setProfileStation( final Feature profileFeature, final BigDecimal decimal )
  {
    profileFeature.setProperty( QNAME_STATION, decimal );
  }
  
  private static IProfilBuilding buildingFromFeature( final Feature buildingFeature ) throws ProfilDataException
  {
    final IObservation<TupleResult> buildingObs = ObservationFeatureFactory.toObservation( buildingFeature );

    final String phenomenon = buildingObs == null ? null : buildingObs.getPhenomenon();
    if( phenomenon == null )
      return null;

    final IProfilBuilding building = ProfilBuildingFactory.createProfilBuilding( phenomenon );

    final TupleResult result = buildingObs.getResult();
    if( result == null )
      return null;

    final IRecord record = result.get( 0 );
    for( final IComponent component : result.getComponents() )
    {
      final String id = component.getId();
      final String name;
      if( id.startsWith( DICT_COMP_PROFILE_BUILDING_PREFIX ) )
        name = id.substring( DICT_COMP_PROFILE_BUILDING_PREFIX.length() );
      else
        name = id;

      final BUILDING_PROPERTY bProperty = BUILDING_PROPERTY.valueOf( name );
      final Object value = record.getValue( component );
      building.setValue( bProperty, value );
    }

    return building;
  }
}
