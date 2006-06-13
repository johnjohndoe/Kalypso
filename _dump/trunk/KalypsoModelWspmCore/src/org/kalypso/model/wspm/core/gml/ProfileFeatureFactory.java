/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

import org.kalypso.commons.metadata.MetadataObject;
import org.kalypso.commons.xml.NS;
import org.kalypso.commons.xml.XmlTypes;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilBuilding;
import org.kalypso.model.wspm.core.profil.IProfilDevider;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.ProfilBuildingFactory;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.core.profil.ProfilDeviderFactory;
import org.kalypso.model.wspm.core.profil.ProfilDeviderMap;
import org.kalypso.model.wspm.core.profil.ProfilFactory;
import org.kalypso.model.wspm.core.profil.IProfil.PROFIL_PROPERTY;
import org.kalypso.model.wspm.core.profil.IProfil.RAUHEIT_TYP;
import org.kalypso.model.wspm.core.profil.IProfilBuilding.BUILDING_PROPERTY;
import org.kalypso.model.wspm.core.profil.IProfilBuilding.BUILDING_TYP;
import org.kalypso.model.wspm.core.profil.IProfilDevider.DEVIDER_PROPERTY;
import org.kalypso.model.wspm.core.profil.IProfilDevider.DEVIDER_TYP;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.Observation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.observation.result.ValueComponent;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * Intermediates between the {@link IProfil} interface and Featurees of QName {org.kalypso.model.wspm.profile}profile
 * 
 * @author belger
 */
public class ProfileFeatureFactory implements IWspmConstants
{
  public final static QName QN_PROF_PROFILE = new QName( NS_WSPMPROF, "Profile" );

  public static final String URN_PHENOMENON_BUILDING = "urn:ogc:phenomenon:wspm:building:";

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
  public static void toFeature( final IProfil profile, final Feature targetFeature ) throws GMLSchemaException
  {
    final IFeatureType featureType = targetFeature.getFeatureType();

    if( !GMLSchemaUtilities.substitutes( featureType, QN_PROF_PROFILE ) )
      throw new IllegalArgumentException( "Feature ist not a profile: " + targetFeature );

    try
    {
      //
      // Station
      //
      final double station = profile.getStation();
      targetFeature.setProperty( new QName( NS_WSPMPROF, "station" ), new Double( station ) );

      //
      // read tuple data into tuple-observation
      //
      final TupleResult result = new TupleResult();
      final LinkedList<POINT_PROPERTY> pointProperties = profile.getPointProperties( false );
      final Map<POINT_PROPERTY, IComponent> compMap = new HashMap<POINT_PROPERTY, IComponent>( pointProperties.size() );
      final ProfilDeviderMap deviderMap = new ProfilDeviderMap( profile );

      final Map<DEVIDER_TYP, IComponent> deviderComponents = new HashMap<DEVIDER_TYP, IComponent>();
      deviderComponents.put( DEVIDER_TYP.DURCHSTROEMTE, new ValueComponent( 0, "Durchströmte Bereiche", "Markierung Durchströmte Bereiche", XmlTypes.XS_BOOLEAN, Boolean.FALSE, "-" ) );
      deviderComponents.put( DEVIDER_TYP.BORDVOLL, new ValueComponent( 1, "Bordvollpunkte", "Markierung Bordvollpunkte", XmlTypes.XS_BOOLEAN, Boolean.FALSE, "-" ) );
      deviderComponents.put( DEVIDER_TYP.TRENNFLAECHE, new ValueComponent( 2, "Trennflächen", "Markierung Trennflächen", XmlTypes.XS_STRING, "none", "-" ) );
      deviderComponents.put( DEVIDER_TYP.WEHR, new ValueComponent( 3, "Trennlinie Wehr", "Markierung Trennlinie Wehr", XmlTypes.XS_DOUBLE, Double.NaN, "-" ) );

      // add all devider types which have deviders
      // if we don't do it here, we get later null entries in the result
      for( final DEVIDER_TYP dTyp : DEVIDER_TYP.values() )
      {
        final IProfilDevider[] devider = profile.getDevider( dTyp );
        if( devider != null && devider.length > 0 )
          result.addComponent( deviderComponents.get( dTyp ) );
      }

      int compCount = 4;

      for( final IProfilPoint point : profile.getPoints() )
      {
        final IRecord record = result.createRecord();
        result.add( record );

        for( final POINT_PROPERTY pp : pointProperties )
        {
          if( !compMap.containsKey( pp ) )
          {
            final ValueComponent component = componentForPointProperty( compCount++, profile, pp );
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
              value = pos.booleanValue() ? "high" : "low";
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
      ObservationFeatureFactory.toFeature( tableObs, targetFeature );

      //
      // Building
      //
      final QName memberQName = new QName( NS_WSPMPROF, "member" );
      final FeatureList memberFeatures = (FeatureList) targetFeature.getProperty( memberQName );
      memberFeatures.clear(); // delete existing features

      final IProfilBuilding building = profile.getBuilding();
      if( building != null )
      {
        final IObservation<TupleResult> buildingObs = observationFromBuilding( building );
        final Feature buildingFeature = FeatureHelper.addFeature( targetFeature, memberQName, new QName( NS.OM, "Observation" ) );
        ObservationFeatureFactory.toFeature( buildingObs, buildingFeature );
      }
    }
    catch( final ProfilDataException e )
    {
      e.printStackTrace();

      // TODO: handle exceptions
    }
  }

  private static IObservation<TupleResult> observationFromBuilding( IProfilBuilding building ) throws ProfilDataException
  {
    final Collection<BUILDING_PROPERTY> buildingProperties = building.getBuildingProperties();

    final TupleResult result = new TupleResult();
    final IRecord record = result.createRecord();
    result.add( record );

    int compCount = 0;
    for( final BUILDING_PROPERTY bp : buildingProperties )
    {
      final ValueComponent component = new ValueComponent( compCount++, bp.name(), "", new QName( NS.XSD_SCHEMA, "double" ), 0.0, "" );
      result.addComponent( component );
      record.setValue( component, building.getValueFor( bp ) );
    }

    final List<MetadataObject> metaList = new ArrayList<MetadataObject>();

    final BUILDING_TYP typ = building.getTyp();
    final String phenomenon = URN_PHENOMENON_BUILDING + typ.name();

    final IObservation<TupleResult> observation = new Observation<TupleResult>( typ.toString(), "Bauwerk-Observation", result, metaList );
    observation.setPhenomenon( phenomenon );

    return observation;
  }

  private static ValueComponent componentForPointProperty( final int pos, final IProfil profile, final POINT_PROPERTY pp )
  {
    final String unit;
    if( pp == POINT_PROPERTY.RAUHEIT )
      unit = ((RAUHEIT_TYP) profile.getProperty( PROFIL_PROPERTY.RAUHEIT_TYP )).name();
    else
      unit = "?";

    return new ValueComponent( pos, pp.name(), "", new QName( NS.XSD_SCHEMA, "double" ), 0.0, unit );
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
    // TODO: we now create the needed metastrings and such für the prf serializer
    // this should not be necessary
    final ArrayList<Object> metastrings = new ArrayList<Object>( 14 );
    metastrings.add( profileFeature.getWorkspace().getContext().toString() );
    metastrings.add( "" );
    metastrings.add( "" );
    metastrings.add( "" );
    metastrings.add( "" );
    metastrings.add( "" );
    metastrings.add( "Gewässer Zustand" );
    metastrings.add( "QUERPROFIL 000" );
    metastrings.add( "STATION KM 000.0000" );
    metastrings.add( "" );
    metastrings.add( "01.01.1900" );
    metastrings.add( "B-1 0 0 0 0 0 0" );
    metastrings.add( "" );
    profil.setProperty( IProfil.PROFIL_PROPERTY.METASTRINGS, metastrings );
    profil.setProperty( IProfil.PROFIL_PROPERTY.STATUS, "Zustand" );
    profil.setProperty( IProfil.PROFIL_PROPERTY.VERZWEIGUNGSKENNUNG, "0" );
    profil.setProperty( IProfil.PROFIL_PROPERTY.WASSERSPIEGEL, "Gewaesser" );
    profil.setProperty( IProfil.PROFIL_PROPERTY.MEHRFELDBRUECKE, "0" );

    //
    // Station
    //
    final Double property = (Double) profileFeature.getProperty( new QName( NS_WSPMPROF, "station" ) );
    profil.setStation( property == null ? Double.NaN : property.doubleValue() );

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
      final String name = component.getName();
      try
      {
        final POINT_PROPERTY pp = IProfilPoint.POINT_PROPERTY.valueOf( name );
        if( pp == POINT_PROPERTY.RAUHEIT )
        {
          final RAUHEIT_TYP rauheit_typ = IProfil.RAUHEIT_TYP.valueOf( ((ValueComponent) component).getUnit() );
          profil.setProperty( IProfil.PROFIL_PROPERTY.RAUHEIT_TYP, rauheit_typ );
        }

        profil.addPointProperty( pp );
        compMap.put( component, pp );
      }
      catch( final IllegalArgumentException e )
      {
        // ignore, name is not know, maybe we have a marker
      }
    }

    for( final IRecord record : result )
    {
      final IProfilPoint point = profil.addPoint( 0.0, 0.0 );

      for( final IComponent component : components )
      {
        final String compName = component.getName();
        final Object value = record.getValue( component );

        final POINT_PROPERTY pp = compMap.get( component );
        if( pp != null )
        {
          point.setValueFor( pp, (Double) value );
          continue;
        }
        // Devider
        else if( "Durchströmte Bereiche".equals( compName ) )
        {
          final Boolean hasDevider = (Boolean) value;
          if( hasDevider.booleanValue() )
          {
            final IProfilDevider devider = ProfilDeviderFactory.createDevider( DEVIDER_TYP.DURCHSTROEMTE, point );
            profil.addDevider( devider );
          }
        }
        else if( "Bordvollpunkte".equals( compName ) )
        {
          final Boolean hasDevider = (Boolean) value;
          if( hasDevider.booleanValue() )
          {
            final IProfilDevider devider = ProfilDeviderFactory.createDevider( DEVIDER_TYP.BORDVOLL, point );
            profil.addDevider( devider );
          }
        }
        else if( "Trennflächen".equals( compName ) )
        {
          final String kind = (String) value;
          if( !"none".equals( kind ) )
          {
            final IProfilDevider devider = ProfilDeviderFactory.createDevider( DEVIDER_TYP.TRENNFLAECHE, point );
            final Boolean high = Boolean.valueOf( "high".equals( kind ) );
            devider.setValueFor( DEVIDER_PROPERTY.BOESCHUNG, high );
            profil.addDevider( devider );
          }
          else if( "Trennlinie Wehr".equals( compName ) )
          {
            final IProfilDevider devider = ProfilDeviderFactory.createDevider( DEVIDER_TYP.TRENNFLAECHE, point );
            devider.setValueFor( DEVIDER_PROPERTY.BOESCHUNG, value );
            profil.addDevider( devider );
          }
        }

      }
    }

    return profil;
  }

  private static IProfilBuilding buildingFromFeature( final Feature buildingFeature ) throws ProfilDataException
  {
    final IObservation<TupleResult> buildingObs = ObservationFeatureFactory.toObservation( buildingFeature );

    final String phenomenon = buildingObs.getPhenomenon();
    final BUILDING_TYP bType = BUILDING_TYP.valueOf( phenomenon.substring( URN_PHENOMENON_BUILDING.length() ) );

    final IProfilBuilding building = ProfilBuildingFactory.createProfilBuilding( bType );

    final TupleResult result = buildingObs.getResult();
    final IRecord record = result.get( 0 );
    for( final IComponent component : result.getComponents() )
    {
      final BUILDING_PROPERTY bProperty = BUILDING_PROPERTY.valueOf( component.getName() );
      final Object value = record.getValue( component );
      building.setValue( bProperty, value );
    }

    return building;
  }
}
