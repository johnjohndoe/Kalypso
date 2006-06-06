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
package org.kalypso.ui.model.wspm.core.profile;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

import org.kalypso.commons.metadata.MetadataObject;
import org.kalypso.commons.xml.NS;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.core.profil.ProfilFactory;
import org.kalypso.model.wspm.core.profil.IProfil.PROFIL_PROPERTY;
import org.kalypso.model.wspm.core.profil.IProfil.RAUHEIT_TYP;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.Observation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.observation.result.ValueComponent;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ui.model.wspm.IWspmConstants;
import org.kalypsodeegree.model.feature.Feature;


/**
 * Intermediates between the {@link IProfil} interface and Featurees of QName {org.kalypso.model.wspm.profile}profile
 * 
 * @author belger
 */
public class ProfileFeatureFactory implements IWspmConstants
{
  public final static QName QN_PROF_PROFILE = new QName( NS_WSPMPROF, "Profile" );

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
    final IFeatureType featureType = targetFeature.getFeatureType();

    if( !GMLSchemaUtilities.substitutes( featureType, QN_PROF_PROFILE ) )
      throw new IllegalArgumentException( "Feature ist not a profile: " + targetFeature );

    try
    {
      //
      // read tuple data into tuple-observation
      //
      final TupleResult result = new TupleResult();
      final LinkedList<POINT_PROPERTY> pointProperties = profile.getPointProperties( false );
      final Map<POINT_PROPERTY, IComponent> compMap = new HashMap<POINT_PROPERTY, IComponent>( pointProperties.size() );
      
      int compCount = 0;
      
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
      }

      // write the table into the main feature
      final List<MetadataObject> metadata = new ArrayList<MetadataObject>();
      final Observation<TupleResult> tableObs = new Observation<TupleResult>( "Profil", "", result, metadata );
      ObservationFeatureFactory.toFeature( tableObs, targetFeature );

      //
      // Buildings
      //
//      final IProfilBuilding building = profile.getBuilding();
//      final Collection<BUILDING_PROPERTY> buildingProperties = building.getBuildingProperties();
//      final BUILDING_TYP typ = building.getTyp();
      
//    final QName memberQName = new QName( NS_WSPMPROF, "member" );
//    final FeatureList memberFeatures = (FeatureList) targetFeature.getProperty( memberQName );
//    // delete existing features
//    memberFeatures.clear();
      
      
      //
      // set station
      //
      // final Feature stationFeature = FeatureHelper.getSubFeature( targetFeature, new QName( NS_WSPMPROF, "station" )
      // );
      // ignore for now

      //
      // read buildings & markers
      //

    }
    catch( final ProfilDataException e )
    {
      e.printStackTrace();

      // TODO: handle exceptions
    }
  }

  private static ValueComponent componentForPointProperty( final int pos, final IProfil profile, final POINT_PROPERTY pp )
  {
    final String unit;
    if( pp == POINT_PROPERTY.RAUHEIT )
      unit = ((RAUHEIT_TYP)profile.getProperty( PROFIL_PROPERTY.RAUHEIT_TYP )).name();
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
    
    final TupleResult result = observation.getResult();
    final IComponent[] components = result.getComponents();
    final Map<IComponent, IProfilPoint.POINT_PROPERTY> compMap = new HashMap<IComponent, IProfilPoint.POINT_PROPERTY>();
    for( final IComponent component : components )
    {
      final String name = component.getName();
      final POINT_PROPERTY pp = IProfilPoint.POINT_PROPERTY.valueOf( name );
      
      if( pp == POINT_PROPERTY.RAUHEIT )
      {
        final RAUHEIT_TYP rauheit_typ = IProfil.RAUHEIT_TYP.valueOf( ((ValueComponent)component).getUnit() );
        profil.setProperty( IProfil.PROFIL_PROPERTY.RAUHEIT_TYP, rauheit_typ );
      }
      
      profil.addPointProperty( pp );
      compMap.put( component, pp );
    }
    
    for( final IRecord record : result )
    {
      final IProfilPoint point = profil.addPoint( 0.0, 0.0 );
      
      for( final IComponent component : components )
      {
        final POINT_PROPERTY pp = compMap.get( component );
        
        final Double value = (Double) record.getValue( component );
        
        point.setValueFor( pp, value );
      }      
    }
    
    return profil;
  }
}
