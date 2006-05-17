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
package org.kalypso.ogc.gml.om;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

import javax.xml.namespace.QName;

import net.opengis.swe.PhenomenonPropertyType;
import net.opengis.swe.PhenomenonType;

import org.kalypso.commons.metadata.MetadataObject;
import org.kalypso.commons.xml.NS;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.Observation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.swe.RepresentationType;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author schlienger
 */
public class ObservationFeatureFactory
{
  public final static QName SWE_COMPONENT = new QName( NS.SWE, "component" );

  public final static QName SWE_ITEMDEFINITION = new QName( NS.SWE, "ItemDefinition" );

  public final static QName SWE_PROPERTY = new QName( NS.SWE, "property" );

  public final static QName SWE_REPRESENTATION = new QName( NS.SWE, "representation" );

  /**
   * Makes an observation from a feature. The feature must substitute http://www.opengis.net/om:Observation .
   */
  public IObservation<TupleResult> observationFromFeature( final Feature f ) throws ParseException
  {
    final IFeatureType featureType = f.getFeatureType();

    if( !GMLSchemaUtilities.substitutes( featureType, new QName( NS.OM, "Observation" ) ) )
      throw new IllegalArgumentException( "Feature ist not an Observation: " + f );

    final String name = (String) f.getProperty( new QName( NS.GML3, "name" ) );
    final String desc = (String) f.getProperty( new QName( NS.GML3, "description" ) );
    final List<MetadataObject> meta = (List<MetadataObject>) f.getProperty( new QName( NS.GML3, "metadata" ) );

    final Feature recordDefinition = (Feature) f.getProperty( new QName( NS.OM, "resultDefinition" ) );
    final String result = (String) f.getProperty( new QName( NS.OM, "result" ) );
    
    final TupleResult tupleResult = buildTupleResult( recordDefinition, result );

    return new Observation<TupleResult>( name, desc, tupleResult, meta );
  }

  public static TupleResult buildTupleResult( final Feature recordDefinition, final String result ) throws ParseException
  {
    final ComponentDefinition[] definitions = buildComponentDefinitions( recordDefinition );
    final IComponent[] components = new IComponent[definitions.length];
    
    for( int i = 0; i < definitions.length; i++ )
      components[i] = definitions[i].toComponent();
    
    final TupleResult tupleResult = new TupleResult( components );

    final StringTokenizer tk = new StringTokenizer( result );
    int nb = 0;
    IRecord record = null;
    while( tk.hasMoreElements() )
    {
      if( nb == 0 )
      {
        record = tupleResult.createRecord();
        tupleResult.add( record );
      }
      
      final String token = tk.nextToken();
      final Object value = definitions[nb].getTypeHandler().parseType( token );
      record.setValue( components[nb], value );

      nb++;
      nb = nb % definitions.length;
    }
    
    return tupleResult;
  }

  /**
   * Builds the list of component definitions defined as ItemDefinitions within the recordDefinition element
   */
  public static ComponentDefinition[] buildComponentDefinitions( final Feature recordDefinition )
  {
    final List<ComponentDefinition> components = new ArrayList<ComponentDefinition>();

    final Object[] props = recordDefinition.getProperties();
    for( int i = 0; i < props.length; i++ )
    {
      final Feature f = (Feature) props[i];
      if( f.getFeatureType().getQName().equals( SWE_COMPONENT ) )
      {
        final Feature itemDef = (Feature) f.getProperty( SWE_ITEMDEFINITION );

        final PhenomenonPropertyType phenomenonProp = (PhenomenonPropertyType) itemDef.getProperty( SWE_PROPERTY );
        final PhenomenonType phenomenonValue = phenomenonProp.getPhenomenon().getValue();

        final RepresentationType rep = (RepresentationType) itemDef.getProperty( SWE_REPRESENTATION );

        components.add( new ComponentDefinition( phenomenonValue, rep ) );
      }
    }

    return components.toArray( new ComponentDefinition[components.size()]);
  }

  /**
   * Writes the contents of an observation into a feature. The feature must substitute
   * http://www.opengis.net/om:Observation.
   */
  public void writeObservationToFeature( final IObservation source, final Feature target )
  {

  }
}
