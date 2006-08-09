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
package org.kalypso.ogc.gml.om;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.Map.Entry;
import java.util.logging.Logger;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IAdapterFactory;
import org.kalypso.commons.metadata.MetadataObject;
import org.kalypso.commons.xml.NS;
import org.kalypso.contribs.java.xml.XMLUtilities;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.Observation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.swe.RepresentationType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author schlienger
 */
public class ObservationFeatureFactory implements IAdapterFactory
{
  public final static QName GML_NAME = new QName( NS.GML3, "name" );

  public final static QName GML_DESCRIPTION = new QName( NS.GML3, "description" );

  public final static QName GML_METADATA = new QName( NS.GML3, "metaDataProperty" );

  public final static QName OM_OBSERVATION = new QName( NS.OM, "Observation" );

  public final static QName OM_OBSERVED_PROP = new QName( NS.OM, "observedProperty" );

  public final static QName OM_RESULT = new QName( NS.OM, "result" );

  public final static QName OM_RESULTDEFINITION = new QName( NS.OM, "resultDefinition" );

  public final static QName SWE_COMPONENT = new QName( NS.SWE, "component" );

  public final static QName SWE_ITEMDEFINITION = new QName( NS.SWE, "ItemDefinition" );

  public final static QName SWE_PROPERTY = new QName( NS.SWE, "property" );

  public final static QName SWE_PHENOMENONTYPE = new QName( NS.SWE, "Phenomenon" );

  public final static QName SWE_RECORDDEFINITIONTYPE = new QName( NS.SWE, "RecordDefinition" );

  public final static QName SWE_REPRESENTATION = new QName( NS.SWE, "representation" );

  /**
   * Makes a tuple based observation from a feature. The feature must substitute http://www.opengis.net/om:Observation .
   */
  @SuppressWarnings("unchecked")
  public static IObservation<TupleResult> toObservation( final Feature f )
  {
    final IFeatureType featureType = f.getFeatureType();

    if( !GMLSchemaUtilities.substitutes( featureType, OM_OBSERVATION ) )
      throw new IllegalArgumentException( "Feature ist not an Observation: " + f );

    final String name = (String) FeatureHelper.getFirstProperty( f, GML_NAME );
    final String desc = (String) FeatureHelper.getFirstProperty( f, GML_DESCRIPTION );
    final List<MetadataObject> meta = (List<MetadataObject>) f.getProperty( GML_METADATA );

    // TODO: only strings, no linked features are supported now
    final Object phenProp = f.getProperty( OM_OBSERVED_PROP );
    final String phenomenon = phenProp instanceof String ? (String) phenProp : null;

    final Feature recordDefinition = FeatureHelper.resolveLink( f, OM_RESULTDEFINITION );

    final String result = (String) f.getProperty( OM_RESULT );
    final String withoutCDATA = result == null ? "" : result.replace( XMLUtilities.CDATA_BEGIN, "" ).replace( XMLUtilities.CDATA_END, "" );

    final TupleResult tupleResult = buildTupleResult( recordDefinition, withoutCDATA );

    final IObservation<TupleResult> observation = new Observation<TupleResult>( name, desc, tupleResult, meta );
    observation.setPhenomenon( phenomenon );

    return observation;
  }

  /**
   * Helper: builds the tuple result for a tuple based observation according to the record definition encoded as a
   * feature (subtype of ItemDefinition).
   * <p>
   * This method is declared protected, but if the need emanes, it could be made public.
   */
  protected static TupleResult buildTupleResult( final Feature recordDefinition, final String result )
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
      try
      {
        final Object value = definitions[nb].getTypeHandler().convertToJavaValue( token );
        record.setValue( components[nb], value );
      }
      catch( final NumberFormatException e )
      {
        // TODO: set null here: Problem: the other components can't handle null now, they should
        record.setValue( components[nb], null );
      }

      nb++;
      nb = nb % definitions.length;
    }

    return tupleResult;
  }

  /**
   * Helper: builds the list of component definitions defined as ItemDefinitions within the recordDefinition element.
   * <p>
   * This method is declared protected, but if the need emanes, it could be made public.
   */
  protected static ComponentDefinition[] buildComponentDefinitions( final Feature recordDefinition )
  {
    if( recordDefinition == null )
      return new ComponentDefinition[0];

    final List<ComponentDefinition> components = new ArrayList<ComponentDefinition>();

    final FeatureList comps = (FeatureList) recordDefinition.getProperty( SWE_COMPONENT );
    for( int i = 0; i < comps.size(); i++ )
    {
      final Feature itemDef = FeatureHelper.getFeature( recordDefinition.getWorkspace(), comps.get( i ) );

      final Feature phenomenon = FeatureHelper.resolveLink( itemDef, SWE_PROPERTY );
      if( phenomenon == null )
      {
        Logger.getLogger( ObservationFeatureFactory.class.getName() ).warning( "Phenomenon could not be found for ItemDefinition: " + itemDef );
        continue;
      }

      final String name = (String) FeatureHelper.getFirstProperty( phenomenon, GML_NAME );
      final String desc = (String) FeatureHelper.getFirstProperty( phenomenon, GML_DESCRIPTION );

      final RepresentationType rep = (RepresentationType) itemDef.getProperty( SWE_REPRESENTATION );

      components.add( new ComponentDefinition( i, name, desc, rep ) );
    }

    return components.toArray( new ComponentDefinition[components.size()] );
  }

  /**
   * Helper: builds the list of component definitions from the components of the tuple result.
   * <p>
   * This method is declared protected, but if the need emanes, it could be made public.
   */
  protected static Map<IComponent, ComponentDefinition> buildComponentDefinitions( final TupleResult result )
  {
    final IComponent[] components = result.getComponents();

    // REMARK: we use the linked hash map to p�reserve the order of the components
    // its not really importent here, but maybe we shouldn't shuffle it too much
    final Map<IComponent, ComponentDefinition> map = new LinkedHashMap<IComponent, ComponentDefinition>( components.length );

    // for each component, set a component property, create a feature: ItemDefinition
    for( int i = 0; i < components.length; i++ )
      map.put( components[i], ComponentDefinition.create( components[i] ) );

    return map;
  }

  /**
   * Writes the contents of a tuple based observation into a feature. The feature must substitute
   * http://www.opengis.net/om:Observation.
   */
  public static void toFeature( final IObservation<TupleResult> source, final Feature targetObsFeature )
  {
    final IFeatureType featureType = targetObsFeature.getFeatureType();

    if( !GMLSchemaUtilities.substitutes( featureType, OM_OBSERVATION ) )
      throw new IllegalArgumentException( "Feature ist not an Observation: " + targetObsFeature );

    FeatureHelper.addProperty( targetObsFeature, GML_NAME, source.getName() );
    targetObsFeature.setProperty( GML_DESCRIPTION, source.getDescription() );

    final List<MetadataObject> mdList = source.getMetadataList();
    targetObsFeature.setProperty( GML_METADATA, mdList );

    targetObsFeature.setProperty( OM_OBSERVED_PROP, source.getPhenomenon() );

    final TupleResult result = source.getResult();

    final Map<IComponent, ComponentDefinition> map = buildComponentDefinitions( result );

    // REMARK + BUGFIX: the order of the records and the order of the serialized string must
    // be the same!
    // Policy: always use the order of the map
    final Feature rd = buildRecordDefinition( targetObsFeature, map );
    targetObsFeature.setProperty( OM_RESULTDEFINITION, rd );

    final String strResult = serializeResultAsString( result, map );
    targetObsFeature.setProperty( OM_RESULT, strResult );
  }

  /**
   * Helper: builds the record definition according to the components of the tuple result.
   * <p>
   * This method is declared protected, but if the need emanes, it could be made public.
   * 
   * @param map
   *          ATTENTION: the recordset is written in the same order as this map
   */
  protected static Feature buildRecordDefinition( final Feature targetObsFeature, final Map<IComponent, ComponentDefinition> map )
  {
    final IGMLSchema schema = targetObsFeature.getWorkspace().getGMLSchema();

    // set resultDefinition property, create RecordDefinition feature
    final Feature featureRD = targetObsFeature.getWorkspace().createFeature( targetObsFeature, schema.getFeatureType( SWE_RECORDDEFINITIONTYPE ) );

    // for each component, set a component property, create a feature: ItemDefinition
    for( final Map.Entry<IComponent, ComponentDefinition> entry : map.entrySet() )
    {
      final ComponentDefinition compDef = entry.getValue();

      final Feature featureItemDef = targetObsFeature.getWorkspace().createFeature( targetObsFeature, schema.getFeatureType( SWE_ITEMDEFINITION ) );

      final Feature featurePhenomenon = targetObsFeature.getWorkspace().createFeature( targetObsFeature, schema.getFeatureType( SWE_PHENOMENONTYPE ) );
      FeatureHelper.addProperty( featurePhenomenon, GML_NAME, compDef.getName() );
      featurePhenomenon.setProperty( GML_DESCRIPTION, compDef.getDescription() );

      featureItemDef.setProperty( SWE_PROPERTY, featurePhenomenon );
      featureItemDef.setProperty( SWE_REPRESENTATION, compDef.getRepresentationType() );

      FeatureHelper.addProperty( featureRD, SWE_COMPONENT, featureItemDef );
    }

    return featureRD;
  }

  private static String serializeResultAsString( final TupleResult result, final Map<IComponent, ComponentDefinition> map )
  {
    final StringBuffer buffer = new StringBuffer();

    final Set<Entry<IComponent, ComponentDefinition>> entries = map.entrySet();
    final Entry<IComponent, ComponentDefinition> firstEntry = entries.iterator().next();

    for( final IRecord record : result )
    {
      for( final Entry<IComponent, ComponentDefinition> entry : entries )
      {
        if( entry != firstEntry )
          buffer.append( " " );

        final IComponent comp = entry.getKey();

        final Object value = record.getValue( comp );
        final String strValue = map.get( comp ).getTypeHandler().convertToXMLString( value );

        buffer.append( strValue );
      }

      buffer.append( "\n" );
    }

    return XMLUtilities.encapsulateInCDATA( buffer.toString() );
  }

  /**
   * TODO do not directly return an observation, but rather an observation provider TODO do not create an observation
   * twice for the same feature, pooling?
   * 
   * @see org.eclipse.core.runtime.IAdapterFactory#getAdapter(java.lang.Object, java.lang.Class)
   */
  public Object getAdapter( final Object adaptableObject, final Class adapterType )
  {
    if( adapterType == IObservation.class && adaptableObject instanceof Feature )
      return toObservation( (Feature) adaptableObject );

    return null;
  }

  /**
   * @see org.eclipse.core.runtime.IAdapterFactory#getAdapterList()
   */
  public Class[] getAdapterList( )
  {
    final Class[] classes = { IObservation.class };
    return classes;
  }
}
