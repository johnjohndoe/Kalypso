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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.StringTokenizer;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IAdapterFactory;
import org.kalypso.commons.metadata.MetadataObject;
import org.kalypso.commons.xml.NS;
import org.kalypso.commons.xml.XmlTypes;
import org.kalypso.contribs.java.xml.XMLUtilities;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.ITypeRegistry;
import org.kalypso.gmlschema.types.MarshallingTypeRegistrySingleton;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.Observation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.swe.RepresentationType;
import org.kalypso.ogc.swe.RepresentationType.KIND;
import org.kalypsodeegree.model.XsdBaseTypeHandler;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;

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

    final TupleResult tupleResult = buildTupleResult( f );

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
  protected static TupleResult buildTupleResult( final Feature f )
  {
    final Feature recordDefinition = FeatureHelper.resolveLink( f, OM_RESULTDEFINITION );

    final String resultRaw = (String) f.getProperty( OM_RESULT );
    final String result = resultRaw == null ? "" : resultRaw.replace( XMLUtilities.CDATA_BEGIN, "" ).replace( XMLUtilities.CDATA_END, "" );

    final IComponent[] components = buildComponents( recordDefinition );
    final XsdBaseTypeHandler[] typeHandlers = typeHandlersForComponents( components );

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
      final IComponent component = components[nb];
      final XsdBaseTypeHandler handler = typeHandlers[nb];
      try
      {
        final Object value;
        if( "null".equals( token ) )
          value = null;
        else
          value = handler.convertToJavaValue( token );
        record.setValue( component, value );
      }
      catch( final NumberFormatException e )
      {
        // TODO: set null here: Problem: the other components can't handle null now, they should
        record.setValue( component, null );
      }

      nb++;
      nb = nb % components.length;
    }

    return tupleResult;
  }

  private static XsdBaseTypeHandler[] typeHandlersForComponents( final IComponent[] components )
  {
    final XsdBaseTypeHandler[] typeHandlers = new XsdBaseTypeHandler[components.length];
    final ITypeRegistry<IMarshallingTypeHandler> typeRegistry = MarshallingTypeRegistrySingleton.getTypeRegistry();
    for( int i = 0; i < components.length; i++ )
    {
      final IComponent component = components[i];
      final QName valueTypeName = component.getValueTypeName();
      final IMarshallingTypeHandler handler = typeRegistry.getTypeHandlerForTypeName( valueTypeName );
      if( handler instanceof XsdBaseTypeHandler )
        typeHandlers[i] = (XsdBaseTypeHandler) handler;
    }
    return typeHandlers;
  }

  public static IComponent[] componentsFromFeature( final Feature f )
  {
    final IFeatureType featureType = f.getFeatureType();

    if( !GMLSchemaUtilities.substitutes( featureType, OM_OBSERVATION ) )
      throw new IllegalArgumentException( "Feature ist not an Observation: " + f );

    final Feature recordDefinition = getOrCreateRecordDefinition( f );
    return buildComponents( recordDefinition );
  }

  /**
   * Helper: builds the list of component definitions defined as ItemDefinitions within the recordDefinition element.
   * <p>
   * This method is declared protected, but if the need emanes, it could be made public.
   */
  protected static IComponent[] buildComponents( final Feature recordDefinition )
  {
    if( recordDefinition == null )
      return new IComponent[0];

    final List<IComponent> components = new ArrayList<IComponent>();

    final FeatureList comps = (FeatureList) recordDefinition.getProperty( SWE_COMPONENT );
    for( int i = 0; i < comps.size(); i++ )
    {
      final Feature itemDef = FeatureHelper.getFeature( recordDefinition.getWorkspace(), comps.get( i ) );

      components.add( new FeatureComponent( itemDef ) );
    }

    return components.toArray( new IComponent[components.size()] );
  }

  /**
   * Writes the contents of a tuple based observation into a feature. The feature must substitute
   * http://www.opengis.net/om:Observation.
   */
  public static void toFeature( final IObservation<TupleResult> source, final Feature targetObsFeature )
  {
    final FeatureChange[] changes = toFeatureAsChanges( source, targetObsFeature );
    for( final FeatureChange change : changes )
      change.getFeature().setProperty( change.getProperty(), change.getNewValue() );
  }

  public static FeatureChange[] toFeatureAsChanges( final IObservation<TupleResult> source, final Feature targetObsFeature )
  {
    final IFeatureType featureType = targetObsFeature.getFeatureType();

    if( !GMLSchemaUtilities.substitutes( featureType, OM_OBSERVATION ) )
      throw new IllegalArgumentException( "Feature ist not an Observation: " + targetObsFeature );

    final List<FeatureChange> changes = new ArrayList<FeatureChange>();

    changes.add( new FeatureChange( targetObsFeature, featureType.getProperty( GML_NAME ), Collections.singletonList( source.getName() ) ) );
    changes.add( new FeatureChange( targetObsFeature, featureType.getProperty( GML_DESCRIPTION ), source.getDescription() ) );

    final List<MetadataObject> mdList = source.getMetadataList();
    changes.add( new FeatureChange( targetObsFeature, featureType.getProperty( GML_METADATA ), mdList ) );

    changes.add( new FeatureChange( targetObsFeature, featureType.getProperty( OM_OBSERVED_PROP ), source.getPhenomenon() ) );

    final TupleResult result = source.getResult();

    final IComponent[] components = result.getComponents();

    final Feature rd = buildRecordDefinition( targetObsFeature, components );
    changes.add( new FeatureChange( targetObsFeature, featureType.getProperty( OM_RESULTDEFINITION ), rd ) );

    final String strResult = serializeResultAsString( result );
    changes.add( new FeatureChange( targetObsFeature, featureType.getProperty( OM_RESULT ), strResult ) );

    return changes.toArray( new FeatureChange[changes.size()] );
  }

  /**
   * Helper: builds the record definition according to the components of the tuple result.
   * 
   * @param map
   *          ATTENTION: the recordset is written in the same order as this map
   */
  public static Feature buildRecordDefinition( final Feature targetObsFeature, final IComponent[] components )
  {
    final IGMLSchema schema = targetObsFeature.getWorkspace().getGMLSchema();

    // set resultDefinition property, create RecordDefinition feature
    final Feature featureRD = targetObsFeature.getWorkspace().createFeature( targetObsFeature, schema.getFeatureType( SWE_RECORDDEFINITIONTYPE ) );

    // for each component, set a component property, create a feature: ItemDefinition
    for( final IComponent comp : components )
    {
      final Feature featureItemDef = itemDefinitionFromComponent( targetObsFeature, schema, comp );

      FeatureHelper.addProperty( featureRD, SWE_COMPONENT, featureItemDef );
    }

    return featureRD;
  }

  private static Feature itemDefinitionFromComponent( final Feature recordDefinition, final IGMLSchema schema, final IComponent comp )
  {
    if( comp instanceof FeatureComponent )
    {
      final FeatureComponent fc = (FeatureComponent) comp;
      final Feature itemDef = fc.getItemDefinition();

      // TODO: clone feature and set new parent

      return itemDef;
    }

    final Feature itemDefinition = recordDefinition.getWorkspace().createFeature( recordDefinition, schema.getFeatureType( SWE_ITEMDEFINITION ) );

    /* Phenomenon */
    final Feature featurePhenomenon = recordDefinition.getWorkspace().createFeature( recordDefinition, schema.getFeatureType( SWE_PHENOMENONTYPE ) );
    FeatureHelper.addProperty( featurePhenomenon, GML_NAME, comp.getName() );
    featurePhenomenon.setProperty( GML_DESCRIPTION, comp.getDescription() );
    itemDefinition.setProperty( SWE_PROPERTY, featurePhenomenon );

    /* Representation type */
    final RepresentationType rt = createRepresentationType( comp );
    itemDefinition.setProperty( SWE_REPRESENTATION, rt );

    return itemDefinition;
  }

  /**
   * Creates an instance of ComponentDefinition that best fits the given component
   */
  public static RepresentationType createRepresentationType( final IComponent component )
  {
    if( component == null )
      throw new IllegalArgumentException( "component is null" );

    final QName valueTypeName = component.getValueTypeName();

    final String classification = "";

    final String unit = component.getUnit();

    final String frame = component.getFrame();

    return new RepresentationType( toKind( valueTypeName ), valueTypeName, unit, frame, classification );
  }

  /**
   * Finds the best KIND that suits the given QName
   */
  private static KIND toKind( final QName valueTypeName )
  {
    if( XmlTypes.XS_BOOLEAN.equals( valueTypeName ) )
      return KIND.Boolean;

    if( XmlTypes.isNumber( valueTypeName ) )
      return KIND.Number;

    if( XmlTypes.isDate( valueTypeName ) )
      return KIND.SimpleType;

    return KIND.Word;
  }

  public static String serializeResultAsString( final TupleResult result )
  {
    final StringBuffer buffer = new StringBuffer();

    final IComponent[] components = result.getComponents();

    final XsdBaseTypeHandler[] handlers = typeHandlersForComponents( components );

    for( final IRecord record : result )
    {
      for( int i = 0; i < components.length; i++ )
      {
        final XsdBaseTypeHandler handler = handlers[i];
        final IComponent comp = components[i];

        if( comp != components[0] )
          buffer.append( " " );

        final Object value = record.getValue( comp );
        final String strValue;
        if( value == null )
          strValue = "null";
        else
          strValue = handler.convertToXMLString( value );

        buffer.append( strValue );
      }

      buffer.append( "\n" );
    }

    return XMLUtilities.encapsulateInCDATA( buffer.toString() );
  }

  /**
   * TODO do not directly return an observation, but rather an observation provider
   * <p>
   * TODO do not create an observation twice for the same feature, pooling?
   * </p>
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

  public static IComponent createDictionaryComponent( final Feature obsFeature, final String dictUrn )
  {
    final Feature recordDefinition = getOrCreateRecordDefinition( obsFeature );

    final IGMLSchema schema = obsFeature.getWorkspace().getGMLSchema();
    final IFeatureType featureType = schema.getFeatureType( SWE_ITEMDEFINITION );

    final Feature itemDef = new XLinkedFeature_Impl( recordDefinition, featureType, dictUrn, null, null, null, null, null );
    return new FeatureComponent( itemDef );
  }

  private static Feature getOrCreateRecordDefinition( final Feature obsFeature )
  {
    final Feature recordDefinition = FeatureHelper.resolveLink( obsFeature, OM_RESULTDEFINITION );
    /* Make sure there is always a record definition */
    if( recordDefinition == null )
    {
      final Feature rd = buildRecordDefinition( obsFeature, new IComponent[] {} );
      obsFeature.setProperty( OM_RESULTDEFINITION, rd );
      return rd;
    }
    return recordDefinition;
  }
}
