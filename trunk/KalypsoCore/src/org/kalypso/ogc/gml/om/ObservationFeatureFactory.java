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

import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.StringTokenizer;

import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IAdapterFactory;
import org.kalypso.commons.xml.NS;
import org.kalypso.commons.xml.XmlTypes;
import org.kalypso.core.KalypsoCoreExtensions;
import org.kalypso.core.i18n.Messages;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.gmlschema.property.restriction.IRestriction;
import org.kalypso.gmlschema.swe.RepresentationType;
import org.kalypso.gmlschema.swe.RepresentationType.KIND;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.ITypeRegistry;
import org.kalypso.gmlschema.types.MarshallingTypeRegistrySingleton;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.Observation;
import org.kalypso.observation.phenomenon.DictionaryPhenomenon;
import org.kalypso.observation.phenomenon.IPhenomenon;
import org.kalypso.observation.phenomenon.Phenomenon;
import org.kalypso.observation.phenomenon.PhenomenonUtilities;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.typeHandler.XsdBaseTypeHandler;
import org.kalypsodeegree.model.typeHandler.XsdBaseTypeHandlerString;
import org.kalypsodeegree.model.typeHandler.XsdBaseTypeHandlerXMLGregorianCalendar;
import org.kalypsodeegree_impl.gml.binding.commons.NamedFeatureHelper;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;

/**
 * @author schlienger
 */
public class ObservationFeatureFactory implements IAdapterFactory
{
  public final static QName GML_METADATA = new QName( NS.GML3, "metaDataProperty" ); //$NON-NLS-1$

  public final static QName OM_OBSERVATION = new QName( NS.OM, "Observation" ); //$NON-NLS-1$

  public final static QName OM_OBSERVED_PROP = new QName( NS.OM, "observedProperty" ); //$NON-NLS-1$

  public final static QName OM_RESULT = new QName( NS.OM, "result" ); //$NON-NLS-1$

  public final static QName OM_RESULTDEFINITION = new QName( NS.OM, "resultDefinition" ); //$NON-NLS-1$

  public final static QName SWE_COMPONENT = new QName( NS.SWE, "component" ); //$NON-NLS-1$

  public final static QName SWE_ITEMDEFINITION = new QName( NS.SWE, "ItemDefinition" ); //$NON-NLS-1$

  public final static QName SWE_PROPERTY = new QName( NS.SWE, "property" ); //$NON-NLS-1$

  public final static QName SWE_PHENOMENONTYPE = new QName( NS.SWE, "Phenomenon" ); //$NON-NLS-1$

  public final static QName SWE_RECORDDEFINITIONTYPE = new QName( NS.SWE, "RecordDefinition" ); //$NON-NLS-1$

  public final static QName SWE_REPRESENTATION = new QName( NS.SWE, "representation" ); //$NON-NLS-1$

  private static final QName QNAME_F_SORTED_RECORD_DEFINITION = new QName( NS.SWE_EXTENSIONS, "SortedRecordDefinition" ); //$NON-NLS-1$

  private static final QName QNAME_P_SORTED_COMPONENT = new QName( NS.SWE_EXTENSIONS, "sortedComponent" ); //$NON-NLS-1$

  /**
   * Makes a tuple based observation from a feature. The feature must substitute http://www.opengis.net/om:Observation .
   */
  public static IObservation<TupleResult> toObservation( final Feature f )
  {
    final IFeatureType featureType = f.getFeatureType();

    if( !GMLSchemaUtilities.substitutes( featureType, ObservationFeatureFactory.OM_OBSERVATION ) )
      throw new IllegalArgumentException( Messages.getString( "org.kalypso.ogc.gml.om.ObservationFeatureFactory.16" ) + f ); //$NON-NLS-1$

    final String name = (String) FeatureHelper.getFirstProperty( f, Feature.QN_NAME );
    final String desc = (String) FeatureHelper.getFirstProperty( f, Feature.QN_DESCRIPTION );

    final Object phenProp = f.getProperty( ObservationFeatureFactory.OM_OBSERVED_PROP );

    final Feature phenFeature = FeatureHelper.getFeature( f.getWorkspace(), phenProp );
    final IPhenomenon phenomenon;
    if( phenFeature != null )
    {
      final String phenId = phenFeature instanceof XLinkedFeature_Impl ? ((XLinkedFeature_Impl) phenFeature).getHref() : phenFeature.getId();
      final String phenName = NamedFeatureHelper.getName( phenFeature );
      final String phenDesc = NamedFeatureHelper.getDescription( phenFeature );
      phenomenon = new Phenomenon( phenId, phenName, phenDesc );
    }
    else
      phenomenon = null;

    final TupleResult tupleResult = ObservationFeatureFactory.buildTupleResult( f );

    final IObservation<TupleResult> observation = new Observation<TupleResult>( name, desc, tupleResult );
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
    final Feature recordDefinition = FeatureHelper.resolveLink( f, ObservationFeatureFactory.OM_RESULTDEFINITION );

    final String result = (String) f.getProperty( ObservationFeatureFactory.OM_RESULT );

    final IComponent[] components = ObservationFeatureFactory.buildComponents( recordDefinition );
    final IComponent[] sortComponents = ObservationFeatureFactory.buildSortComponents( recordDefinition );
    final XsdBaseTypeHandler< ? >[] typeHandlers = ObservationFeatureFactory.typeHandlersForComponents( components );

    final TupleResult tupleResult = new TupleResult( components );
    tupleResult.setSortComponents( sortComponents );

    // TODO: move into own method
    // TODO: be more robust against inconsistency between resultDefinition and result
    final StringTokenizer tk = new StringTokenizer( result == null ? "" : result.trim() ); //$NON-NLS-1$
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
      final XsdBaseTypeHandler< ? > handler = typeHandlers[nb];

      if( handler == null )
        continue;
      try
      {
        Object value = null;
        if( "null".equals( token ) ) //$NON-NLS-1$
          value = null;
        else // TODO fabrication method needed!
        if( handler instanceof XsdBaseTypeHandlerString )
        {
          final XsdBaseTypeHandlerString myHandler = (XsdBaseTypeHandlerString) handler;
          value = myHandler.convertToJavaValue( URLDecoder.decode( token, "UTF-8" ) ); //$NON-NLS-1$
        }
        else if( handler instanceof XsdBaseTypeHandlerXMLGregorianCalendar )
        {
          final XsdBaseTypeHandlerXMLGregorianCalendar myHandler = (XsdBaseTypeHandlerXMLGregorianCalendar) handler;
          value = myHandler.convertToJavaValue( URLDecoder.decode( token, "UTF-8" ) ); //$NON-NLS-1$
        }
        else
        {

          value = handler.convertToJavaValue( token );
        }

        record.setValue( nb, value );
      }
      catch( final NumberFormatException e )
      {
        e.printStackTrace();
        // TODO: set null here: Problem: the other components can't handle null now, they should
        record.setValue( nb, null );
      }
      catch( final UnsupportedEncodingException e )
      {
        e.printStackTrace();
        record.setValue( nb, null );
      }
      nb++;
      nb = nb % components.length;
    }

    return tupleResult;
  }

  /**
   * Creates the list of components the observation should be sorted by. Returns always null, excpet redordDefinition is
   * of type sweExt:SortedRecordDefinition.
   */
  private static IComponent[] buildSortComponents( final Feature recordDefinition )
  {
    if( recordDefinition == null || !GMLSchemaUtilities.substitutes( recordDefinition.getFeatureType(), ObservationFeatureFactory.QNAME_F_SORTED_RECORD_DEFINITION ) )
      return new IComponent[0];

    final List<IComponent> components = new ArrayList<IComponent>();

    final FeatureList comps = (FeatureList) recordDefinition.getProperty( ObservationFeatureFactory.QNAME_P_SORTED_COMPONENT );
    for( final Object object : comps )
    {
      final Feature itemDef = FeatureHelper.getFeature( recordDefinition.getWorkspace(), object );

      components.add( new FeatureComponent( itemDef ) );
    }

    return components.toArray( new IComponent[components.size()] );
  }

  public static XsdBaseTypeHandler< ? >[] typeHandlersForComponents( final IComponent[] components )
  {
    final XsdBaseTypeHandler< ? >[] typeHandlers = new XsdBaseTypeHandler[components.length];

    for( int i = 0; i < components.length; i++ )
      typeHandlers[i] = ObservationFeatureFactory.typeHanderForComponent( components[i] );

    return typeHandlers;
  }

  public static final XsdBaseTypeHandler< ? > typeHanderForComponent( final IComponent component )
  {
    final ITypeRegistry<IMarshallingTypeHandler> typeRegistry = MarshallingTypeRegistrySingleton.getTypeRegistry();

    /*
     * Get the marshaller via the component handler mechanism. Only take a default handler based on the type if no such
     * handler was found.
     */
    final IComponentHandler compHandler = KalypsoCoreExtensions.findComponentHandler( component.getId() );
    if( compHandler != null )
      return compHandler.getTypeHandler();

    final QName valueTypeName = component.getValueTypeName();

    // REMARK: special case for boolean values, as their string represantation is not always
    // 'true' or 'false'
    if( valueTypeName.equals( new QName( NS.XSD_SCHEMA, "boolean" ) ) ) //$NON-NLS-1$
    {
      // TODO: we need to return a special type handler, that serializes according to the enumeration restriction
      // of the component
      // At the moment we still have the old behaviour: we read/write only 'false' and 'true'
    }

    final IMarshallingTypeHandler handler = typeRegistry.getTypeHandlerForTypeName( valueTypeName );
    if( handler instanceof XsdBaseTypeHandler )
      return (XsdBaseTypeHandler< ? >) handler;

    return null;
  }

  public static IComponent[] componentsFromFeature( final Feature f )
  {
    final IFeatureType featureType = f.getFeatureType();

    if( !GMLSchemaUtilities.substitutes( featureType, ObservationFeatureFactory.OM_OBSERVATION ) )
      throw new IllegalArgumentException( Messages.getString( "org.kalypso.ogc.gml.om.ObservationFeatureFactory.22" ) + f ); //$NON-NLS-1$

    final Feature recordDefinition = ObservationFeatureFactory.getOrCreateRecordDefinition( f );
    return ObservationFeatureFactory.buildComponents( recordDefinition );
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

    final FeatureList comps = (FeatureList) recordDefinition.getProperty( ObservationFeatureFactory.SWE_COMPONENT );
    for( final Object object : comps )
    {
      final Feature itemDef = FeatureHelper.getFeature( recordDefinition.getWorkspace(), object );

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
    final FeatureChange[] changes = ObservationFeatureFactory.toFeatureAsChanges( source, targetObsFeature );
    for( final FeatureChange change : changes )
      change.getFeature().setProperty( change.getProperty(), change.getNewValue() );
  }

  public static FeatureChange[] toFeatureAsChanges( final IObservation<TupleResult> source, final Feature targetObsFeature )
  {
    final IFeatureType featureType = targetObsFeature.getFeatureType();

    if( !GMLSchemaUtilities.substitutes( featureType, ObservationFeatureFactory.OM_OBSERVATION ) )
      throw new IllegalArgumentException( Messages.getString( "org.kalypso.ogc.gml.om.ObservationFeatureFactory.23" ) + targetObsFeature ); //$NON-NLS-1$

    final List<FeatureChange> changes = new ArrayList<FeatureChange>();

    changes.add( new FeatureChange( targetObsFeature, featureType.getProperty( Feature.QN_NAME ), Collections.singletonList( source.getName() ) ) );
    changes.add( new FeatureChange( targetObsFeature, featureType.getProperty( Feature.QN_DESCRIPTION ), source.getDescription() ) );

    // TODO: at the moment, only referenced phenomenons are supported
    final IRelationType phenPt = (IRelationType) featureType.getProperty( ObservationFeatureFactory.OM_OBSERVED_PROP );
    final IPhenomenon phenomenon = source.getPhenomenon();
    final Object phenomenonRef;
    if( phenomenon == null )
      phenomenonRef = null;
    else
      phenomenonRef = FeatureHelper.createLinkToID( phenomenon.getID(), targetObsFeature, phenPt, phenPt.getTargetFeatureType() );

    changes.add( new FeatureChange( targetObsFeature, phenPt, phenomenonRef ) );

    final TupleResult result = source.getResult();

    final IComponent[] components = result.getComponents();
    final IComponent[] sortComponents = result.getSortComponents();

    final IRelationType targetObsFeatureRelation = (IRelationType) featureType.getProperty( ObservationFeatureFactory.OM_RESULTDEFINITION );
    final Feature rd = ObservationFeatureFactory.buildRecordDefinition( targetObsFeature, targetObsFeatureRelation, components, sortComponents );
    changes.add( new FeatureChange( targetObsFeature, targetObsFeatureRelation, rd ) );

    final String strResult = ObservationFeatureFactory.serializeResultAsString( result );
    changes.add( new FeatureChange( targetObsFeature, featureType.getProperty( ObservationFeatureFactory.OM_RESULT ), strResult ) );

    return changes.toArray( new FeatureChange[changes.size()] );
  }

  /**
   * Helper: builds the record definition according to the components of the tuple result.
   * 
   * @param map
   *          ATTENTION: the recordset is written in the same order as this map
   */
  public static Feature buildRecordDefinition( final Feature targetObsFeature, final IRelationType targetObsFeatureRelation, final IComponent[] components, final IComponent[] sortComponents )
  {
    final IGMLSchema schema = targetObsFeature.getWorkspace().getGMLSchema();

    final IFeatureType recordDefinitionFT;
    if( sortComponents == null || sortComponents.length == 0 )
      recordDefinitionFT = schema.getFeatureType( ObservationFeatureFactory.SWE_RECORDDEFINITIONTYPE );
    else
      recordDefinitionFT = schema.getFeatureType( ObservationFeatureFactory.QNAME_F_SORTED_RECORD_DEFINITION );

    // set resultDefinition property, create RecordDefinition feature
    final Feature featureRD = targetObsFeature.getWorkspace().createFeature( targetObsFeature, targetObsFeatureRelation, recordDefinitionFT );
    final IRelationType itemDefRelation = (IRelationType) featureRD.getFeatureType().getProperty( ObservationFeatureFactory.SWE_COMPONENT );

    // for each component, set a component property, create a feature: ItemDefinition
    for( final IComponent comp : components )
    {
      final Feature featureItemDef = ObservationFeatureFactory.itemDefinitionFromComponent( featureRD, itemDefRelation, schema, comp );

      NamedFeatureHelper.setName( featureItemDef, comp.getName() );
      NamedFeatureHelper.setDescription( featureItemDef, comp.getDescription() );

      FeatureHelper.addProperty( featureRD, ObservationFeatureFactory.SWE_COMPONENT, featureItemDef );
    }

    if( sortComponents != null && sortComponents.length > 0 )
    {
      final IRelationType sortedItemDefRelation = (IRelationType) featureRD.getFeatureType().getProperty( ObservationFeatureFactory.QNAME_P_SORTED_COMPONENT );
      for( final IComponent comp : sortComponents )
      {
        final Feature featureItemDef = ObservationFeatureFactory.itemDefinitionFromComponent( featureRD, sortedItemDefRelation, schema, comp );
        FeatureHelper.addProperty( featureRD, ObservationFeatureFactory.QNAME_P_SORTED_COMPONENT, featureItemDef );
      }
    }

    return featureRD;
  }

  private static Feature itemDefinitionFromComponent( final Feature recordDefinition, final IRelationType itemDefinitionRelation, final IGMLSchema schema, final IComponent comp )
  {
    // TODO set name and description

    final String id = comp.getId();
    // try to find a dictionary entry for this component, if it exists, create xlinked-feature to it
    final XLinkedFeature_Impl xlink = new XLinkedFeature_Impl( recordDefinition, itemDefinitionRelation, schema.getFeatureType( ObservationFeatureFactory.SWE_ITEMDEFINITION ), id, "", "", "", "", "" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
    if( xlink.getFeature() != null )
      return xlink;

    if( comp instanceof FeatureComponent )
    {
      // TODO: dangerous, we should always create a new feature here, don't we?

      final FeatureComponent fc = (FeatureComponent) comp;
      return fc.getItemDefinition();
    }

    final Feature itemDefinition = recordDefinition.getWorkspace().createFeature( recordDefinition, itemDefinitionRelation, schema.getFeatureType( ObservationFeatureFactory.SWE_ITEMDEFINITION ) );

    /* Phenomenon */
    final IRelationType phenomenonRelation = (IRelationType) itemDefinition.getFeatureType().getProperty( ObservationFeatureFactory.SWE_PROPERTY );
    final IPhenomenon phenomenon = comp.getPhenomenon();

    final Feature featurePhenomenon = PhenomenonUtilities.createPhenomenonFeature( phenomenon, itemDefinition, phenomenonRelation );
    // FIXME: hack, ask gernot - dictionary components were wrongly written to resultDefinition
    if( phenomenon instanceof DictionaryPhenomenon )
      return featurePhenomenon;

    itemDefinition.setProperty( phenomenonRelation, featurePhenomenon );

    /* Representation type */
    final RepresentationType rt = ObservationFeatureFactory.createRepresentationType( comp );
    itemDefinition.setProperty( ObservationFeatureFactory.SWE_REPRESENTATION, rt );

    return itemDefinition;
  }

  /**
   * Creates an instance of ComponentDefinition that best fits the given component
   */
  public static RepresentationType createRepresentationType( final IComponent component )
  {
    if( component == null )
      throw new IllegalArgumentException( Messages.getString( "org.kalypso.ogc.gml.om.ObservationFeatureFactory.29" ) ); //$NON-NLS-1$

    final QName valueTypeName = component.getValueTypeName();

    final String classification = ""; //$NON-NLS-1$

    final String unit = component.getUnit();

    final String frame = component.getFrame();

    final IRestriction[] restrictions = component.getRestrictions();

    return new RepresentationType( ObservationFeatureFactory.toKind( valueTypeName ), valueTypeName, unit, frame, restrictions, classification );
  }

  /**
   * Finds the best KIND that suits the given QName
   */
  private static KIND toKind( final QName valueTypeName )
  {
    if( XmlTypes.XS_BOOLEAN.equals( valueTypeName ) )
      return KIND.Boolean;

    if( valueTypeName.equals( XmlTypes.XS_DOUBLE ) )
    {
      // TODO: also check, if comp only contains min/maxXXclusive restrictions, else it is no number
// if( XmlTypes.isNumber( valueTypeName ) )
      return KIND.Number;
    }

    if( XmlTypes.isNumber( valueTypeName ) || XmlTypes.isDate( valueTypeName ) )
      return KIND.SimpleType;

    return KIND.Word;
  }

  public static String serializeResultAsString( final TupleResult result )
  {
    final StringBuffer buffer = new StringBuffer();

    final IComponent[] components = result.getComponents();

    final XsdBaseTypeHandler< ? >[] handlers = ObservationFeatureFactory.typeHandlersForComponents( components );

    for( final IRecord record : result )
    {
      for( int i = 0; i < components.length; i++ )
      {
        final XsdBaseTypeHandler< ? > handler = handlers[i];
        final IComponent comp = components[i];

        if( comp != components[0] )
          buffer.append( " " ); //$NON-NLS-1$

        final Object value = record.getValue( i );
        final String bufferValue = ObservationFeatureFactory.recordValueToString( value, handler );
        buffer.append( bufferValue );
      }

      buffer.append( "\n" ); //$NON-NLS-1$
    }

    return buffer.toString();
  }

  private static String recordValueToString( final Object value, final XsdBaseTypeHandler handler )
  {
    if( value == null )
      return "null"; //$NON-NLS-1$

    // REMARK URLEncoder: encoding of xml file is not known - at this point we assume target encoding as "UTF-8".
    // Windows Eclipse often creates Cp-1252 encoded files
    try
    {
      // FIXME implement other type handlers over an fabrication method
      if( handler instanceof XsdBaseTypeHandlerString )
      {
        final XsdBaseTypeHandlerString myHandler = (XsdBaseTypeHandlerString) handler;
        return myHandler.convertToXMLString( URLEncoder.encode( (String) value, "UTF-8" ) ); //$NON-NLS-1$
      }

      if( handler instanceof XsdBaseTypeHandlerXMLGregorianCalendar )
      {
        final XsdBaseTypeHandlerXMLGregorianCalendar myHandler = (XsdBaseTypeHandlerXMLGregorianCalendar) handler;
        final XMLGregorianCalendar cal = (XMLGregorianCalendar) value;
        return URLEncoder.encode( myHandler.convertToXMLString( cal ), "UTF-8" ); //$NON-NLS-1$
      }

      return handler.convertToXMLString( value );
    }
    catch( final UnsupportedEncodingException e )
    {
      e.printStackTrace();
    }
    return null;
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
      return ObservationFeatureFactory.toObservation( (Feature) adaptableObject );

    return null;
  }

  /**
   * @see org.eclipse.core.runtime.IAdapterFactory#getAdapterList()
   */
  public Class[] getAdapterList( )
  {
    return new Class[] { IObservation.class };
  }

  public static FeatureComponent createDictionaryComponent( final Feature obsFeature, final String dictUrn )
  {
    final Feature recordDefinition = ObservationFeatureFactory.getOrCreateRecordDefinition( obsFeature );

    final IGMLSchema schema = obsFeature.getWorkspace().getGMLSchema();
    final IFeatureType featureType = schema.getFeatureType( ObservationFeatureFactory.SWE_ITEMDEFINITION );

    final IRelationType componentRelation = (IRelationType) recordDefinition.getFeatureType().getProperty( ObservationFeatureFactory.SWE_COMPONENT );

    final Feature itemDef = new XLinkedFeature_Impl( recordDefinition, componentRelation, featureType, dictUrn, null, null, null, null, null );

    final List componentList = (List) recordDefinition.getProperty( componentRelation );
    componentList.add( itemDef );

    return new FeatureComponent( itemDef );
  }

  private static Feature getOrCreateRecordDefinition( final Feature obsFeature )
  {
    final IRelationType resultRelation = (IRelationType) obsFeature.getFeatureType().getProperty( ObservationFeatureFactory.OM_RESULTDEFINITION );
    final Feature recordDefinition = FeatureHelper.resolveLink( obsFeature, ObservationFeatureFactory.OM_RESULTDEFINITION );
    /* Make sure there is always a record definition */
    if( recordDefinition == null )
    {
      final Feature rd = ObservationFeatureFactory.buildRecordDefinition( obsFeature, resultRelation, new IComponent[] {}, null );
      obsFeature.setProperty( ObservationFeatureFactory.OM_RESULTDEFINITION, rd );
      return rd;
    }
    return recordDefinition;
  }
}
