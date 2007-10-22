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
package org.kalypsodeegree.model;

import javax.xml.bind.JAXBContext;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.namespace.QName;

import ogc31.www.opengis.net.gml.ConversionToPreferredUnitType;
import ogc31.www.opengis.net.gml.DerivationUnitTermType;
import ogc31.www.opengis.net.gml.DirectionPropertyType;
import ogc31.www.opengis.net.gml.RangeSetType;

import org.kalypso.commons.xml.NS;
import org.kalypso.contribs.ogc2x.KalypsoOGC2xJAXBcontext;
import org.kalypso.contribs.ogc31.KalypsoOGC31JAXBcontext;
import org.kalypso.gmlschema.swe.RepresentationTypeHandler;
import org.kalypso.gmlschema.types.GenericBindingTypeHandler;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.ITypeRegistry;
import org.kalypso.gmlschema.types.JAXBContextProvider;
import org.kalypso.gmlschema.types.MetaDataPropertyTypeHandler;
import org.kalypso.gmlschema.types.TypeRegistryException;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_MultiCurve;
import org.kalypsodeegree.model.geometry.GM_MultiPoint;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.typeHandler.GM_EnvelopeBindingTypeHandler;
import org.kalypsodeegree.model.typeHandler.GenericGM_ObjectBindingTypeHandler;
import org.kalypsodeegree.model.typeHandler.TriangulatedSurfaceHandler;
import org.kalypsodeegree.model.typeHandler.XsdBaseTypeHandlerBigDecimal;
import org.kalypsodeegree.model.typeHandler.XsdBaseTypeHandlerBigInteger;
import org.kalypsodeegree.model.typeHandler.XsdBaseTypeHandlerBoolean;
import org.kalypsodeegree.model.typeHandler.XsdBaseTypeHandlerByte;
import org.kalypsodeegree.model.typeHandler.XsdBaseTypeHandlerByteArray;
import org.kalypsodeegree.model.typeHandler.XsdBaseTypeHandlerDirectory;
import org.kalypsodeegree.model.typeHandler.XsdBaseTypeHandlerDouble;
import org.kalypsodeegree.model.typeHandler.XsdBaseTypeHandlerDuration;
import org.kalypsodeegree.model.typeHandler.XsdBaseTypeHandlerFile;
import org.kalypsodeegree.model.typeHandler.XsdBaseTypeHandlerFloat;
import org.kalypsodeegree.model.typeHandler.XsdBaseTypeHandlerHexArray;
import org.kalypsodeegree.model.typeHandler.XsdBaseTypeHandlerInteger;
import org.kalypsodeegree.model.typeHandler.XsdBaseTypeHandlerLong;
import org.kalypsodeegree.model.typeHandler.XsdBaseTypeHandlerQName;
import org.kalypsodeegree.model.typeHandler.XsdBaseTypeHandlerRGB;
import org.kalypsodeegree.model.typeHandler.XsdBaseTypeHandlerShort;
import org.kalypsodeegree.model.typeHandler.XsdBaseTypeHandlerString;
import org.kalypsodeegree.model.typeHandler.XsdBaseTypeHandlerStringArray;
import org.kalypsodeegree.model.typeHandler.XsdBaseTypeHandlerXMLGregorianCalendar;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridDomainTypeHandlerGml3;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * @author doemming
 */
public class TypeHandlerUtilities
{
  /**
   * simple type handler of build-in XMLSCHEMA types <br>
   * TODO typehandler for lists of simpletypes <br>
   * TODO mixed types typehandler <br>
   * TODO better solution for anytype <br>
   * TODO substitutiongroups of simple types
   */
  public static void registerXSDSimpleTypeHandler( final ITypeRegistry<IMarshallingTypeHandler> registry )
  {
    try
    {
      final DatatypeFactory dataTypeFactory = DatatypeFactory.newInstance();

      // <element name="gMonthDay" type="gMonthDay" />
      // <element name="gDay" type="gDay" />
      // <element name="gMonth" type="gMonth" />
      // <element name="gYear" type="gYear" />
      // <element name="gYearMonth" type="gYearMonth" />
      // <element name="date" type="date" />
      // <element name="time" type="time" />
      // <element name="dateTime" type="dateTime" />
      // <element name="dateTime" type="dateTime" />

      final String[] calendarTypes = { "gMonthDay", "gDay", "gMonth", "gYear", "gYearMonth", "date", "time", "dateTime" };
      for( final String xmlTypeName : calendarTypes )
      {
        registry.registerTypeHandler( new XsdBaseTypeHandlerXMLGregorianCalendar( dataTypeFactory, xmlTypeName ) );
      }

      // <element name="duration" type="duration" />
      registry.registerTypeHandler( new XsdBaseTypeHandlerDuration( dataTypeFactory ) );

      // <element name="string" type="string" />
      // <element name="normalizedString" type="normalizedString" />
      // <element name="token" type="token" />
      // <element name="language" type="language" />

      // <element name="Name" type="Name" />
      // <element name="NCName" type="NCName" />
      // <element name="ID" type="ID" />
      // <element name="ENTITY" type="ENTITY" />
      // <element name="NMTOKEN" type="NMTOKEN" />
      // <element name="anyURI" type="anyURI" />
      // <element name="IDREF" type="IDREF" />
      // <element name="IDREFS" type="IDREFS" />
      // <element name="anyType" type="anyType" />

      final String[] stringTypes = { "string", "normalizedString", "token", "language", "Name", "NCName", "ID", "ENTITY", "NMTOKEN", "anyURI", "IDREF", "IDREFS", "anyType" };
      for( final String xmlTypeName : stringTypes )
      {
        registry.registerTypeHandler( new XsdBaseTypeHandlerString( xmlTypeName ) );
      }

      // <element name="ENTITIES" type="ENTITIES" />
      // <element name="NMTOKENS" type="NMTOKENS" />
      final String[] stringArrayTypes = { "ENTITIES", "NMTOKENS" };
      for( final String xmlType : stringArrayTypes )
      {
        registry.registerTypeHandler( new XsdBaseTypeHandlerStringArray( xmlType ) );
      }

      // <element name="boolean" type="boolean" />
      registry.registerTypeHandler( new XsdBaseTypeHandlerBoolean() );

      // <element name="base64Binary" type="base64Binary" />
      registry.registerTypeHandler( new XsdBaseTypeHandlerByteArray() );

      // <element name="hexBinary" type="hexBinary" />
      registry.registerTypeHandler( new XsdBaseTypeHandlerHexArray() );

      // <element name="float" type="float" />
      registry.registerTypeHandler( new XsdBaseTypeHandlerFloat() );

      // <element name="decimal" type="decimal" />
      registry.registerTypeHandler( new XsdBaseTypeHandlerBigDecimal() );

      // <element name="integer" type="integer" />
      // <element name="positiveInteger" type="positiveInteger" />
      // <element name="nonPositiveInteger" type="nonPositiveInteger" />
      // <element name="negativeInteger" type="negativeInteger" />
      // <element name="nonNegativeInteger" type="nonNegativeInteger" />
      // <element name="unsignedLong" type="unsignedLong" />
      final String[] bigIntegerTypes = { "integer", "positiveInteger", "nonPositiveInteger", "negativeInteger", "nonNegativeInteger", "unsignedLong" };
      for( final String xmlType : bigIntegerTypes )
      {
        registry.registerTypeHandler( new XsdBaseTypeHandlerBigInteger( xmlType ) );
      }

      // <element name="long" type="long" />
      // <element name="unsignedInt" type="unsignedInt" />
      final String[] longTypes = { "long", "unsignedInt" };
      for( final String xmlType : longTypes )
      {
        registry.registerTypeHandler( new XsdBaseTypeHandlerLong( xmlType ) );
      }

      // <element name="int" type="int" />
      // <element name="unsignedShort" type="unsignedShort" />
      final String[] integerTypes = { "int", "unsignedShort" };
      for( final String xmlType : integerTypes )
      {
        registry.registerTypeHandler( new XsdBaseTypeHandlerInteger( xmlType ) );
      }

      // <element name="short" type="short" />
      // <element name="unsignedByte" type="unsignedByte" />
      final String[] shortTypes = { "short", "unsignedByte" };
      for( final String xmlType : shortTypes )
      {
        registry.registerTypeHandler( new XsdBaseTypeHandlerShort( xmlType ) );
      }

      // <element name="byte" type="byte" />
      registry.registerTypeHandler( new XsdBaseTypeHandlerByte() );

      // <element name="double" type="double" />
      registry.registerTypeHandler( new XsdBaseTypeHandlerDouble() );

      // <element name="QName" type="QName" />
      registry.registerTypeHandler( new XsdBaseTypeHandlerQName() );

      // <element name="color" type="color" />
      registry.registerTypeHandler( new XsdBaseTypeHandlerRGB() );

      // <element name="directory" type="directory" />
      registry.registerTypeHandler( new XsdBaseTypeHandlerDirectory() );

      // <element name="file" type="file" />
      registry.registerTypeHandler( new XsdBaseTypeHandlerFile() );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  /**
   * Type handler for GML3 types
   */
  public static void registerTypeHandlers( final ITypeRegistry<IMarshallingTypeHandler> registry ) throws TypeRegistryException
  {
    final JAXBContextProvider jaxbContextProvider = new JAXBContextProvider()
    {
      public JAXBContext getJaxBContextForGMLVersion( final String gmlVersion )
      {
        if( (gmlVersion == null) || gmlVersion.startsWith( "2" ) )
        {
          return KalypsoOGC2xJAXBcontext.getContext();
        }
        else if( gmlVersion.startsWith( "3" ) )
        {
          return KalypsoOGC31JAXBcontext.getContext();
        }
        throw new UnsupportedOperationException( "GMLVersion " + gmlVersion + " is not supported" );
      }
    };
    // geometry types
    registry.registerTypeHandler( new GenericGM_ObjectBindingTypeHandler( jaxbContextProvider, GeometryUtilities.QN_POLYGON_PROPERTY, GeometryUtilities.QN_POLYGON, GM_Surface.class, true ) );

    registry.registerTypeHandler( new GenericGM_ObjectBindingTypeHandler( jaxbContextProvider, GeometryUtilities.QN_POINT_PROPERTY, GeometryUtilities.QN_POINT, GM_Point.class, true ) );
    registry.registerTypeHandler( new GenericGM_ObjectBindingTypeHandler( jaxbContextProvider, GeometryUtilities.QN_LINE_STRING_PROPERTY, GeometryUtilities.QN_LINE_STRING, GM_Curve.class, true ) );
    registry.registerTypeHandler( new GenericGM_ObjectBindingTypeHandler( jaxbContextProvider, GeometryUtilities.QN_MULTI_POINT_PROPERTY, GeometryUtilities.QN_MULTI_POINT, GM_MultiPoint.class, true ) );
    registry.registerTypeHandler( new GenericGM_ObjectBindingTypeHandler( jaxbContextProvider, GeometryUtilities.QN_MULTI_LINE_STRING_PROPERTY, GeometryUtilities.QN_MULTI_LINE_STRING, GM_MultiCurve.class, true ) );
    registry.registerTypeHandler( new GenericGM_ObjectBindingTypeHandler( jaxbContextProvider, GeometryUtilities.QN_MULTI_POLYGON_PROPERTY, GeometryUtilities.QN_MULTI_POLYGON, GM_MultiSurface.class, true ) );

    // {http://www.opengis.net/gml}GeometryPropertyType
    registry.registerTypeHandler( new GenericGM_ObjectBindingTypeHandler( jaxbContextProvider, new QName( NS.GML3, "GeometryPropertyType" ), GeometryUtilities.QN_MULTI_POLYGON, GM_Object.class, true ) );
    // TODO: the next line is wrong; GM_Envelope is no GM_Object
    // registry.registerTypeHandler( new GenericGM_ObjectBindingTypeHandler( jaxbContextProvider, new QName( NS.GML3,
    // "BoundingShapeType" ), new QName( NS.GML3, "Envelope" ), GM_Envelope.class, false ) );
    registry.registerTypeHandler( new GM_EnvelopeBindingTypeHandler( jaxbContextProvider, new QName( NS.GML3, "BoundingShapeType" ), GM_Envelope.class, false ) );

    // other GML3 types:
    registry.registerTypeHandler( new GenericGM_ObjectBindingTypeHandler( jaxbContextProvider, GeometryUtilities.QN_LOCATION_PROPERTY, GeometryUtilities.QN_LOCATION, GM_Object.class, true ) );
// registry.registerTypeHandler( new GenericBindingTypeHandler( jaxbContextProvider,
// GeometryUtilities.QN_LOCATION_PROPERTY, GeometryUtilities.QN_LOCATION, LocationPropertyType.class, false ) );
    registry.registerTypeHandler( new GenericBindingTypeHandler( jaxbContextProvider, GeometryUtilities.QN_DIRECTION_PROPERTY, GeometryUtilities.QN_DIRECTION, DirectionPropertyType.class, false ) );
    registry.registerTypeHandler( new GenericBindingTypeHandler( jaxbContextProvider, new QName( NS.GML3, "RangeSetType" ), new QName( NS.GML3, "rangeSet" ), RangeSetType.class, false, true, false ) );

    registry.registerTypeHandler( new TriangulatedSurfaceHandler() );

// registry.registerTypeHandler( new GenericBindingTypeHandler( jaxbContextProvider, new QName( NS.GML3,
// "TrianglePatchArrayPropertyType" ), new QName( NS.GML3, "trianglePatches" ), TrianglePatchArrayPropertyType.class,
// true, true, false ) );

    registry.registerTypeHandler( new RectifiedGridDomainTypeHandlerGml3() );

    // for the elements of ConventionalUnitType
    registry.registerTypeHandler( new GenericBindingTypeHandler( jaxbContextProvider, new QName( NS.GML3, "ConversionToPreferredUnitType" ), new QName( NS.GML3, "conversionToPreferredUnit" ), ConversionToPreferredUnitType.class, false, true, false ) );
    registry.registerTypeHandler( new GenericBindingTypeHandler( jaxbContextProvider, new QName( NS.GML3, "RoughConversionToPreferredUnitType" ), new QName( NS.GML3, "roughConversionToPreferredUnit" ), ConversionToPreferredUnitType.class, false, true, false ) );
    registry.registerTypeHandler( new GenericBindingTypeHandler( jaxbContextProvider, new QName( NS.GML3, "DerivationUnitTermType" ), new QName( NS.GML3, "derivationUnitTerm" ), DerivationUnitTermType.class, false, true, false ) );

    registry.registerTypeHandler( new MetaDataPropertyTypeHandler() );

    registry.registerTypeHandler( new RepresentationTypeHandler() );
  }
}
