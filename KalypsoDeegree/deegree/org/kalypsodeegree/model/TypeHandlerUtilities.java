/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 *
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always.
 *
 * If you intend to use this software in other ways than in kalypso
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree,
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */
package org.kalypsodeegree.model;

import javax.xml.bind.JAXBContext;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.namespace.QName;

import ogc31.www.opengis.net.gml.FileType;

import org.kalypso.commons.xml.NS;
import org.kalypso.contribs.ogc2x.KalypsoOGC2xJAXBcontext;
import org.kalypso.contribs.ogc31.KalypsoOGC31JAXBcontext;
import org.kalypso.gmlschema.swe.RepresentationTypeHandler;
import org.kalypso.gmlschema.types.GenericBindingTypeHandler;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.ITypeRegistry;
import org.kalypso.gmlschema.types.JAXBContextProvider;
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
        registry.registerTypeHandler( new XsdBaseTypeHandlerShort( xmlType ) );

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

    // Basic GML 3 types
    registry.registerTypeHandler( new GM_EnvelopeBindingTypeHandler( jaxbContextProvider, new QName( NS.GML3, "BoundingShapeType" ), GM_Envelope.class, false ) );

    // Geometries
    registry.registerTypeHandler( new GenericGM_ObjectBindingTypeHandler( jaxbContextProvider, GeometryUtilities.QN_GEOMETRY, GeometryUtilities.QN_GEOMETRY, GM_Object.class, true ) );
    registry.registerTypeHandler( new GenericGM_ObjectBindingTypeHandler( jaxbContextProvider, GeometryUtilities.QN_SURFACE, GeometryUtilities.QN_SURFACE, GM_Surface.class, true ) );
    registry.registerTypeHandler( new GenericGM_ObjectBindingTypeHandler( jaxbContextProvider, GeometryUtilities.QN_POLYGON, GeometryUtilities.QN_POLYGON, GM_Surface.class, true ) );
    registry.registerTypeHandler( new GenericGM_ObjectBindingTypeHandler( jaxbContextProvider, GeometryUtilities.QN_POINT, GeometryUtilities.QN_POINT, GM_Point.class, true ) );
    registry.registerTypeHandler( new GenericGM_ObjectBindingTypeHandler( jaxbContextProvider, GeometryUtilities.QN_LINE_STRING, GeometryUtilities.QN_LINE_STRING, GM_Curve.class, true ) );
    registry.registerTypeHandler( new GenericGM_ObjectBindingTypeHandler( jaxbContextProvider, GeometryUtilities.QN_CURVE, GeometryUtilities.QN_CURVE, GM_Curve.class, true ) );
    registry.registerTypeHandler( new GenericGM_ObjectBindingTypeHandler( jaxbContextProvider, GeometryUtilities.QN_MULTI_POINT, GeometryUtilities.QN_MULTI_POINT, GM_MultiPoint.class, true ) );
    registry.registerTypeHandler( new GenericGM_ObjectBindingTypeHandler( jaxbContextProvider, GeometryUtilities.QN_MULTI_LINE_STRING, GeometryUtilities.QN_MULTI_LINE_STRING, GM_MultiCurve.class, true ) );
    registry.registerTypeHandler( new GenericGM_ObjectBindingTypeHandler( jaxbContextProvider, GeometryUtilities.QN_MULTI_CURVE, GeometryUtilities.QN_MULTI_CURVE, GM_MultiCurve.class, true ) );
    registry.registerTypeHandler( new GenericGM_ObjectBindingTypeHandler( jaxbContextProvider, GeometryUtilities.QN_MULTI_POLYGON, GeometryUtilities.QN_MULTI_POLYGON, GM_MultiSurface.class, true ) );
    registry.registerTypeHandler( new GenericGM_ObjectBindingTypeHandler( jaxbContextProvider, GeometryUtilities.QN_LOCATION, GeometryUtilities.QN_LOCATION, GM_Object.class, true ) );
    registry.registerTypeHandler( new TriangulatedSurfaceHandler() );

    // Coverages
    registry.registerTypeHandler( new GenericBindingTypeHandler( jaxbContextProvider, new QName( NS.GML3, "RangeSetType" ), new QName( NS.GML3, "File" ), FileType.class, false ) );
    registry.registerTypeHandler( new RectifiedGridDomainTypeHandlerGml3() );

    // http://www.opengis.net/swe
    registry.registerTypeHandler( new RepresentationTypeHandler() );
  }
}
