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

import java.math.BigDecimal;
import java.math.BigInteger;

import javax.xml.bind.JAXBContext;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.Duration;
import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.namespace.QName;

import ogc31.www.opengis.net.gml.ConversionToPreferredUnitType;
import ogc31.www.opengis.net.gml.CoverageFunctionType;
import ogc31.www.opengis.net.gml.DerivationUnitTermType;
import ogc31.www.opengis.net.gml.DirectionPropertyType;
import ogc31.www.opengis.net.gml.GridDomainType;
import ogc31.www.opengis.net.gml.LocationPropertyType;
import ogc31.www.opengis.net.gml.RangeSetType;

import org.apache.commons.codec.binary.Base64;
import org.apache.commons.lang.ArrayUtils;
import org.apache.xmlbeans.impl.util.HexBin;
import org.kalypso.commons.xml.NS;
import org.kalypso.contribs.ogc2x.KalypsoOGC2xJAXBcontext;
import org.kalypso.contribs.ogc31.KalypsoOGC31JAXBcontext;
import org.kalypso.gmlschema.basics.JAXBContextProvider;
import org.kalypso.gmlschema.types.GenericBindingTypeHandler;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.ITypeRegistry;
import org.kalypso.gmlschema.types.MetaDataPropertyTypeHandler;
import org.kalypso.gmlschema.types.TypeRegistryException;
import org.kalypso.ogc.swe.RepresentationTypeHandler;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_EnvelopeBindingTypeHandler;
import org.kalypsodeegree.model.geometry.GM_MultiCurve;
import org.kalypsodeegree.model.geometry.GM_MultiPoint;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GenericGM_ObjectBindingTypeHandler;
import org.kalypsodeegree_impl.model.cv.RectifiedGridDomainTypeHandlerGml3;
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
        registry.registerTypeHandler( new XsdBaseTypeHandler<XMLGregorianCalendar>( xmlTypeName, XMLGregorianCalendar.class )
        {
          /**
           * @see org.kalypsodeegree.model.XsdBaseTypeHandler#convertToJavaValue(java.lang.String)
           */
          @Override
          public XMLGregorianCalendar convertToJavaValue( String xmlString )
          {
            return dataTypeFactory.newXMLGregorianCalendar( xmlString );
          }

          /**
           * @see org.kalypsodeegree.model.XsdBaseTypeHandler#convertToXMLString(T)
           */
          @Override
          public String convertToXMLString( XMLGregorianCalendar value )
          {
            return value.toXMLFormat();
          }
        } );
      }

      // <element name="duration" type="duration" />
      registry.registerTypeHandler( new XsdBaseTypeHandler<Duration>( "duration", Duration.class )
      {
        /**
         * @see org.kalypsodeegree.model.XsdBaseTypeHandler#convertToJavaValue(java.lang.String)
         */
        @Override
        public Duration convertToJavaValue( String xmlString )
        {
          return dataTypeFactory.newDuration( xmlString );
        }

        /**
         * @see org.kalypsodeegree.model.XsdBaseTypeHandler#convertToXMLString(T)
         */
        @Override
        public String convertToXMLString( Duration value )
        {
          return value.toString();
        }
      } );

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
        registry.registerTypeHandler( new XsdBaseTypeHandler<String>( xmlTypeName, String.class )
        {
          /**
           * @see org.kalypsodeegree.model.XsdBaseTypeHandler#convertToJavaValue(java.lang.String)
           */
          @Override
          public String convertToJavaValue( final String xmlString )
          {
            return xmlString;
          }

          /**
           * @see org.kalypsodeegree.model.XsdBaseTypeHandler#convertToXMLString(T)
           */
          @Override
          public String convertToXMLString( String value )
          {
            return value;
          }
        } );

      }
      // <element name="ENTITIES" type="ENTITIES" />
      // <element name="NMTOKENS" type="NMTOKENS" />
      final String[] stringArrayTypes = { "ENTITIES", "NMTOKENS" };
      for( final String xmlType : stringArrayTypes )
      {
        registry.registerTypeHandler( new XsdBaseTypeHandler<String[]>( xmlType, String[].class )
        {
          /**
           * @see org.kalypsodeegree.model.XsdBaseTypeHandler#convertToJavaValue(java.lang.String)
           */
          @Override
          public String[] convertToJavaValue( String xmlString )
          {
            return xmlString.split( "/s+" );
          }

          /**
           * @see org.kalypsodeegree.model.XsdBaseTypeHandler#convertToXMLString(T)
           */
          @Override
          public String convertToXMLString( String value[] )
          {
            final StringBuffer result = new StringBuffer();
            for( int k = 0; k < value.length; k++ )
            {
              if( k != 0 )
                result.append( " " );
              result.append( value[k] );
            }
            return result.toString();
          }
        } );
      }

      // <element name="boolean" type="boolean" />
      registry.registerTypeHandler( new XsdBaseTypeHandler<Boolean>( "boolean", Boolean.class )
      {
        /**
         * @see org.kalypsodeegree.model.XsdBaseTypeHandler#convertToJavaValue(java.lang.String)
         */
        @Override
        public Boolean convertToJavaValue( String xmlString )
        {
          return Boolean.parseBoolean( xmlString );
        }

        /**
         * @see org.kalypsodeegree.model.XsdBaseTypeHandler#convertToXMLString(T)
         */
        @Override
        public String convertToXMLString( Boolean value )
        {
          return Boolean.toString( value );
        }
      } );

      // <element name="base64Binary" type="base64Binary" />
      registry.registerTypeHandler( new XsdBaseTypeHandler<Byte[]>( "base64Binary", Byte[].class )
      {
        /**
         * @see org.kalypsodeegree.model.XsdBaseTypeHandler#convertToJavaValue(java.lang.String)
         */
        @Override
        public Byte[] convertToJavaValue( String xmlString )
        {
          final byte[] bytes = xmlString.getBytes();
          final byte[] encodeBase64 = Base64.encodeBase64( bytes );
          return ArrayUtils.toObject( encodeBase64 );
        }

        /**
         * @see org.kalypsodeegree.model.XsdBaseTypeHandler#convertToXMLString(T)
         */
        @Override
        public String convertToXMLString( Byte[] value )
        {
          final byte[] base64Data = ArrayUtils.toPrimitive( value );
          byte[] bytes = Base64.decodeBase64( base64Data );
          return new String( bytes );
        }
      } );

      // <element name="hexBinary" type="hexBinary" />
      registry.registerTypeHandler( new XsdBaseTypeHandler<Byte[]>( "hexBinary", Byte[].class )
      {
        /**
         * @see org.kalypsodeegree.model.XsdBaseTypeHandler#convertToJavaValue(java.lang.String)
         */
        @Override
        public Byte[] convertToJavaValue( String xmlString )
        {
          final byte[] bytes = HexBin.stringToBytes( xmlString );
          return ArrayUtils.toObject( bytes );
        }

        /**
         * @see org.kalypsodeegree.model.XsdBaseTypeHandler#convertToXMLString(T)
         */
        @Override
        public String convertToXMLString( Byte[] value )
        {
          final byte[] bytes = ArrayUtils.toPrimitive( value );
          return HexBin.bytesToString( bytes );
        }
      } );

      // <element name="float" type="float" />
      registry.registerTypeHandler( new XsdBaseTypeHandler<Float>( "float", Float.class )
      {
        /**
         * @see org.kalypsodeegree.model.XsdBaseTypeHandler#convertToJavaValue(java.lang.String)
         */
        @Override
        public Float convertToJavaValue( String xmlString )
        {
          return Float.valueOf( xmlString );
        }

        /**
         * @see org.kalypsodeegree.model.XsdBaseTypeHandler#convertToXMLString(T)
         */
        @Override
        public String convertToXMLString( Float value )
        {
          return Float.toString( value );
        }
      } );

      // <element name="decimal" type="decimal" />
      registry.registerTypeHandler( new XsdBaseTypeHandler<BigDecimal>( "decimal", BigDecimal.class )
      {
        /**
         * @see org.kalypsodeegree.model.XsdBaseTypeHandler#convertToJavaValue(java.lang.String)
         */
        @Override
        public BigDecimal convertToJavaValue( String xmlString )
        {
          return new BigDecimal( xmlString );
        }

        /**
         * @see org.kalypsodeegree.model.XsdBaseTypeHandler#convertToXMLString(T)
         */
        @Override
        public String convertToXMLString( final BigDecimal value )
        {
          return value == null ? "" : value.toString();
        }
      } );
      // <element name="integer" type="integer" />
      // <element name="positiveInteger" type="positiveInteger" />
      // <element name="nonPositiveInteger" type="nonPositiveInteger" />
      // <element name="negativeInteger" type="negativeInteger" />
      // <element name="nonNegativeInteger" type="nonNegativeInteger" />
      // <element name="unsignedLong" type="unsignedLong" />
      final String[] bigIntegerTypes = { "integer", "positiveInteger", "nonPositiveInteger", "negativeInteger", "nonNegativeInteger", "unsignedLong" };
      for( final String xmlType : bigIntegerTypes )
      {
        registry.registerTypeHandler( new XsdBaseTypeHandler<BigInteger>( xmlType, BigInteger.class )
        {
          /**
           * @see org.kalypsodeegree.model.XsdBaseTypeHandler#convertToJavaValue(java.lang.String)
           */
          @Override
          public BigInteger convertToJavaValue( String xmlString )
          {
            return new BigInteger( xmlString );
          }

          /**
           * @see org.kalypsodeegree.model.XsdBaseTypeHandler#convertToXMLString(T)
           */
          @Override
          public String convertToXMLString( BigInteger value )
          {
            return value.toString();
          }
        } );

      }
      // <element name="long" type="long" />
      // <element name="unsignedInt" type="unsignedInt" />
      final String[] longTypes = { "long", "unsignedInt" };
      for( String xmlType : longTypes )
      {
        registry.registerTypeHandler( new XsdBaseTypeHandler<Long>( xmlType, Long.class )
        {
          /**
           * @see org.kalypsodeegree.model.XsdBaseTypeHandler#convertToJavaValue(java.lang.String)
           */
          @Override
          public Long convertToJavaValue( String xmlString )
          {
            return Long.valueOf( xmlString );
          }

          /**
           * @see org.kalypsodeegree.model.XsdBaseTypeHandler#convertToXMLString(T)
           */
          @Override
          public String convertToXMLString( Long value )
          {
            return Long.toString( value );
          }
        } );
      }

      // <element name="int" type="int" />
      // <element name="unsignedShort" type="unsignedShort" />
      final String[] integerTypes = { "int", "unsignedShort" };
      for( String xmlType : integerTypes )
      {
        registry.registerTypeHandler( new XsdBaseTypeHandler<Integer>( xmlType, Integer.class )
        {
          /**
           * @see org.kalypsodeegree.model.XsdBaseTypeHandler#convertToJavaValue(java.lang.String)
           */
          @Override
          public Integer convertToJavaValue( String xmlString )
          {
            return Integer.valueOf( xmlString );
          }

          /**
           * @see org.kalypsodeegree.model.XsdBaseTypeHandler#convertToXMLString(T)
           */
          @Override
          public String convertToXMLString( Integer value )
          {
            return Integer.toString( value );
          }
        } );
      }
      // <element name="short" type="short" />
      // <element name="unsignedByte" type="unsignedByte" />
      final String[] shortTypes = { "short", "unsignedByte" };
      for( final String xmlType : shortTypes )
      {
        registry.registerTypeHandler( new XsdBaseTypeHandler<Short>( xmlType, Short.class )
        {
          /**
           * @see org.kalypsodeegree.model.XsdBaseTypeHandler#convertToJavaValue(java.lang.String)
           */
          @Override
          public Short convertToJavaValue( String xmlString )
          {
            return Short.valueOf( xmlString );
          }

          /**
           * @see org.kalypsodeegree.model.XsdBaseTypeHandler#convertToXMLString(T)
           */
          @Override
          public String convertToXMLString( Short value )
          {
            return Integer.toString( value );
          }
        } );
      }

      // <element name="byte" type="byte" />
      registry.registerTypeHandler( new XsdBaseTypeHandler<Byte>( "byte", Byte.class )
      {
        /**
         * @see org.kalypsodeegree.model.XsdBaseTypeHandler#convertToJavaValue(java.lang.String)
         */
        @Override
        public Byte convertToJavaValue( String xmlString )
        {
          return Byte.valueOf( xmlString );
        }

        /**
         * @see org.kalypsodeegree.model.XsdBaseTypeHandler#convertToXMLString(T)
         */
        @Override
        public String convertToXMLString( Byte value )
        {
          return Byte.toString( value );
        }
      } );
      // <element name="double" type="double" />
      registry.registerTypeHandler( new XsdBaseTypeHandler<Double>( "double", Double.class )
      {
        /**
         * @see org.kalypsodeegree.model.XsdBaseTypeHandler#convertToJavaValue(java.lang.String)
         */
        @Override
        public Double convertToJavaValue( String xmlString )
        {
          return Double.valueOf( xmlString );
        }

        /**
         * @see org.kalypsodeegree.model.XsdBaseTypeHandler#convertToXMLString(T)
         */
        @Override
        public String convertToXMLString( Double value )
        {
          return Double.toString( value );
        }
      } );
      // <element name="QName" type="QName" />
      registry.registerTypeHandler( new XsdBaseTypeHandler<QName>( "QName", QName.class )
      {
        /**
         * @see org.kalypsodeegree.model.XsdBaseTypeHandler#convertToJavaValue(java.lang.String)
         */
        @Override
        public QName convertToJavaValue( String xmlString )
        {
          return QName.valueOf( xmlString );
        }

        /**
         * @see org.kalypsodeegree.model.XsdBaseTypeHandler#convertToXMLString(T)
         */
        @Override
        public String convertToXMLString( QName value )
        {
          return value.toString();
        }
      } );
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
        if( gmlVersion == null || gmlVersion.startsWith( "2" ) )
          return KalypsoOGC2xJAXBcontext.getContext();
        else if( gmlVersion.startsWith( "3" ) )
          return KalypsoOGC31JAXBcontext.getContext();
        throw new UnsupportedOperationException( "GMLVersion " + gmlVersion + " is not supported" );
      }
    };
    // geometry types
    registry.registerTypeHandler( new GenericGM_ObjectBindingTypeHandler( jaxbContextProvider, GeometryUtilities.QN_POLYGON_PROPERTY, GeometryUtilities.QN_POLYGON, GM_Surface.class, true ) );

    registry.registerTypeHandler( new GenericGM_ObjectBindingTypeHandler( jaxbContextProvider, GeometryUtilities.QN_POINT_PROPERTY, GeometryUtilities.QN_POINT, GM_Point.class, true ) );
    registry.registerTypeHandler( new GenericGM_ObjectBindingTypeHandler( jaxbContextProvider, GeometryUtilities.QN_LINE_STRING_PROPERTY, GeometryUtilities.QN_LINE_STRING, GM_Curve.class, true ) );
    // registry.registerTypeHandler( new GenericGM_ObjectBindingTypeHandler( context, new QName( NS.GML3,
    // "PolygonPropertyType" ), new QName( NS.GML3, "Polygon" ), GM_Surface.class, true ) );
    registry.registerTypeHandler( new GenericGM_ObjectBindingTypeHandler( jaxbContextProvider, GeometryUtilities.QN_MULTI_POINT_PROPERTY, GeometryUtilities.QN_MULTI_POINT, GM_MultiPoint.class, true ) );
    registry.registerTypeHandler( new GenericGM_ObjectBindingTypeHandler( jaxbContextProvider, GeometryUtilities.QN_MULTI_LINE_STRING_PROPERTY, GeometryUtilities.QN_MULTI_LINE_STRING, GM_MultiCurve.class, true ) );
    registry.registerTypeHandler( new GenericGM_ObjectBindingTypeHandler( jaxbContextProvider, GeometryUtilities.QN_MULTI_POLYGON_PROPERTY, GeometryUtilities.QN_MULTI_POLYGON, GM_MultiSurface.class, true ) );

//    {http://www.opengis.net/gml}GeometryPropertyType
    registry.registerTypeHandler( new GenericGM_ObjectBindingTypeHandler( jaxbContextProvider, new QName(NS.GML3,"GeometryPropertyType"), GeometryUtilities.QN_MULTI_POLYGON, GM_Object.class, true ) );
    // TODO: the next line is wrong; GM_Envelope is no GM_Object
    // registry.registerTypeHandler( new GenericGM_ObjectBindingTypeHandler( jaxbContextProvider, new QName( NS.GML3,
    // "BoundingShapeType" ), new QName( NS.GML3, "Envelope" ), GM_Envelope.class, false ) );
    registry.registerTypeHandler( new GM_EnvelopeBindingTypeHandler( jaxbContextProvider, new QName( NS.GML3, "BoundingShapeType" ), GM_Envelope.class, false ) );

    // other GML3 types:
    registry.registerTypeHandler( new GenericBindingTypeHandler( jaxbContextProvider, GeometryUtilities.QN_LOCATION_PROPERTY, GeometryUtilities.QN_LOCATION, LocationPropertyType.class, false ) );
    registry.registerTypeHandler( new GenericBindingTypeHandler( jaxbContextProvider, GeometryUtilities.QN_DIRECTION_PROPERTY, GeometryUtilities.QN_DIRECTION, DirectionPropertyType.class, false ) );
    registry.registerTypeHandler( new GenericBindingTypeHandler( jaxbContextProvider, new QName( NS.GML3, "RangeSetType" ), new QName( NS.GML3, "rangeSet" ), RangeSetType.class, false, true, false ) );
    registry.registerTypeHandler( new GenericBindingTypeHandler( jaxbContextProvider, new QName( NS.GML3, "CoverageFunctionType" ), new QName( NS.GML3, "coverageFunction" ), CoverageFunctionType.class, false ) );
    registry.registerTypeHandler( new GenericBindingTypeHandler( jaxbContextProvider, new QName( NS.GML3, "GridDomainType" ), new QName( NS.GML3, "gridDomain" ), GridDomainType.class, false ) );
    // We do not use the binding classes, because they do not work
    // registry.registerTypeHandler( new GenericBindingTypeHandler( jaxbContextProvider, new QName( NS.GML3,
    // "RectifiedGridDomainType" ), new QName( NS.GML3, "rectifiedGridDomain" ), RectifiedGridDomainType.class, false )
    // );
    registry.registerTypeHandler( new RectifiedGridDomainTypeHandlerGml3() );

    // for the elements of ConventionalUnitType
    registry.registerTypeHandler( new GenericBindingTypeHandler( jaxbContextProvider, new QName( NS.GML3, "ConversionToPreferredUnitType" ), new QName( NS.GML3, "conversionToPreferredUnit" ), ConversionToPreferredUnitType.class, false, true, false ) );
    registry.registerTypeHandler( new GenericBindingTypeHandler( jaxbContextProvider, new QName( NS.GML3, "RoughConversionToPreferredUnitType" ), new QName( NS.GML3, "roughConversionToPreferredUnit" ), ConversionToPreferredUnitType.class, false, true, false ) );
    registry.registerTypeHandler( new GenericBindingTypeHandler( jaxbContextProvider, new QName( NS.GML3, "DerivationUnitTermType" ), new QName( NS.GML3, "derivationUnitTerm" ), DerivationUnitTermType.class, false, true, false ) );

    registry.registerTypeHandler( new MetaDataPropertyTypeHandler() );

    // swe types
    // registry.registerTypeHandler( new GenericBindingTypeHandler( context, new QName( NS.SWE, "PhenomenonPropertyType"
    // ), new QName( NS.SWE, "observedProperty" ), PhenomenonPropertyType.class, false, false, false ) );
    // registry.registerTypeHandler( new GenericBindingTypeHandler( context, new QName( NS.SWE,
    // "DataDefinitionPropertyType" ), new QName( NS.SWE, "resultDefinition" ), DataDefinitionPropertyType.class, false,
    // false, false ) );
    // registry.registerTypeHandler( new GenericBindingTypeHandler( context, new QName( NS.SWE, "RelativeMeasureType" ),
    // new QName( NS.SWE, "RelativeMeasureType" ), RelativeMeasureType.class, false, true, false ) );

    registry.registerTypeHandler( new RepresentationTypeHandler() );
    // registry.registerTypeHandler( new GenericBindingTypeHandler( context, new QName( NS.SWE, "DataDefinition" ), new
    // QName( NS.SWE, "DataDefinitionType" ), DataDefinitionType.class, false, true ) );
    // registry.registerTypeHandler( new GenericBindingTypeHandler( context, new QName( NS.SWE,
    // "SWE_RecordSchemaPropertyType" ), new QName( NS.SWE, "component" ), SWERecordSchemaPropertyType.class, false,
    // false ) );

    // gmd types
    // registry.registerTypeHandler( new GenericBindingTypeHandler( context, new QName( NS.GMD,
    // "DQ_DataQuality_PropertyType" ), new QName( NS.SWE, "quality" ), DQDataQualityPropertyType.class, false, false )
    // );

    // registry.registerTypeHandler( new GenericBindingTypeHandler( context, new QName( NS.GML3,
    // "TimePrimitivePropertyType" ), new QName( NS.GML3, "validTime" ), TimePrimitivePropertyType.class, false ) );
    // registry.registerTypeHandler( new GenericBindingTypeHandler( context, new QName( NS.GML3, "" ), new QName(
    // NS.GML3, "" ), .class, false ) );
    // registry.registerTypeHandler( new GenericBindingTypeHandler( context, new QName( NS.GML3, "" ), new QName(
    // NS.GML3, "" ), .class, false ) );
    // registry.registerTypeHandler( new GenericBindingTypeHandler( context, new QName( NS.GML3, "" ), new QName(
    // NS.GML3, "" ), .class, false ) );

    // registry.registerTypeHandler( new GenericBindingTypeHandler( context, new QName( NS.GML3, "EnvelopeType" ), new
    // QName( NS.GML3, "Envelope" ), GM_Envelope.class, false ) );

    // registry.registerTypeHandler( new GenericBindingTypeHandler( context,
    // PointType.class, true ) );
    // registry.registerTypeHandler( new GenericBindingTypeHandler( context,
    // PolygonType.class, true ) );
    // registry.registerTypeHandler( new GenericBindingTypeHandler( context,
    // LineStringType.class, true ) );
    // registry.registerTypeHandler( new GenericBindingTypeHandler( context,
    // MultiPointType.class, true ) );
    // registry.registerTypeHandler( new GenericBindingTypeHandler( context,
    // MultiLineStringType.class, true ) );
    // registry.registerTypeHandler( new GenericBindingTypeHandler( context,
    // MultiPolygonType.class, true ) );
    // registry.registerTypeHandler( new GenericBindingTypeHandler( context,
    // EnvelopeType.class, true ) );

    // registry.registerTypeHandler( new GMLBoundingShapeTypeHandler() );
    // final String gmlns = XMLHelper.GMLSCHEMA_NS;
    // final QName[] points = new QName[] { new QName( gmlns,
    // "PointPropertyType" ) };
    // registry.registerTypeHandler( new GM_ObjectTypeHandler( points,
    // GeometryUtilities.getPointClass() ) );
    //
    // final QName[] multiPoints = new QName[] { new QName( gmlns,
    // "MultiPointPropertyType" ) };
    // registry.registerTypeHandler( new GM_ObjectTypeHandler( multiPoints,
    // GeometryUtilities.getMultiPointClass() ) );
    //
    // final QName[] lineString = new QName[] { new QName( gmlns,
    // "LineStringPropertyType" ) };
    // registry.registerTypeHandler( new GM_ObjectTypeHandler( lineString,
    // GeometryUtilities.getLineStringClass() ) );
    //
    // final QName[] multiLineStrings = new QName[] { new QName( gmlns,
    // "MultiLineStringPropertyType" ) };
    // registry.registerTypeHandler( new GM_ObjectTypeHandler(
    // multiLineStrings,
    // GeometryUtilities.getMultiLineStringClass() ) );
    //
    // final QName[] polygon = new QName[] { new QName( gmlns,
    // "PolygonPropertyType" ) };
    // registry.registerTypeHandler( new GM_ObjectTypeHandler( polygon,
    // GeometryUtilities.getPolygonClass() ) );
    //
    // final QName[] multPolygons = new QName[] { new QName( gmlns,
    // "MultiPolygonPropertyType" ) };
    // registry.registerTypeHandler( new GM_ObjectTypeHandler( multPolygons,
    // GeometryUtilities.getMultiPolygonClass() )
    // );
    //
    // // final QName[] geometry = new QName[] { new QName( gmlns,
    // "GeometryAssociationType" ),//
    // // new QName( gmlns, "GeometryPropertyType" ) };
    // // registry.registerTypeHandler( new GM_ObjectTypeHandler( geometry,
    // GeometryUtilities.getUndefinedGeometryClass() )
    // // );
    //
    // // final QName[] multGeometry = new QName[] { new QName(
    // XMLHelper.GMLSCHEMA_NS, "MultiGeometryPropertyType" ) };
    // // registry.registerTypeHandler( new GM_ObjectTypeHandler(
    // // multGeometry,GeometryUtilities.getUndefinedGeometryClass() ) );
    //
  }
}
