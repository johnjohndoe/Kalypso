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

import ogc31.www.opengis.net.gml.CoverageFunctionType;
import ogc31.www.opengis.net.gml.DirectionPropertyType;
import ogc31.www.opengis.net.gml.GridDomainType;
import ogc31.www.opengis.net.gml.LocationPropertyType;
import ogc31.www.opengis.net.gml.ObjectFactory;
import ogc31.www.opengis.net.gml.RangeSetType;
import ogc31.www.opengis.net.gml.RectifiedGridDomainType;
import ogc31.www.opengis.net.gml.TimePrimitivePropertyType;

import org.apache.commons.codec.binary.Base64;
import org.apache.commons.lang.ArrayUtils;
import org.apache.xmlbeans.impl.util.HexBin;
import org.kalypso.commons.xml.NS;
import org.kalypso.gmlschema.types.GenericBindingTypeHandler;
import org.kalypso.gmlschema.types.ITypeRegistry;
import org.kalypso.gmlschema.types.TypeRegistryException;
import org.kalypso.jwsdp.JaxbUtilities;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_MultiCurve;
import org.kalypsodeegree.model.geometry.GM_MultiPoint;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GenericGM_ObjectBindingTypeHandler;

/**
 * @author doemming
 */
public class TypeHandlerUtilities
{
  // commn typehandler that will always exist

  // final ITypeHandler stringTH = registry.getTypeHandlerForClassName(
  // String.class );
  // final ITypeHandler integerTH = registry.getTypeHandlerForClassName(
  // Integer.class );
  // final ITypeHandler longTH = registry.getTypeHandlerForClassName(
  // Long.class );
  // final ITypeHandler doubleTH = registry.getTypeHandlerForClassName(
  // Double.class );
  // final ITypeHandler floatTH = registry.getTypeHandlerForClassName(
  // Float.class );
  // final ITypeHandler booleanTH = registry.getTypeHandlerForClassName(
  // Boolean.class );
  // final ITypeHandler dateTH = registry.getTypeHandlerForClassName(
  // Date.class );
  /**
   * simple type handler of build-in XMLSCHEMA types <br>
   * TODO typehandler for lists of simpletypes <br>
   * TODO mixed types typehandler <br>
   * TODO better solution for anytype <br>
   * TODO substitutiongroups of simple types
   */
  public static void registerXSDSimpleTypeHandler( ITypeRegistry registry )
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
      for( int i = 0; i < calendarTypes.length; i++ )
      {
        final String xmlTypeName = calendarTypes[i];

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
      for( int i = 0; i < stringTypes.length; i++ )
      {
        final String xmlTypeName = stringTypes[i];

        registry.registerTypeHandler( new XsdBaseTypeHandler<String>( xmlTypeName, String.class )
        {
          /**
           * @see org.kalypsodeegree.model.XsdBaseTypeHandler#convertToJavaValue(java.lang.String)
           */
          @Override
          public String convertToJavaValue( String xmlString )
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
      for( int i = 0; i < stringArrayTypes.length; i++ )
      {
        final String xmlType = stringArrayTypes[i];

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
            StringBuffer result = new StringBuffer();
            for( int i = 0; i < value.length; i++ )
            {
              if( i != 0 )
                result.append( " " );
              result.append( value[i] );
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
        public String convertToXMLString( BigDecimal value )
        {
          return value.toString();
        }
      } );
      // <element name="integer" type="integer" />
      // <element name="positiveInteger" type="positiveInteger" />
      // <element name="nonPositiveInteger" type="nonPositiveInteger" />
      // <element name="negativeInteger" type="negativeInteger" />
      // <element name="nonNegativeInteger" type="nonNegativeInteger" />
      // <element name="unsignedLong" type="unsignedLong" />
      final String[] bigIntegerTypes = { "integer", "positiveInteger", "nonPositiveInteger", "negativeInteger", "nonNegativeInteger", "unsignedLong" };
      for( int i = 0; i < bigIntegerTypes.length; i++ )
      {
        final String xmlType = bigIntegerTypes[i];
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
      for( int i = 0; i < longTypes.length; i++ )
      {
        final String xmlType = longTypes[i];

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
      for( int i = 0; i < integerTypes.length; i++ )
      {
        final String xmlType = integerTypes[i];
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
      for( int i = 0; i < shortTypes.length; i++ )
      {
        final String xmlType = shortTypes[i];
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

      // registry.registerTypeHandler(new XsdBaseTypeHandler("boolean",
      // Boolean.class));
      // registry.registerTypeHandler(new XsdBaseTypeHandler("integer",
      // BigInteger.class));
      // registry.registerTypeHandler(new XsdBaseTypeHandler("int",
      // Integer.class));
      // registry.registerTypeHandler(new XsdBaseTypeHandler("integer",
      // Integer.class));
      // registry.registerTypeHandler(new XsdBaseTypeHandler("long",
      // Long.class));
      // registry.registerTypeHandler(new XsdBaseTypeHandler("double",
      // Double.class));
      // registry.registerTypeHandler(new XsdBaseTypeHandler("date",
      // XMLGregorianCalendar.class));
      // registry.registerTypeHandler(new XsdBaseTypeHandler("decimal",
      // BigDecimal.class));

      // registry.registerTypeHandler( new XSDDateTypeHandler() ); // date
      // registry.registerTypeHandler( new XSDDateTimeTypeHandler() ); //
      // datetime
      // registry.registerTypeHandler( new XSDStringTypeHandler() ); //
      // string
      // registry.registerTypeHandler( new XSDBooleanTypeHandler() ); //
      // boolean
      // registry.registerTypeHandler( new XSDIntegerTypeHandler() ); //
      // integer
      // registry.registerTypeHandler( new XSDLongTypeHandler() ); // long
      // registry.registerTypeHandler( new XSDDoubleTypeHandler() ); //

      /*
       * registry.registerTypeHandler(new GenericXMLBeanTypeHandler( XmlGMonthDay.type));
       * registry.registerTypeHandler(new GenericXMLBeanTypeHandler( XmlGDay.type)); registry.registerTypeHandler(new
       * GenericXMLBeanTypeHandler( XmlGMonth.type)); registry.registerTypeHandler(new GenericXMLBeanTypeHandler(
       * XmlGYear.type)); registry.registerTypeHandler(new GenericXMLBeanTypeHandler( XmlGYearMonth.type));
       * registry.registerTypeHandler(new GenericXMLBeanTypeHandler( XmlDate.type)); registry.registerTypeHandler(new
       * GenericXMLBeanTypeHandler( XmlTime.type)); registry.registerTypeHandler(new GenericXMLBeanTypeHandler(
       * XmlDateTime.type)); registry.registerTypeHandler(new GenericXMLBeanTypeHandler( XmlDuration.type));
       * registry.registerTypeHandler(new GenericXMLBeanTypeHandler( XmlString.type)); registry.registerTypeHandler(new
       * GenericXMLBeanTypeHandler( XmlNormalizedString.type)); registry.registerTypeHandler(new
       * GenericXMLBeanTypeHandler( XmlToken.type)); registry.registerTypeHandler(new GenericXMLBeanTypeHandler(
       * XmlLanguage.type)); registry.registerTypeHandler(new GenericXMLBeanTypeHandler( XmlName.type));
       * registry.registerTypeHandler(new GenericXMLBeanTypeHandler( XmlNCName.type)); registry.registerTypeHandler(new
       * GenericXMLBeanTypeHandler( XmlID.type)); registry.registerTypeHandler(new GenericXMLBeanTypeHandler(
       * XmlIDREF.type)); registry.registerTypeHandler(new GenericXMLBeanTypeHandler( XmlIDREFS.type));
       * registry.registerTypeHandler(new GenericXMLBeanTypeHandler( XmlENTITY.type)); registry.registerTypeHandler(new
       * GenericXMLBeanTypeHandler( XmlENTITIES.type)); registry.registerTypeHandler(new GenericXMLBeanTypeHandler(
       * XmlNMTOKEN.type)); registry.registerTypeHandler(new GenericXMLBeanTypeHandler( XmlNMTOKENS.type));
       * registry.registerTypeHandler(new GenericXMLBeanTypeHandler( XmlBoolean.type)); registry.registerTypeHandler(new
       * GenericXMLBeanTypeHandler( XmlBase64Binary.type)); registry.registerTypeHandler(new GenericXMLBeanTypeHandler(
       * XmlHexBinary.type)); registry.registerTypeHandler(new GenericXMLBeanTypeHandler( XmlFloat.type));
       * registry.registerTypeHandler(new GenericXMLBeanTypeHandler( XmlDecimal.type)); registry.registerTypeHandler(new
       * GenericXMLBeanTypeHandler( XmlInteger.type)); registry.registerTypeHandler(new GenericXMLBeanTypeHandler(
       * XmlNonPositiveInteger.type)); registry.registerTypeHandler(new GenericXMLBeanTypeHandler(
       * XmlNegativeInteger.type)); registry.registerTypeHandler(new GenericXMLBeanTypeHandler( XmlLong.type));
       * registry.registerTypeHandler(new GenericXMLBeanTypeHandler( XmlInt.type)); registry.registerTypeHandler(new
       * GenericXMLBeanTypeHandler( XmlShort.type)); registry.registerTypeHandler(new GenericXMLBeanTypeHandler(
       * XmlByte.type)); registry.registerTypeHandler(new GenericXMLBeanTypeHandler( XmlNonNegativeInteger.type));
       * registry.registerTypeHandler(new GenericXMLBeanTypeHandler( XmlUnsignedLong.type));
       * registry.registerTypeHandler(new GenericXMLBeanTypeHandler( XmlUnsignedInt.type));
       * registry.registerTypeHandler(new GenericXMLBeanTypeHandler( XmlUnsignedShort.type));
       * registry.registerTypeHandler(new GenericXMLBeanTypeHandler( XmlUnsignedByte.type));
       * registry.registerTypeHandler(new GenericXMLBeanTypeHandler( XmlPositiveInteger.type));
       * registry.registerTypeHandler(new GenericXMLBeanTypeHandler( XmlDouble.type)); registry.registerTypeHandler(new
       * GenericXMLBeanTypeHandler( XmlAnyURI.type)); registry.registerTypeHandler(new GenericXMLBeanTypeHandler(
       * XmlQName.type)); registry.registerTypeHandler(new GenericXMLBeanTypeHandler( XmlNOTATION.type));
       */
      // registry.registerTypeHandler( new XSDDateTypeHandler() ); // date
      // registry.registerTypeHandler( new XSDDateTimeTypeHandler() ); //
      // datetime
      // registry.registerTypeHandler( new XSDStringTypeHandler() ); //
      // string
      // registry.registerTypeHandler( new XSDBooleanTypeHandler() ); //
      // boolean
      // registry.registerTypeHandler( new XSDIntegerTypeHandler() ); //
      // integer
      // registry.registerTypeHandler( new XSDLongTypeHandler() ); // long
      // registry.registerTypeHandler( new XSDDoubleTypeHandler() ); //
      // double
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

  }

  /**
   * type handler for GML3 types
   */
  public static void registerGeometryGML2typeHandler( final ITypeRegistry registry ) throws TypeRegistryException
  {
    final JAXBContext context = JaxbUtilities.createQuiet( ObjectFactory.class );
    // geometrie types
    registry.registerTypeHandler( new GenericGM_ObjectBindingTypeHandler( context, new QName( NS.GML3, "PointPropertyType" ), new QName( NS.GML3, "Point" ), GM_Point.class, true ) );
    registry.registerTypeHandler( new GenericGM_ObjectBindingTypeHandler( context, new QName( NS.GML3, "LineStringPropertyType" ), new QName( NS.GML3, "LineString" ), GM_Curve.class, true ) );
    registry.registerTypeHandler( new GenericGM_ObjectBindingTypeHandler( context, new QName( NS.GML3, "PolygonPropertyType" ), new QName( NS.GML3, "Polygon" ), GM_Surface.class, true ) );
    registry.registerTypeHandler( new GenericGM_ObjectBindingTypeHandler( context, new QName( NS.GML3, "MultiPointPropertyType" ), new QName( NS.GML3, "MultiPoint" ), GM_MultiPoint.class, true ) );
    registry.registerTypeHandler( new GenericGM_ObjectBindingTypeHandler( context, new QName( NS.GML3, "MultiLineStringPropertyType" ), new QName( NS.GML3, "MultiLineString" ), GM_MultiCurve.class, true ) );
    registry.registerTypeHandler( new GenericGM_ObjectBindingTypeHandler( context, new QName( NS.GML3, "MultiPolygonPropertyType" ), new QName( NS.GML3, "MultiPolygon" ), GM_MultiSurface.class, true ) );
    registry.registerTypeHandler( new GenericGM_ObjectBindingTypeHandler( context, new QName( NS.GML3, "BoundingShapeType" ), new QName( NS.GML3, "Envelope" ), GM_Envelope.class, false ) );

    // other GML3 types:
    registry.registerTypeHandler( new GenericBindingTypeHandler( context, new QName( NS.GML3, "LocationPropertyType" ), new QName( NS.GML3, "location" ), LocationPropertyType.class, false ) );
    registry.registerTypeHandler( new GenericBindingTypeHandler( context, new QName( NS.GML3, "DirectionPropertyType" ), new QName( NS.GML3, "direction" ), DirectionPropertyType.class, false ) );
    registry.registerTypeHandler( new GenericBindingTypeHandler( context, new QName( NS.GML3, "RangeSetType" ), new QName( NS.GML3, "rangeSet" ), RangeSetType.class, false ) );
    registry.registerTypeHandler( new GenericBindingTypeHandler( context, new QName( NS.GML3, "CoverageFunctionType" ), new QName( NS.GML3, "coverageFunction" ), CoverageFunctionType.class, false ) );
    registry.registerTypeHandler( new GenericBindingTypeHandler( context, new QName( NS.GML3, "GridDomainType" ), new QName( NS.GML3, "gridDomain" ), GridDomainType.class, false ) );
    registry.registerTypeHandler( new GenericBindingTypeHandler( context, new QName( NS.GML3, "RectifiedGridDomainType" ), new QName( NS.GML3, "rectifiedGridDomain" ), RectifiedGridDomainType.class, false ) );
    //    registry.registerTypeHandler( new GenericBindingTypeHandler( context, new QName( NS.GML3, "TimePrimitivePropertyType" ), new QName( NS.GML3, "validTime" ), TimePrimitivePropertyType.class, false ) );
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
