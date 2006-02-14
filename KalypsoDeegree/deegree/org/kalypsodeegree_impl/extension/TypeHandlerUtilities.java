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
package org.kalypsodeegree_impl.extension;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.types.ITypeRegistry;
import org.kalypso.gmlschema.types.TypeRegistryException;
import org.kalypsodeegree_impl.gml.schema.XMLHelper;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * @author doemming
 */
public class TypeHandlerUtilities
{
  // commn typehandler that will always exist

  // final ITypeHandler stringTH = registry.getTypeHandlerForClassName( String.class );
  // final ITypeHandler integerTH = registry.getTypeHandlerForClassName( Integer.class );
  // final ITypeHandler longTH = registry.getTypeHandlerForClassName( Long.class );
  // final ITypeHandler doubleTH = registry.getTypeHandlerForClassName( Double.class );
  // final ITypeHandler floatTH = registry.getTypeHandlerForClassName( Float.class );
  // final ITypeHandler booleanTH = registry.getTypeHandlerForClassName( Boolean.class );
  // final ITypeHandler dateTH = registry.getTypeHandlerForClassName( Date.class );

  public static void registerXSDSimpleTypeHandler( ITypeRegistry registry ) throws TypeRegistryException
  {
    registry.registerTypeHandler( new XSDStringTypeHandler() );
    registry.registerTypeHandler( new XSDIntegerTypeHandler() );
    registry.registerTypeHandler( new XSDDoubleTypeHandler() );
    registry.registerTypeHandler( new XSDLongTypeHandler() );
    registry.registerTypeHandler( new XSDDateTypeHandler() );
    registry.registerTypeHandler( new XSDBooleanTypeHandler() );
    
  }

  public static void registerGeometryGML2typeHandler( ITypeRegistry registry ) throws TypeRegistryException
  {
    registry.registerTypeHandler( new GMLBoundingShapeTypeHandler() );
    final String gmlns = XMLHelper.GMLSCHEMA_NS;
    final QName[] points = new QName[] { new QName( gmlns, "PointPropertyType" ) };
    registry.registerTypeHandler( new GM_ObjectTypeHandler( points, GeometryUtilities.getPointClass() ) );

    final QName[] multiPoints = new QName[] { new QName( gmlns, "MultiPointPropertyType" ) };
    registry.registerTypeHandler( new GM_ObjectTypeHandler( multiPoints, GeometryUtilities.getMultiPointClass() ) );

    final QName[] lineString = new QName[] { new QName( gmlns, "LineStringPropertyType" ) };
    registry.registerTypeHandler( new GM_ObjectTypeHandler( lineString, GeometryUtilities.getLineStringClass() ) );

    final QName[] multiLineStrings = new QName[] { new QName( gmlns, "MultiLineStringPropertyType" ) };
    registry.registerTypeHandler( new GM_ObjectTypeHandler( multiLineStrings, GeometryUtilities.getMultiLineStringClass() ) );

    final QName[] polygon = new QName[] { new QName( gmlns, "PolygonPropertyType" ) };
    registry.registerTypeHandler( new GM_ObjectTypeHandler( polygon, GeometryUtilities.getPolygonClass() ) );

    final QName[] multPolygons = new QName[] { new QName( gmlns, "MultiPolygonPropertyType" ) };
    registry.registerTypeHandler( new GM_ObjectTypeHandler( multPolygons, GeometryUtilities.getMultiPolygonClass() ) );

    // final QName[] geometry = new QName[] { new QName( gmlns, "GeometryAssociationType" ),//
    // new QName( gmlns, "GeometryPropertyType" ) };
    // registry.registerTypeHandler( new GM_ObjectTypeHandler( geometry, GeometryUtilities.getUndefinedGeometryClass() )
    // );

    // final QName[] multGeometry = new QName[] { new QName( XMLHelper.GMLSCHEMA_NS, "MultiGeometryPropertyType" ) };
    // registry.registerTypeHandler( new GM_ObjectTypeHandler(
    // multGeometry,GeometryUtilities.getUndefinedGeometryClass() ) );

  }
}
