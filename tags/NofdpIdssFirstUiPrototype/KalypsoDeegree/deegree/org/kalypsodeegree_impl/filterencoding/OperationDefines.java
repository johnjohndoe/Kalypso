/*--------------- Kalypso-Deegree-Header ------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 
 history:
 
 Files in this package are originally taken from deegree and modified here
 to fit in kalypso. As goals of kalypso differ from that one in deegree
 interface-compatibility to deegree is wanted but not retained always. 
 
 If you intend to use this software in other ways than in kalypso 
 (e.g. OGC-web services), you should consider the latest version of deegree,
 see http://www.deegree.org .

 all modifications are licensed as deegree, 
 original copyright:
 
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon GmbH
 http://www.lat-lon.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypsodeegree_impl.filterencoding;

import java.util.HashMap;
import java.util.Map;

/**
 * Defines codes and constants for easy coping with the different kinds of Operations (both XML-Entities & JavaObjects).
 * 
 * @author Markus Schneider
 * @version 06.08.2002
 */
public class OperationDefines
{

  // used to associate names with the OperationInfos
  private static Map<String, OperationInfo> names = null;

  // used to associate ids (Integers) with the OperationInfos
  private static Map<Integer, OperationInfo> ids = null;

  // different types of operations
  public static final int TYPE_SPATIAL = 0;

  public static final int TYPE_COMPARISON = 1;

  public static final int TYPE_LOGICAL = 2;

  public static final int TYPE_UNKNOWN = -1;

  // spatial operations
  public static final int EQUALS = 0;

  public static final int DISJOINT = 1;

  public static final int INTERSECTS = 2;

  public static final int TOUCHES = 3;

  public static final int CROSSES = 4;

  public static final int WITHIN = 5;

  public static final int CONTAINS = 6;

  public static final int OVERLAPS = 7;

  public static final int BEYOND = 8;

  public static final int BBOX = 9;

  // calvin added on 10/21/2003
  public static final int DWITHIN = 10;

  // comparison operations
  public static final int PROPERTYISEQUALTO = 100;

  public static final int PROPERTYISLESSTHAN = 101;

  public static final int PROPERTYISGREATERTHAN = 102;

  public static final int PROPERTYISLESSTHANOREQUALTO = 103;

  public static final int PROPERTYISGREATERTHANOREQUALTO = 104;

  public static final int PROPERTYISLIKE = 105;

  public static final int PROPERTYISNULL = 106;

  public static final int PROPERTYISBETWEEN = 107;

  // logical operations
  public static final int AND = 200;

  public static final int OR = 201;

  public static final int NOT = 202;

  public static final int UNKNOWN = -1;

  static
  {
    if( names == null )
      buildHashMaps();
  }

  /**
   * Returns the type of an operation for a given name.
   * 
   * @return TYPE_SPATIAL / TYPE_COMPARISON / TYPE_LOGICAL / TYPE_UNKNOWN
   */
  public static int getTypeByName( String name )
  {
    OperationInfo operationInfo = names.get( name.toLowerCase() );
    if( operationInfo == null )
      return TYPE_UNKNOWN;
    return operationInfo.m_type;
  }

  /**
   * Returns the id of an operation for a given name.
   * 
   * @return BBOX / PROPERTYISEQUAL / AND / ...
   */
  public static int getIdByName( String name )
  {
    OperationInfo operationInfo = names.get( name.toLowerCase() );
    if( operationInfo == null )
      return UNKNOWN;
    return operationInfo.m_id;
  }

  /**
   * Returns the type of an operation for a given id.
   * 
   * @return TYPE_SPATIAL / TYPE_COMPARISON / TYPE_LOGICAL / TYPE_UNKNOWN
   */
  public static int getTypeById( int id )
  {
    OperationInfo operationInfo = ids.get( new Integer( id ) );
    if( operationInfo == null )
      return TYPE_UNKNOWN;
    return operationInfo.m_type;
  }

  /**
   * Returns the name of an operation for a given id.
   * 
   * @return null / Name of operation
   */
  public static String getNameById( int id )
  {

    OperationInfo operationInfo = ids.get( new Integer( id ) );
    if( operationInfo == null )
      return null;
    return operationInfo.m_name;
  }

  private static void addOperationInfo( int id, String name, int type )
  {
    OperationInfo operationInfo = new OperationInfo( id, type, name );
    names.put( name, operationInfo );
    names.put( name.toLowerCase(), operationInfo );
    names.put( name.toUpperCase(), operationInfo );
    ids.put( new Integer( id ), operationInfo );
  }

  private static void buildHashMaps( )
  {
    names = new HashMap<String, OperationInfo>( 25 );
    ids = new HashMap<Integer, OperationInfo>( 25 );

    addOperationInfo( BBOX, "BBOX", TYPE_SPATIAL );
    addOperationInfo( EQUALS, "Equals", TYPE_SPATIAL );
    addOperationInfo( DISJOINT, "Disjoint", TYPE_SPATIAL );
    addOperationInfo( INTERSECTS, "Intersects", TYPE_SPATIAL );
    addOperationInfo( TOUCHES, "Touches", TYPE_SPATIAL );
    addOperationInfo( CROSSES, "Crosses", TYPE_SPATIAL );
    addOperationInfo( WITHIN, "Within", TYPE_SPATIAL );
    addOperationInfo( CONTAINS, "Contains", TYPE_SPATIAL );
    addOperationInfo( OVERLAPS, "Overlaps", TYPE_SPATIAL );
    addOperationInfo( BEYOND, "Beyond", TYPE_SPATIAL );
    addOperationInfo( DWITHIN, "DWithin", TYPE_SPATIAL );

    addOperationInfo( PROPERTYISEQUALTO, "PropertyIsEqualTo", TYPE_COMPARISON );
    addOperationInfo( PROPERTYISLESSTHAN, "PropertyIsLessThan", TYPE_COMPARISON );
    addOperationInfo( PROPERTYISGREATERTHAN, "PropertyIsGreaterThan", TYPE_COMPARISON );
    addOperationInfo( PROPERTYISLESSTHANOREQUALTO, "PropertyIsLessThanOrEqualTo", TYPE_COMPARISON );
    addOperationInfo( PROPERTYISGREATERTHANOREQUALTO, "PropertyIsGreaterThanOrEqualTo", TYPE_COMPARISON );
    addOperationInfo( PROPERTYISLIKE, "PropertyIsLike", TYPE_COMPARISON );
    addOperationInfo( PROPERTYISNULL, "PropertyIsNull", TYPE_COMPARISON );
    addOperationInfo( PROPERTYISBETWEEN, "PropertyIsBetween", TYPE_COMPARISON );

    addOperationInfo( AND, "And", TYPE_LOGICAL );
    addOperationInfo( OR, "Or", TYPE_LOGICAL );
    addOperationInfo( NOT, "Not", TYPE_LOGICAL );
  }
  
}

class OperationInfo
{
  int m_id;

  int m_type;

  String m_name;

  OperationInfo( int id, int type, String name )
  {
    m_id = id;
    m_type = type;
    m_name = name;
  }
}