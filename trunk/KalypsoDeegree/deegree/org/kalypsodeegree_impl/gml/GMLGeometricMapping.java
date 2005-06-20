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
package org.kalypsodeegree_impl.gml;

import java.util.HashMap;

/**
 * class used by the GML implementation for mapping formal and descriptive property names.
 * 
 * <p>
 * ----------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version 07.02.2001
 *          <p>
 */
class GMLGeometricMapping
{
  private static HashMap toFN = new HashMap();

  private static HashMap toGeoType = new HashMap();

  //formal names
  public static final String BOUNDEDBY = "gml:boundedBy";

  public static final String POINTPROPERTY = "gml:pointProperty";

  public static final String LINESTRINGPROPERTY = "gml:lineStringProperty";

  public static final String POLYGONPROPERTY = "gml:polygonProperty";

  public static final String GEOMETRYPROPERTY = "gml:geometryProperty";

  public static final String MULTIPOINTPROPERTY = "gml:multiPointProperty";

  public static final String MULTILINESTRINGPROPERTY = "gml:multiLineStringProperty";

  public static final String MULTIPOLYGONPROPERTY = "gml:multiPolygonProperty";

  public static final String MULTIGEOMETRYPROPERTY = "gml:multiGeometryProperty";

  // descriptive names
  public static final String LOCATION = "gml:location";

  public static final String POSITION = "gml:position";

  public static final String CENTEROF = "gml:centerOf";

  public static final String CENTERLINEOF = "gml:centerLineOf";

  public static final String EDGEOF = "gml:edgeOf";

  public static final String EXTENTOF = "gml:extentOf";

  public static final String COVERAGE = "gml:coverage";

  public static final String MULTILOCATION = "gml:multiLocation";

  public static final String MULTIPOSITION = "gml:multiPosition";

  public static final String MULTICENTEROF = "gml:multiCenterOf";

  public static final String MULTICENTERLINEOF = "gml:multiCenterLineOf";

  public static final String MULTIEDGEOF = "gml:multiEdgeOf";

  public static final String MULTIEXTENTOF = "gml:multiExtentOf";

  public static final String MULTICOVERAGE = "gml:multiCoverage";

  //geometry types
  public static final String BOX = "gml:Box";

  public static final String POINT = "gml:Point";

  public static final String LINESTRING = "gml:LineString";

  public static final String POLYGON = "gml:Polygon";

  public static final String MULTIPOINT = "gml:MultiPoint";

  public static final String MULTILINESTRING = "gml:MultiLineString";

  public static final String MULTIPOLYGON = "gml:MultiPolygon";

  public static final String MULTIGEOMETRY = "gml:MultiGeometry";

  static
  {
    toFN.put( LOCATION, POINTPROPERTY );
    toFN.put( POSITION, POINTPROPERTY );
    toFN.put( CENTEROF, POINTPROPERTY );
    toFN.put( POINTPROPERTY, POINTPROPERTY );
    toFN.put( CENTERLINEOF, LINESTRINGPROPERTY );
    toFN.put( EDGEOF, LINESTRINGPROPERTY );
    toFN.put( LINESTRINGPROPERTY, LINESTRINGPROPERTY );
    toFN.put( EXTENTOF, POLYGONPROPERTY );
    toFN.put( COVERAGE, POLYGONPROPERTY );
    toFN.put( POLYGONPROPERTY, POLYGONPROPERTY );
    toFN.put( MULTILOCATION, MULTIPOINTPROPERTY );
    toFN.put( MULTIPOSITION, MULTIPOINTPROPERTY );
    toFN.put( MULTICENTEROF, MULTIPOINTPROPERTY );
    toFN.put( MULTIPOINTPROPERTY, MULTIPOINTPROPERTY );
    toFN.put( MULTICENTERLINEOF, MULTILINESTRINGPROPERTY );
    toFN.put( MULTIEDGEOF, MULTILINESTRINGPROPERTY );
    toFN.put( MULTILINESTRINGPROPERTY, MULTILINESTRINGPROPERTY );
    toFN.put( MULTIEXTENTOF, MULTIPOLYGONPROPERTY );
    toFN.put( MULTICOVERAGE, MULTIPOLYGONPROPERTY );
    toFN.put( MULTIPOLYGONPROPERTY, MULTIPOLYGONPROPERTY );

    toGeoType.put( BOUNDEDBY, BOX );
    toGeoType.put( POINTPROPERTY, POINT );
    toGeoType.put( LINESTRINGPROPERTY, LINESTRING );
    toGeoType.put( POLYGONPROPERTY, POLYGON );
    toGeoType.put( MULTIPOINTPROPERTY, MULTIPOINT );
    toGeoType.put( MULTILINESTRINGPROPERTY, MULTILINESTRING );
    toGeoType.put( MULTIPOLYGONPROPERTY, MULTIPOLYGON );
    toGeoType.put( MULTIGEOMETRYPROPERTY, MULTIGEOMETRY );
  }

  /**
   * returns the formal name of a descriptive property name
   */
  public static String getFormalName( String descriptiveName )
  {
    return (String)toFN.get( descriptiveName );
  }

  /**
   * adds a association between a descriptive name and the formal name
   */
  public static void addDN2FN( String descriptiveName, String formalName )
  {
    toFN.put( descriptiveName, formalName );
  }

  /**
   * returns the name of the geometry type a formal property name is associated with.
   */
  public static String getGeometryType( String formalName )
  {
    return (String)toGeoType.get( formalName );
  }

  /**
   * adds a association between a formal name and a geometry type
   */
  public static void addFN2GT( String formalName, String geoType )
  {
    toGeoType.put( formalName, geoType );
  }
}

/*
 * Changes to this class. What the people haven been up to:
 * 
 * $Log$
 * Revision 1.6  2005/06/20 14:07:46  belger
 * Formatierung
 * Revision 1.5 2005/03/08 11:01:04 doemming *** empty log message ***
 * 
 * Revision 1.4 2005/01/18 12:50:42 doemming *** empty log message ***
 * 
 * Revision 1.3 2004/10/07 14:09:14 doemming *** empty log message ***
 * 
 * Revision 1.1 2004/09/02 23:56:58 doemming *** empty log message *** Revision 1.3 2004/08/31 13:03:30 doemming ***
 * empty log message *** Revision 1.3 2004/04/07 06:43:48 poth no message
 * 
 * Revision 1.2 2003/04/23 15:44:39 poth no message
 * 
 * Revision 1.1.1.1 2002/09/25 16:01:04 poth no message
 * 
 * Revision 1.4 2002/08/19 15:59:29 ap no message
 * 
 * Revision 1.3 2002/08/05 16:11:02 ap no message
 * 
 * Revision 1.2 2002/07/31 06:26:06 ap no message
 *  
 */
