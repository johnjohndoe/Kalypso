/*----------------    FILE HEADER  ------------------------------------------

 This file is part of deegree.
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon Fitzke/Fretter/Poth GbR
 http://www.lat-lon.de

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

 Andreas Poth
 lat/lon Fitzke/Fretter/Poth GbR
 Meckenheimer Allee 176
 53115 Bonn
 Germany
 E-Mail: poth@lat-lon.de

 Jens Fitzke
 Department of Geography
 University of Bonn
 Meckenheimer Allee 166
 53115 Bonn
 Germany
 E-Mail: jens.fitzke@uni-bonn.de

 
 ---------------------------------------------------------------------------*/
package org.deegree_impl.model.feature;

import java.io.PrintWriter;
import java.util.Map;
import java.util.StringTokenizer;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureException;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree.model.geometry.GM_Object;
import org.deegree.model.geometry.GM_Position;
import org.deegree.xml.XMLTools;
import org.deegree_impl.model.geometry.GMLAdapter;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.deegree_impl.tools.Debug;
import org.w3c.dom.Element;

/**
 * 
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 */
public class GMLFeatureAdapter
{

  /**
   * exports a Feature to the passed PrintWriter
   * 
   * @param feature
   *          feature to export
   * @param prefixes
   *          prefixes of the elements to be used. gml: will be used for
   *          GML-namespace; xlink: will be used for XLink ns.
   * @param pw
   *          PrintWriter to write to
   */
  public static void export( Feature feature, Map prefixes, PrintWriter pw ) throws FeatureException
  {
    Debug.debugMethodBegin();

    String prefix = "";
    if( prefixes != null && prefixes.get( feature.getFeatureType().getName() ) != null )
    {
      prefix = prefixes.get( feature.getFeatureType().getName() ) + ":";
    }
    String featName = feature.getFeatureType().getName();
    // replace invalid chars for XML elements
    featName = featName.replace( ' ', '_' );
    featName = featName.replace( '/', '.' );
    featName = featName.replace( '\\', '.' );
    featName = featName.replace( ':', '_' );
    String id = feature.getId();
    pw.print( "<" + prefix + featName + " fid=\"" + id + "\">" );

    FeatureTypeProperty[] ftp = feature.getFeatureType().getProperties();
    for( int i = 0; i < ftp.length; i++ )
    {
      String name = ftp[i].getName();
      Object o = feature.getProperty( name );
      name = name.replace( ' ', '_' );
      name = name.replace( '/', '.' );
      pw.print( "<" + prefix + name + ">" );
      if( o == null )
      {
        // nothing
      }
      else if( o instanceof GM_Object )
      {
        printGeometry( (GM_Object)o, pw );
      }
      else
      {
        pw.print( o.toString() );
      }
      pw.print( "</" + prefix + name + ">" );
    }

    pw.print( "</" + prefix + featName + ">" );

    Debug.debugMethodEnd();
  }

  /**
   * prints the passed geometry to the also passed PrintWriter formatted as GML
   * 
   * @param geo
   *          geometry to print/extport
   * @param pw
   *          target of the printing/export
   * @throws FeatureException
   */
  private static void printGeometry( GM_Object geo, PrintWriter pw ) throws FeatureException
  {
    Debug.debugMethodBegin();

    try
    {
      pw.print( GMLAdapter.export( geo ) );
    }
    catch( Exception e )
    {
      throw new FeatureException( "could not export feature to GML", e );
    }

    Debug.debugMethodEnd();
  }

  /**
   * returns an array of GM_Positions created from the passed coordinates
   * 
   * @param element
   *          <coordinates>
   * 
   * @return instance of <tt>GM_Position[]</tt>
   */
  public static GM_Position[] createPositionFromCoordinates( Element element )
  {
    Debug.debugMethodBegin();

    GM_Position[] points = null;

    String ts = XMLTools.getAttrValue( element, "ts" );

    if( ts == null )
    {
      ts = " ";
    }

    String ds = XMLTools.getAttrValue( element, "decimal" );

    if( ds == null )
    {
      ds = ".";
    }

    String cs = XMLTools.getAttrValue( element, "cs" );

    if( cs == null )
    {
      cs = ",";
    }

    String value = XMLTools.getStringValue( element );

    // first tokenizer, tokens the tuples
    StringTokenizer tuple = new StringTokenizer( value, ts );
    points = new GM_Position[tuple.countTokens()];

    int i = 0;

    while( tuple.hasMoreTokens() )
    {
      String s = tuple.nextToken();
      // second tokenizer, tokens the coordinates
      StringTokenizer coort = new StringTokenizer( s, cs );
      double[] p = new double[coort.countTokens()];

      for( int k = 0; k < p.length; k++ )
      {
        p[k] = Double.parseDouble( coort.nextToken() );
      }

      points[i++] = GeometryFactory.createGM_Position( p );
    }

    Debug.debugMethodEnd();
    return points;
  }
}