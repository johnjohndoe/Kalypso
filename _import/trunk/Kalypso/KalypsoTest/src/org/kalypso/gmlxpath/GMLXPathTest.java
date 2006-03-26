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
package org.kalypso.gmlxpath;

import java.net.URL;
import java.util.List;

import junit.framework.TestCase;

import org.kalypso.KalypsoTest;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPathUtilities;

/**
 * @author doemming
 */
public class GMLXPathTest extends TestCase
{
  /**
   * @see junit.framework.TestCase#setUp()
   */
  @Override
  protected void setUp( ) throws Exception
  {
    KalypsoTest.init();
    super.setUp();
  }

  private String[] getXPathes( )
  {
    return new String[] {// 
    // 
        // tested:

        "id( 'a816c00e010a02356c64000000086e20' )",//
        "id( 'a816c00e010a02356c64000000086e20' ) /name",//
        "FeatureCollection/featureMember/BPlan[ @fid = 'c013800d0109e45d440e00000037a018' ]/gkz",//
        "FeatureCollection/featureMember",//
        "FeatureCollection/featureMember/BPlan[ @fid = 'c013800d0109e45d440e00000037a018' ]",//
    // 
    // not tested:
    // "featureMember[BPlan]",// test deprecated1
    // "./FeatureCollection/featureMember",//
    // "/FeatureCollection/featureMember",//
    // "/FeatureCollection/featureMember[ name() = 'BPlan' ]",//
    // "featureMember[ @id = 'a816c00e0109e45d440e00000037a018' ]",//
    // "#fid#a816c00e010a02356c64000000086e20",// test deprecated2
    // "id( 'a816c00e010a02356c64000000086e20' )",//
    // "featureMember",//
    // "//*[ name() = 'WasserrechtlicheFestsetzungsFlaeche' ]",//
    };
  }

  public void testGMLXPathes( ) throws Exception
  {
    try
    {
      final URL gmlURL = getClass().getResource( "resources/Bplan.xml" );
      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( gmlURL );
      final String[] pathes = getXPathes();
      for( int i = 0; i < pathes.length; i++ )
      {
        final String xPath = pathes[i];
        System.out.println( "\ntesting: " + xPath );
        final GMLXPath featureXPath = new GMLXPath( xPath );
        final Object featureFromPath = GMLXPathUtilities.query( featureXPath, workspace );
        if( featureFromPath == null )
        {
          System.out.println( "result=null" );
          continue;
        }
        // System.out.println( "resultType : " + featureFromPath.getClass().getName() );
        else if( featureFromPath instanceof Feature )
        {
          final Feature fe = (Feature) featureFromPath;
          System.out.println( "Feature: " + fe.getFeatureType().getQName() + "#" + fe.getId() );
        }
        else if( featureFromPath instanceof FeatureList )
        {
          final FeatureList feList = (FeatureList) featureFromPath;
          System.out.println( "FeatureList: " + feList.size() + " x " );
        }
        else if( featureFromPath instanceof List )
        {
          final List list = (List) featureFromPath;
          System.out.println( "List: " + list.size() + " x " );
        }
        else
        {
          System.out.println( featureFromPath.getClass().getSimpleName() + ": '" + featureFromPath.toString() + "'" );
        }
      }
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }
}
