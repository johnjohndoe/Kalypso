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
package test.org.kalypso.kalypsosimulationmodel;

import java.util.List;

import javax.xml.namespace.QName;

import junit.framework.TestCase;

import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygonCollection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.RoughnessPolygonCollection;
import org.kalypso.kalypsosimulationmodel.schema.UrlCatalogModelSimulationBase;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author antanas
 */
public class TestRoughnessPolygonCollection extends TestCase
{
  public final static QName QNAME_PROP_ROUGHNESSLAYERPOLYNOMCOLLECTION = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "roughnessLayerCollection" ); //$NON-NLS-1$

  public final static QName QNAME_PROP_ROUGHNESSLAYERMEMBER = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "roughnessLayerMember" ); //$NON-NLS-1$

  public final static String cs = "EPSG:31467"; //$NON-NLS-1$

  public final void testRoughnessPolygonCollection( ) throws Exception
  {

    GMLWorkspace workspace = null;
    // GM_Point point = GeometryFactory.createGM_Point( 0.17174177831912307, 0.7929354445797808, cs );
    final GM_Point point = GeometryFactory.createGM_Point( 0.1, 0.53, cs );
    IRoughnessPolygonCollection m_polygonCollection;

    workspace = GmlSerializer.createGMLWorkspace( TestWorkspaces.URL_ROUGHNESS_POLYGON_COLLECTION, null );
    final Feature feature = (Feature) workspace.getRootFeature().getProperty( QNAME_PROP_ROUGHNESSLAYERPOLYNOMCOLLECTION );
    if( feature == null )
    {
      fail( "aaa" ); //$NON-NLS-1$
    }

    m_polygonCollection = new RoughnessPolygonCollection( feature);
    final List<IRoughnessPolygon> polygons = m_polygonCollection.selectRoughnessPolygons( point.getPosition() );
    System.out.println( "" ); //$NON-NLS-1$
    System.out.println( "******************************************************************" ); //$NON-NLS-1$
    System.out.println( "* TESTING FILE: " + TestWorkspaces.URL_ROUGHNESS_POLYGON_COLLECTION ); //$NON-NLS-1$
    System.out.println( "******************************************************************" ); //$NON-NLS-1$
    System.out.println( "******************************************************************" ); //$NON-NLS-1$
    System.out.println( "* TEST: select                                                   *" ); //$NON-NLS-1$
    System.out.println( "******************************************************************" ); //$NON-NLS-1$
    System.out.println( "Point (x,y): (" + point.getX() + ", " + point.getY() + ")" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    System.out.println( "is contained in:" ); //$NON-NLS-1$
    for( int i = 0; i < polygons.size(); i++ )
    {
      System.out.println( (i + 1) + ". - " + polygons.get( i ).getId() ); //$NON-NLS-1$
    }

    if( polygons.size() == 0 )
    {
      System.out.println( " - no polygons contains this point" ); //$NON-NLS-1$
    }

    System.out.println( "******************************************************************" ); //$NON-NLS-1$
    System.out.println( "" ); //$NON-NLS-1$

    final List<IRoughnessPolygon> polygonsList = m_polygonCollection.getOverlappedPolygons();
    System.out.println( "******************************************************************" ); //$NON-NLS-1$
    System.out.println( "* TEST: checksOverlapping                                        *" ); //$NON-NLS-1$
    System.out.println( "******************************************************************" ); //$NON-NLS-1$
    System.out.println( "Overlapping polygons:" ); //$NON-NLS-1$
    for( int i = 0; i < polygonsList.size(); i++ )
    {
      System.out.println( (i + 1) + ". - " + polygonsList.get( i ).getId() ); //$NON-NLS-1$
    }

    if( polygonsList.size() == 0 )
    {
      System.out.println( " - final no overlapping" ); //$NON-NLS-1$
    }
    System.out.println( "******************************************************************" ); //$NON-NLS-1$
    System.out.println( "" ); //$NON-NLS-1$
  }

}
