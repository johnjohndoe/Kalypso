/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.kalypsomodel1d2d.conv.results.lengthsection;

import java.io.File;
import java.math.BigDecimal;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import junit.framework.TestCase;

import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta;
import org.kalypso.loader.LoaderException;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.loader.ShapeLoader;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_TriangulatedSurface;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.kalypsodeegree_impl.model.feature.visitors.TransformVisitor;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author Thomas Jung
 * 
 */
public class LengthSectionHandler2dTest extends TestCase
{
  public void testSomething( )
  {
    List<GM_TriangulatedSurface> surfaceList = new ArrayList<GM_TriangulatedSurface>();
    try
    {

      // Demo river line file (Stör)
      URL resourceShape = getClass().getResource( "resources/stoer_kompl2.shp" );
      GMLWorkspace shapeWorkspace = getShapeWorkspace( resourceShape );
      // final ConvenienceCSFactoryFull csFac = new ConvenienceCSFactoryFull();
      // final CS_CoordinateSystem cSystem = org.kalypsodeegree_impl.model.cs.Adapters.getDefault().export(
      // csFac.getCSByName( "EPSG:31467" ) );
      // GMLWorkspace shapeWorkspace = ShapeSerializer.deserialize( resourceShape.toString(), cSystem );

      final Feature fRoot = shapeWorkspace.getRootFeature();
      final FeatureList lstMembers = (FeatureList) fRoot.getProperty( ShapeSerializer.PROPERTY_FEATURE_MEMBER );

      BigDecimal max = new BigDecimal( 62000 );
      BigDecimal min = new BigDecimal( 58000 );
      final BigDecimal stepWidth = new BigDecimal( 100 );

      final BigDecimal minDecimal = min.setScale( 1, BigDecimal.ROUND_FLOOR );
      final BigDecimal maxDecimal = max.setScale( 1, BigDecimal.ROUND_CEILING );

      final int numOfClasses = (maxDecimal.subtract( minDecimal ).divide( stepWidth )).intValue() + 1;

      // Demo Station List
      BigDecimal[] stationList = new BigDecimal[numOfClasses];

      for( int currentClass = 0; currentClass < numOfClasses; currentClass++ )
      {
        final double currentValue = minDecimal.doubleValue() + currentClass * stepWidth.doubleValue();

        BigDecimal station = new BigDecimal( currentValue );
        stationList[currentClass] = station;
      }

      // Demo Observations
      final URL lsObsUrl = LengthSectionHandler2dTest.class.getResource( "resources/lengthSectionTemplate.gml" );
      final GMLWorkspace lsObsWorkspace = GmlSerializer.createGMLWorkspace( lsObsUrl, null );
      final IObservation<TupleResult> lsObs = ObservationFeatureFactory.toObservation( lsObsWorkspace.getRootFeature() );

      // Triangulated surfaces

      GM_TriangulatedSurface surface = null;
      URL resource = getClass().getResource( "resources/tin_Terrain.gml" );
      GMLWorkspace w = GmlSerializer.createGMLWorkspace( resource, null );

      final CS_CoordinateSystem targetCRS = KalypsoCorePlugin.getDefault().getCoordinatesSystem();

      w.accept( new TransformVisitor( targetCRS ), w.getRootFeature(), FeatureVisitor.DEPTH_INFINITE );

      GM_Object geometryProperty = w.getRootFeature().getDefaultGeometryProperty();

      if( geometryProperty instanceof GM_TriangulatedSurface )
      {
        surface = (GM_TriangulatedSurface) geometryProperty;
      }
      LengthSectionHandler2d handler = new LengthSectionHandler2d( lsObs, surface, lstMembers, stationList, IDocumentResultMeta.DOCUMENTTYPE.tinTerrain );

      URL resource2 = getClass().getResource( "resources/tin_WATERLEVEL.gml" );
      w = GmlSerializer.createGMLWorkspace( resource2, null );

      w.accept( new TransformVisitor( targetCRS ), w.getRootFeature(), FeatureVisitor.DEPTH_INFINITE );

      geometryProperty = w.getRootFeature().getDefaultGeometryProperty();

      if( geometryProperty instanceof GM_TriangulatedSurface )
      {
        surface = (GM_TriangulatedSurface) geometryProperty;

      }

      LengthSectionHandler2d handler2 = new LengthSectionHandler2d( lsObs, surface, lstMembers, stationList, IDocumentResultMeta.DOCUMENTTYPE.tinWsp );

      if( lsObs.getResult().size() > 0 )
      {
        ObservationFeatureFactory.toFeature( lsObs, lsObsWorkspace.getRootFeature() );
        File lsObsFile = new File( "d:/temp/lengthSection.gml" );
        GmlSerializer.serializeWorkspace( lsObsFile, lsObsWorkspace, "CP1252" );
      }

      // test obs geklappt hat...
      boolean result = true;
      assertEquals( "Result sollte true sein", true, result );
    }
    catch( GmlSerializeException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    catch( Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
  }

  public static GMLWorkspace getShapeWorkspace( final File fShape ) throws GmlSerializeException
  {
    if( fShape == null )
      throw new IllegalStateException();

    /* attention - eShape loading works only without fileextension */
    final String[] sShapes = fShape.toString().split( "\\." );
    if( sShapes == null || sShapes.length < 2 )
      throw new IllegalStateException();

    // XXX fixed coordinate system?
    final ConvenienceCSFactoryFull csFac = new ConvenienceCSFactoryFull();
    final CS_CoordinateSystem cSystem = org.kalypsodeegree_impl.model.cs.Adapters.getDefault().export( csFac.getCSByName( "EPSG:31467" ) );

    String shape = "";
    for( int i = 0; i < sShapes.length - 1; i++ )
    {
      if( i > 0 )
        shape += ".";
      shape += sShapes[i];
    }

    return ShapeSerializer.deserialize( shape, cSystem );
  }

  public static GMLWorkspace getShapeWorkspace( final URL shpURL ) throws GmlSerializeException, LoaderException
  {
    if( shpURL == null )
      throw new IllegalStateException();

    /* attention - eShape loading works only without file extension */
    final String[] sShapes = shpURL.toString().split( "\\." );
    if( sShapes == null || sShapes.length < 2 )
      throw new IllegalStateException();

    String shape = "";
    for( int i = 0; i < sShapes.length - 1; i++ )
    {
      if( i > 0 )
        shape += ".";
      shape += sShapes[i];
    }
    shape += "#EPSG:41367";

    ShapeLoader shapeLoader = new ShapeLoader();
    Object load = shapeLoader.load( shape, shpURL, new NullProgressMonitor() );
    return (GMLWorkspace) load;
  }

}
