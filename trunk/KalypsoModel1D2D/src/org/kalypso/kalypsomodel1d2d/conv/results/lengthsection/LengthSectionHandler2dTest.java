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

import junit.framework.TestCase;

import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_TriangulatedSurface;
import org.kalypsodeegree_impl.gml.binding.shape.AbstractShape;
import org.kalypsodeegree_impl.gml.binding.shape.ShapeCollection;
import org.kalypsodeegree_impl.model.feature.visitors.TransformVisitor;

/**
 * @author Thomas Jung
 *
 */
public class LengthSectionHandler2dTest extends TestCase
{
  public void testSomething( )
  {
    try
    {
      // Demo river line file (Stör)
      final URL resourceShape = getClass().getResource( "/etc/testdata/data/stoer_kompl2.shp" ); //$NON-NLS-1$

      final ShapeCollection shapeWorkspace = ShapeSerializer.deserialize( resourceShape.toString(), "EPSG:31467" ); //$NON-NLS-1$

      final BigDecimal max = new BigDecimal( 62000 );
      final BigDecimal min = new BigDecimal( 58000 );
      final BigDecimal stepWidth = new BigDecimal( 100 );

      final BigDecimal minDecimal = min.setScale( 1, BigDecimal.ROUND_FLOOR );
      final BigDecimal maxDecimal = max.setScale( 1, BigDecimal.ROUND_CEILING );

      final int numOfClasses = maxDecimal.subtract( minDecimal ).divide( stepWidth ).intValue() + 1;

      // Demo Station List
      final BigDecimal[] stationList = new BigDecimal[numOfClasses];

      for( int currentClass = 0; currentClass < numOfClasses; currentClass++ )
      {
        final double currentValue = minDecimal.doubleValue() + currentClass * stepWidth.doubleValue();

        final BigDecimal station = new BigDecimal( currentValue );
        stationList[currentClass] = station;
      }

      // Demo Observations
      final URL lsObsUrl = LengthSectionHandler2dTest.class.getResource( "/etc/testdata/data/lengthSectionTemplate.gml" ); //$NON-NLS-1$
      final GMLWorkspace lsObsWorkspace = GmlSerializer.createGMLWorkspace( lsObsUrl, null );
      final IObservation<TupleResult> lsObs = ObservationFeatureFactory.toObservation( lsObsWorkspace.getRootFeature() );

      // Triangulated surfaces

      GM_TriangulatedSurface surface = null;
      final URL resource = getClass().getResource( "/etc/testdata/data/tin_Terrain.gml" ); //$NON-NLS-1$
      GMLWorkspace w = GmlSerializer.createGMLWorkspace( resource, null );

      final String targetCRS = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

      w.accept( new TransformVisitor( targetCRS ), w.getRootFeature(), FeatureVisitor.DEPTH_INFINITE );

      GM_Object geometryProperty = w.getRootFeature().getDefaultGeometryPropertyValue();

      if( geometryProperty instanceof GM_TriangulatedSurface )
      {
        surface = (GM_TriangulatedSurface) geometryProperty;
      }

      final IFeatureBindingCollection<AbstractShape> shapes = shapeWorkspace.getShapes();
      final AbstractShape[] allShapes = shapes.toArray( new AbstractShape[shapes.size()] );
      final LengthSectionHandlerParameters data = new LengthSectionHandlerParameters( allShapes, null, null, null, new BigDecimal( 100 ), false );

      LengthSectionHandler2d.handle2DLenghtsection( lsObs, surface, data, stationList, IDocumentResultMeta.DOCUMENTTYPE.tinTerrain, new NullProgressMonitor() );

      final URL resource2 = getClass().getResource( "/etc/testdata/data/tin_WATERLEVEL.gml" ); //$NON-NLS-1$
      w = GmlSerializer.createGMLWorkspace( resource2, null );

      w.accept( new TransformVisitor( targetCRS ), w.getRootFeature(), FeatureVisitor.DEPTH_INFINITE );

      geometryProperty = w.getRootFeature().getDefaultGeometryPropertyValue();

      if( geometryProperty instanceof GM_TriangulatedSurface )
      {
        surface = (GM_TriangulatedSurface) geometryProperty;
      }

      LengthSectionHandler2d.handle2DLenghtsection( lsObs, surface, data, stationList, IDocumentResultMeta.DOCUMENTTYPE.tinWsp, new NullProgressMonitor() );

      if( lsObs.getResult().size() > 0 )
      {
        ObservationFeatureFactory.toFeature( lsObs, lsObsWorkspace.getRootFeature() );
        final File lsObsFile = FileUtilities.getNewTempFile( "lengthSection", "gml" ); //$NON-NLS-1$ //$NON-NLS-2$
        GmlSerializer.serializeWorkspace( lsObsFile, lsObsWorkspace, "CP1252" ); //$NON-NLS-1$
      }

      // test obs geklappt hat...
      final boolean result = true;
      assertEquals( "Result sollte true sein", true, result ); //$NON-NLS-1$
    }
    catch( final GmlSerializeException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    catch( final Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
  }

  public static ShapeCollection getShapeWorkspace( final File fShape ) throws GmlSerializeException
  {
    if( fShape == null )
      throw new IllegalStateException();

    /* attention - eShape loading works only without fileextension */
    final String[] sShapes = fShape.toString().split( "\\." ); //$NON-NLS-1$
    if( sShapes == null || sShapes.length < 2 )
      throw new IllegalStateException();

    // XXX fixed coordinate system?
    final String cSystem = "EPSG:31467"; //$NON-NLS-1$

    String shape = ""; //$NON-NLS-1$
    for( int i = 0; i < sShapes.length - 1; i++ )
    {
      if( i > 0 )
        shape += "."; //$NON-NLS-1$
      shape += sShapes[i];
    }

    return ShapeSerializer.deserialize( shape, cSystem );
  }
}
