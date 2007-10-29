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
package org.kalypso.risk.test;

import java.io.File;
import java.net.URL;
import java.util.List;

import junit.framework.TestCase;

import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.grid.AbstractDelegatingGeoGrid;
import org.kalypso.grid.GeoGridException;
import org.kalypso.grid.GeoGridUtilities;
import org.kalypso.grid.IGeoGrid;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.risk.model.schema.binding.ILanduseVectorModel;
import org.kalypso.risk.model.schema.binding.ILandusePolygon;
import org.kalypso.risk.model.schema.binding.IWaterdepthCoverage;
import org.kalypso.risk.model.schema.binding.IWaterdepthCoverageModel;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridDomain;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

/**
 * @author Gernot Belger
 */
public class GridTest extends TestCase
{
  public void testGridProcessing( ) throws Exception
  {
    final URL landuseResource = getClass().getResource( "testData/LanduseVectorModel.gml" );
    final URL landuseDatabaseResource = getClass().getResource( "testData/RasterizationControlModel.gml" );
    final URL wdResource = getClass().getResource( "testData/WaterdepthCoverageModel.gml" );

    /* Read input workspace and coverages */
    final GMLWorkspace landuseWorkspace = GmlSerializer.createGMLWorkspace( landuseResource, null );
    final GMLWorkspace landuseDatabaseWorkspace = GmlSerializer.createGMLWorkspace( landuseDatabaseResource, null );
    final GMLWorkspace wdWorkspace = GmlSerializer.createGMLWorkspace( wdResource, null );
    final ILanduseVectorModel landuseVectorModel = (ILanduseVectorModel) landuseWorkspace.getRootFeature().getAdapter( ILanduseVectorModel.class );
    final IFeatureWrapperCollection<ILandusePolygon> polygonCollection = landuseVectorModel.getLandusePolygonCollection();
    final IWaterdepthCoverageModel waterdepthCoverageModel = (IWaterdepthCoverageModel) wdWorkspace.getRootFeature().getAdapter( IWaterdepthCoverageModel.class );
    final IFeatureWrapperCollection<IWaterdepthCoverage> inputCoverages = waterdepthCoverageModel.getWaterdepthCoverageCollection();

    /* Create output workspace and get handle to coverages */
    final File tmpFile = File.createTempFile( "gridTest", ".gml" );
    final GMLWorkspace outputWorkspace = FeatureFactory.createGMLWorkspace( ICoverageCollection.QNAME, tmpFile.toURL(), null );
    final ICoverageCollection outputCoverages = (ICoverageCollection) outputWorkspace.getRootFeature().getAdapter( ICoverageCollection.class );

    for( final IWaterdepthCoverage inputCoverage : inputCoverages )
    {
      final IGeoGrid inputGrid = GeoGridUtilities.toGrid( inputCoverage );

      final IGeoGrid outputGrid = new AbstractDelegatingGeoGrid( inputGrid )
      {
        private RectifiedGridDomain m_gridDomain = inputCoverage.getGridDomain();

        /**
         * @see org.kalypso.grid.AbstractDelegatingGeoGrid#getValue(int, int)
         */
        @Override
        public double getValue( int x, int y ) throws GeoGridException
        {
          final Double value = super.getValue( x, y );
          if( value.equals( Double.NaN) )
            return Double.NaN;
          else
          {
            final GM_Position positionAt = m_gridDomain .getPositionAt( x, y );
            final List<ILandusePolygon> list = polygonCollection.query( positionAt );
            if( list == null || list.size() == 0 )
              return Double.NaN;
            else
              return value;
          }
        }
      };

      final File file = File.createTempFile( "gridTest_grid", ".ascbin" );
      final String filePath = file.getName();
      GeoGridUtilities.addCoverage( outputCoverages, outputGrid, file, filePath, "image/bin", new NullProgressMonitor() );

      inputGrid.dispose();
    }

    /* Write result workspace */
    GmlSerializer.serializeWorkspace( tmpFile, outputWorkspace, "UTF-8" );
  }
}
