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
package org.kalypso.ogc.gml.serialize.test;

import java.io.File;
import java.net.URL;

import junit.framework.TestCase;

import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.grid.AbstractDelegatingGeoGrid;
import org.kalypso.grid.GeoGridException;
import org.kalypso.grid.GeoGridUtilities;
import org.kalypso.grid.IGeoGrid;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverage;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

/**
 * @author Gernot Belger
 */
public class GridTest extends TestCase
{
  public void testGridProcessing( ) throws Exception
  {
    final URL zipResource = getClass().getResource( "resources/dgm2m.zip" ); //$NON-NLS-1$
    final URL gmlResource = new URL( "jar:" + zipResource.toExternalForm() + "!/" + "dgm2m.gml" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    /* Read input workspace and coverages */
    final GMLWorkspace inputWorkspace = GmlSerializer.createGMLWorkspace( gmlResource, null );
    final ICoverageCollection inputCoverages = (ICoverageCollection) inputWorkspace.getRootFeature().getAdapter( ICoverageCollection.class );

    /* Create output workspace and get handle to coverages */
    final File tmpFile = File.createTempFile( "gridTest", ".gml" ); //$NON-NLS-1$ //$NON-NLS-2$
    final GMLWorkspace outputWorkspace = FeatureFactory.createGMLWorkspace( ICoverageCollection.QNAME, tmpFile.toURI().toURL(), null );
    final ICoverageCollection outputCoverages = (ICoverageCollection) outputWorkspace.getRootFeature().getAdapter( ICoverageCollection.class );

    for( final ICoverage inputCoverage : inputCoverages )
    {
      final IGeoGrid inputGrid = GeoGridUtilities.toGrid( inputCoverage );

      final IGeoGrid outputGrid = new AbstractDelegatingGeoGrid( inputGrid )
      {
        /**
         * @see org.kalypso.grid.AbstractDelegatingGeoGrid#getValue(int, int)
         */
        @Override
        public double getValue( final int x, final int y ) throws GeoGridException
        {
          return 2 * super.getValue( x, y );
        }

      };

      final File file = File.createTempFile( "gridTest_grid", ".ascbin" ); //$NON-NLS-1$ //$NON-NLS-2$
      final String filePath = file.getName();
      GeoGridUtilities.addCoverage( outputCoverages, outputGrid, file, filePath, "image/bin", new NullProgressMonitor() ); //$NON-NLS-1$

      inputGrid.dispose();
    }

    /* Write result workspace */
    GmlSerializer.serializeWorkspace( tmpFile, outputWorkspace, "UTF-8" ); //$NON-NLS-1$
  }
}
