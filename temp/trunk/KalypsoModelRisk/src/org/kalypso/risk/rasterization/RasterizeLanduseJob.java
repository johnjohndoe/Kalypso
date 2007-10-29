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
package org.kalypso.risk.rasterization;

import java.io.File;
import java.util.List;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.grid.BinaryGeoGrid;
import org.kalypso.grid.RectifiedGridCoverageGeoGrid;
import org.kalypso.risk.model.schema.binding.ILanduseVectorModel;
import org.kalypso.risk.model.schema.binding.ILandusePolygon;
import org.kalypso.risk.model.schema.binding.IWaterdepthCoverage;
import org.kalypso.risk.model.schema.binding.IWaterdepthCoverageModel;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridDomain;

import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

public class RasterizeLanduseJob
{
  public void rasterize( final File dstFile ) throws Exception
  {
    final IWorkbench workbench = PlatformUI.getWorkbench();
    final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
    final IEvaluationContext context = handlerService.getCurrentState();
    // final IFolder scenarioFolder = (IFolder) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_FOLDER_NAME
    // );
    final SzenarioDataProvider szenarioDataProvider = (SzenarioDataProvider) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );
    final ILanduseVectorModel landuseVectorModel = szenarioDataProvider.getModel( ILanduseVectorModel.class );
    final IWaterdepthCoverageModel coverageModel = szenarioDataProvider.getModel( IWaterdepthCoverageModel.class );

    final IFeatureWrapperCollection<IWaterdepthCoverage> waterdepthCoverageCollection = coverageModel.getWaterdepthCoverageCollection();
    for( final IWaterdepthCoverage waterdepthCoverage : waterdepthCoverageCollection )
    {
      final RectifiedGridCoverageGeoGrid geoGrid = new RectifiedGridCoverageGeoGrid( waterdepthCoverage.getWrappedFeature() );
      final RectifiedGridDomain gridDomain = waterdepthCoverage.getGridDomain();
      final int numRows = gridDomain.getNumRows();
      final int numColumns = gridDomain.getNumColumns();
      final double noDataValue = 0.0;//Double.parseDouble( gridDomain.getNoDataValue() );
      final BinaryGeoGrid binaryGeoGrid = BinaryGeoGrid.createGrid( dstFile, numColumns, numRows, 2, geoGrid.getOrigin(), geoGrid.getOffsetX(), geoGrid.getOffsetY() );
      for( int x = 0; x < numColumns; x++ )
      {
        for( int y = 0; y < numRows; y++ )
        {
          double valueChecked = geoGrid.getValueChecked( x, y );
          if( valueChecked == noDataValue || valueChecked == Double.NaN )
            binaryGeoGrid.setValue( x, y, noDataValue );
          else
          {
            final GM_Position positionAt = gridDomain.getPositionAt( x, y );
            final List<ILandusePolygon> list = landuseVectorModel.getLandusePolygonCollection().query( positionAt );
            if( list == null || list.size() == 0 )
              binaryGeoGrid.setValue( x, y, noDataValue );
            else
              binaryGeoGrid.setValue( x, y, valueChecked );
          }
        }
      }
    }
  }

}