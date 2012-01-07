/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.ui.rrm.internal.cm.thiessen;

import java.util.ArrayList;
import java.util.Collection;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.kalypso.afgui.scenarios.ScenarioHelper;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.model.hydrology.project.INaProjectConstants;
import org.kalypso.model.hydrology.timeseries.binding.ITimeseries;
import org.kalypso.model.rcm.binding.IThiessenStation;
import org.kalypso.model.rcm.binding.IThiessenStationCollection;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree_impl.model.feature.visitors.TransformVisitor;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Polygon;

/**
 * Helper class that represants all chosen timeseries with their corresponding thiessen polygons.
 *
 * @author Gernot Belger
 */
public class TimeseriesThiessenPolygons
{
  private final Collection<Polygon> m_thiessenPolygons = new ArrayList<>();

  private final Collection<String> m_timeseries = new ArrayList<>();

  public Polygon[] getThiessenPolygons( )
  {
    return m_thiessenPolygons.toArray( new Polygon[m_thiessenPolygons.size()] );
  }

  public String[] getTimeseries( )
  {
    return m_timeseries.toArray( new String[m_timeseries.size()] );
  }

  public void loadData( ) throws CoreException
  {
    final IThiessenStationCollection thiessenStations = loadThiessenStations();

    /* Fetch all active timeseries with their polygons */
    final IFeatureBindingCollection<IThiessenStation> stations = thiessenStations.getStations();
    for( final IThiessenStation station : stations )
    {
      try
      {
        if( station.isActive() )
        {
          final GM_Surface<GM_SurfacePatch> area = station.getThiessenArea();
          final Polygon polygon = (Polygon) JTSAdapter.export( area );
          final Feature timeseries = station.getStation();

          if( timeseries instanceof ITimeseries )
            addTimeseries( (ITimeseries) timeseries, polygon );
        }
      }
      catch( final GM_Exception e )
      {
        e.printStackTrace();
        final IStatus status = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), "Failed to read thiessen polygon data", e );
        throw new CoreException( status );
      }
    }
  }

  private void addTimeseries( final ITimeseries timeseries, final Polygon polygon )
  {
    final ZmlLink dataLink = timeseries.getDataLink();
    if( dataLink == null )
      return;

    final String href = dataLink.getHref();
    if( StringUtils.isBlank( href ) )
      return;

    m_thiessenPolygons.add( polygon );
    m_timeseries.add( href );
  }

  private IThiessenStationCollection loadThiessenStations( ) throws CoreException
  {
    try
    {
      /* timeseries file */
      final SzenarioDataProvider scenarioDataProvider = ScenarioHelper.getScenarioDataProvider();
      final IContainer scenarioFolder = scenarioDataProvider.getScenarioFolder();
      final IFile thiessenFile = scenarioFolder.getFile( new Path( INaProjectConstants.GML_THIESSEN_STATION_PATH ) );

      /* load file and transform to kalypso crs */
      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( thiessenFile );
      workspace.accept( new TransformVisitor( KalypsoDeegreePlugin.getDefault().getCoordinateSystem() ), FeatureVisitor.DEPTH_INFINITE );
      return (IThiessenStationCollection) workspace.getRootFeature();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), "Failed to load thiessen stations", e );
      throw new CoreException( status );
    }
  }
}
