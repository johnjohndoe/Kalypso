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
package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import java.math.BigDecimal;
import java.net.URL;

import org.apache.commons.lang3.Range;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.gml.processes.tin.HmoTriangulatedSurfaceConverter;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.elevation.IElevationModel;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Triangle;
import org.kalypsodeegree.model.geometry.GM_TriangulatedSurface;
import org.kalypsodeegree.model.geometry.ISurfacePatchVisitable;
import org.kalypsodeegree.model.geometry.ISurfacePatchVisitor;
import org.kalypsodeegree.model.geometry.MinMaxSurfacePatchVisitor;

/**
 * An {@link IElevationProvider} based on an hmo file
 *
 * @author Patrice Congo
 * @author Madanagopal
 */
public class HMOTerrainElevationModel implements IElevationModel, ISurfacePatchVisitable<GM_Triangle>
{
  // public static final double[][] NO_INTERIOR = {};
  //
  // public static final GM_Position[][] NO_INTERIOR_POS = {};

  private Range<BigDecimal> m_minMax;

  // FIXME: this is nonsense, we should use the crs configured at our containing NativeTerrainModelWrapper and transform
  // our data into that crs
  private final String m_crs = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

  private GM_TriangulatedSurface m_surface;

  public HMOTerrainElevationModel( final URL hmoFileURL ) throws CoreException, GM_Exception
  {
    parseFile( hmoFileURL );
  }

  @Override
  public void dispose( )
  {
  }

  private final void parseFile( final URL hmoFileURL ) throws CoreException, GM_Exception
  {
    final String sourceSrs = m_crs;
    // TODO: It does not transform anything. The read data will be in the source coordinate system.
    final HmoTriangulatedSurfaceConverter converter = new HmoTriangulatedSurfaceConverter( sourceSrs );
    m_surface = converter.convert( hmoFileURL, new NullProgressMonitor() );

    /* Determine min/max */
    final MinMaxSurfacePatchVisitor<GM_Triangle> minMaxVisitor = new MinMaxSurfacePatchVisitor<>();
    final GM_Envelope maxBox = m_surface.getEnvelope();
    m_surface.acceptSurfacePatches( maxBox, minMaxVisitor, new NullProgressMonitor() );

    final BigDecimal min = minMaxVisitor.getMin();
    final BigDecimal max = minMaxVisitor.getMax();

    m_minMax = Range.between( min, max );
  }

  @Override
  public GM_Envelope getBoundingBox( )
  {
    return m_surface.getEnvelope();
  }

  @Override
  public double getElevation( final GM_Point location )
  {
    return m_surface.getValue( location );
  }

  @Override
  public void acceptSurfacePatches( final GM_Envelope envToVisit, final ISurfacePatchVisitor<GM_Triangle> surfacePatchVisitor, final IProgressMonitor monitor ) throws GM_Exception, CoreException
  {
    m_surface.acceptSurfacePatches( envToVisit, surfacePatchVisitor, monitor );
  }

  @Override
  public double getMaxElevation( )
  {
    return m_minMax == null ? Double.NaN : m_minMax.getMaximum().doubleValue();
  }

  @Override
  public double getMinElevation( )
  {
    return m_minMax == null ? Double.NaN : m_minMax.getMinimum().doubleValue();
  }
}