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
package org.kalypso.kalypsomodel1d2d.ui.map.temsys.viz;

import java.awt.Graphics;

import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModel;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.NativeTerrainElevationModelWrapper;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.elevation.ElevationException;
import org.kalypsodeegree.model.elevation.IElevationModel;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_PolygonPatch;
import org.kalypsodeegree.model.geometry.ISurfacePatchVisitable;
import org.kalypsodeegree.model.geometry.ISurfacePatchVisitor;
import org.kalypsodeegree_impl.graphics.displayelements.IElevationColorModel;
import org.kalypsodeegree_impl.graphics.displayelements.SurfacePaintPlainTriangleVisitor;
import org.kalypsodeegree_impl.graphics.displayelements.SurfacePatchVisitableDisplayElement;
import org.kalypsodeegree_impl.graphics.displayelements.SurfacePatchVisitableDisplayElement.IVisitorFactory;
/**
 * @author Gernot Belger
 */
public class ElevationModelDisplayElementFactory
{
  public static final SurfacePatchVisitableDisplayElement<GM_PolygonPatch> createDisplayElement( final Feature feature )
  {
    if( feature == null )
    {
      return null;
    }
    final ITerrainElevationModel terrainElevationModel = (ITerrainElevationModel) feature.getAdapter( ITerrainElevationModel.class );
    if( terrainElevationModel instanceof NativeTerrainElevationModelWrapper )
    {
      final IElevationModel elevationProvider = ((NativeTerrainElevationModelWrapper) terrainElevationModel).getElevationProvider();

      if( elevationProvider instanceof ISurfacePatchVisitable )
      {
        final IVisitorFactory<GM_PolygonPatch> visitorFactory = new SurfacePatchVisitableDisplayElement.IVisitorFactory<GM_PolygonPatch>()
        {
          @Override
          public ISurfacePatchVisitor<GM_PolygonPatch> createVisitor( final Graphics g, final GeoTransform projection )
          {
            final IElevationColorModel colorModel = createColorModel( elevationProvider, projection );
            return new SurfacePaintPlainTriangleVisitor<>( g, colorModel );
          }
        };

        return new SurfacePatchVisitableDisplayElement<>( feature, (ISurfacePatchVisitable<GM_PolygonPatch>)elevationProvider, visitorFactory );
      }
      else
      {
        throw new RuntimeException( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.temsys.viz.ElevationModelDisplayElementFactory.1") + elevationProvider ); //$NON-NLS-1$
      }
    }
    else
    {
      // System.out.println( "Could not adapt to a native terrain ele model:" + feature );
      return null;
    }
  }

  public static IElevationColorModel createColorModel( final IElevationModel elevationProvider, final GeoTransform projection )
  {
    try
    {
      final IElevationColorModel colorModel = ElevationColorControl.getColorModel( elevationProvider.getMinElevation(), elevationProvider.getMaxElevation() );
      // REMARK: very slow -> only call once per triangulated surface!
      colorModel.setProjection( projection );
      return colorModel;
    }
    catch( final ElevationException e )
    {
      e.printStackTrace();
      return null;
    }
  }
}
