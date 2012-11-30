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
package org.kalypso.kalypsosimulationmodel.core.terrainmodel.geom;

import java.util.Map;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModel;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.NativeTerrainElevationModelWrapper;
import org.kalypso.kalypsosimulationmodel.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Function property to provide shape for native terrain elevation
 *
 * @author Manadagopal
 * @author Patrice Congo
 */
public class TerrainElevationShapeGeometry extends FeaturePropertyFunction
{
  @Override
  public void init( final Map<String, String> properties )
  {
    // nothing to do
  }

  @Override
  public Object getValue( final Feature feature, final IPropertyType pt, final Object currentValue )
  {
    // System.out.println("getting shape:"+feature);
    try
    {
      final IFeatureType featureType = feature.getFeatureType();
      final QName featureQName = featureType.getQName();
      if( NativeTerrainElevationModelWrapper.SIM_BASE_F_NATIVE_TERRAIN_ELE_WRAPPER.equals( featureQName ) )
      {
        // transform the bounding box into a curve and return it
        final ITerrainElevationModel terrainElevationModel = (ITerrainElevationModel) feature.getAdapter( ITerrainElevationModel.class );
        final GM_Envelope bBox = terrainElevationModel.getBoundingBox();
        final GM_Position min = bBox.getMin();
        final GM_Position max = bBox.getMax();
        final double minx = min.getX();
        final double miny = min.getY();

        final double maxx = max.getX();
        final double maxy = max.getY();

        final double[] coords = new double[] { minx, miny, maxx, miny, maxx, maxy, minx, maxy, minx, miny, };
        return GeometryFactory.createGM_Curve( coords, 2, terrainElevationModel.getCoordinateSystem() );
      }
      else
      {
        return null;
      }
    }
    catch( final Throwable e )
    {
      // final IStatus status = StatusUtilities.statusFromThrowable( e );
      // KalypsoModelSimulationBaseConsts.getDefault().getLog().log( status );
      e.printStackTrace();
      return null;
    }
  }

  public static final Object toGM_Curve( final GM_Envelope bBox, final String crs )
  {
    // System.out.println("getting shape:"+feature);
    try
    {
      final GM_Position min = bBox.getMin();
      final GM_Position max = bBox.getMax();

      final double minx = min.getX();
      final double miny = min.getY();

      final double maxx = max.getX();
      final double maxy = max.getY();

      final double[] coords = new double[] { minx, miny, maxx, miny, maxx, maxy, minx, maxy, minx, miny, };
      final GM_Curve curve = GeometryFactory.createGM_Curve( coords, 2, crs );

      return curve;
    }
    catch( final Throwable e )
    {
      throw new RuntimeException( Messages.getString( "org.kalypso.kalypsosimulationmodel.core.terrainmodel.geom.TerrainElevationShapeGeometry.0" ), e ); //$NON-NLS-1$
    }
  }

  @Override
  public Object setValue( final Feature feature, final IPropertyType pt, final Object valueToSet )
  {
    // TODO: change underlying node geometry?
    return valueToSet;
  }
}
