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
package org.kalypsodeegree_impl.graphics.displayelements;

import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.geom.Area;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import org.kalypsodeegree.graphics.sld.PolygonSymbolizer;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree.model.geometry.GM_Triangle;
import org.kalypsodeegree.model.geometry.ISurfacePatchVisitor;
import org.kalypsodeegree_impl.graphics.sld.PolygonSymbolizer_Impl;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.tools.Debug;

/**
 * @author Gernot Belger, Thomas Jung
 */
public class SurfacePaintPlainTriangleVisitor<T extends GM_SurfacePatch> implements ISurfacePatchVisitor<T>
{
  private static final PolygonSymbolizer m_defaultSymbolizer = new PolygonSymbolizer_Impl();

  private final Graphics m_gc;

  private final GeoTransform m_projection;

  private final IElevationColorModel m_colorModel;

  private static final double VAL_EPS = 0.0000001;

  public SurfacePaintPlainTriangleVisitor( Feature feature, final Graphics gc, final GeoTransform projection, final IElevationColorModel colorModel )
  {
    m_gc = gc;
    m_projection = projection;
    m_colorModel = colorModel;
  }

  /**
   * @see org.kalypsodeegree.model.geometry.ISurfacePatchVisitor#visit(org.kalypsodeegree.model.geometry.GM_SurfacePatch,
   *      double)
   */
  public boolean visit( final T patch, final double elevationSample ) throws Exception
  {
    if( patch instanceof GM_Triangle )
    {
      final GM_Triangle triangle = (GM_Triangle) patch;

      getTriangleSurface( triangle );
    }
    else
      paintThisSurface( patch, elevationSample );

    return true;
  }

  private void getTriangleSurface( GM_Triangle triangle )
  {
    // get value range of the triangle
    double minValue = Double.POSITIVE_INFINITY;
    double maxValue = Double.NEGATIVE_INFINITY;

    final GM_Position[] positions = triangle.getExteriorRing();

    for( GM_Position position : positions )
    {
      if( position.getZ() < minValue )
        minValue = position.getZ();
      if( position.getZ() > maxValue )
        maxValue = position.getZ();
    }

// final BigDecimal minDecimal = new BigDecimal( minValue ).setScale( 1, BigDecimal.ROUND_FLOOR );

    int numOfClasses = m_colorModel.getNumOfClasses();

    /* loop over all classes */
    for( int currentClass = 0; currentClass <= numOfClasses; currentClass++ )
    {
      /* aktuelles von und bis setzen */
      final double startValue = m_colorModel.getFrom( currentClass );
      final double endValue = m_colorModel.getTo( currentClass );

      if( triangleLiesInsideColorClass( minValue, maxValue, startValue, endValue ) == true )
      {

        /* code below was taken from BCE-2D - bce_FarbFlaechenInAllenDreiecken and a little bit adapted */

        /* Pr¸fung f¸r jede Kante und deren Knotenhˆhen, Vergleich mit Intervallgrenzen von - bis */
        if( startValue <= positions[0].getZ() && positions[0].getZ() <= endValue && startValue <= positions[1].getZ() && positions[1].getZ() <= endValue && startValue <= positions[2].getZ()
            && positions[2].getZ() <= endValue )
        {
          /* paint whole triangle in one color and exit loop */
          Area areaFromRing = new Area( SurfacePatchVisitableDisplayElement.areaFromRing( m_projection, 0, positions ) );

          /* get the mean elevation of the current ring */
          double meanValue = getMeanValue( positions );
          if( meanValue - minValue < VAL_EPS )
            meanValue = meanValue + VAL_EPS;
          else if( meanValue - maxValue < VAL_EPS )
            meanValue = meanValue - VAL_EPS;

          /* get the color from the color model and paint the triangle */
          m_gc.setColor( m_colorModel.getColor( meanValue ) );
          ((Graphics2D) m_gc).fill( areaFromRing );
          break;
        }
        else
        {
          /* get the intersection points */
          final List<GM_Position> posList = new LinkedList<GM_Position>();

          /* loop over all arcs */
          for( int j = 0; j < positions.length - 1; j++ )
          {
            GM_Position pos1 = positions[j];
            GM_Position pos2 = positions[j + 1];

            double x1 = pos1.getX();
            double y1 = pos1.getY();
            double z1 = pos1.getZ();

            double x2 = pos2.getX();
            double y2 = pos2.getY();
            double z2 = pos2.getZ();

            double x = 0;
            double y = 0;
            double z = 0;

            /* ====================== Fallunterscheidungen ========================= */

            /*
             * Knoten 1 liegt innerhalb, Knoten 2 liegt innerhalb Knoten 1.z = Knoten 2.z des aktuell betrachteten
             * Intervalls
             */
            if( z1 == z2 && z1 >= startValue && z1 <= endValue )
            {
              posList.add( GeometryFactory.createGM_Position( x1, y1, z1 ) );

              posList.add( GeometryFactory.createGM_Position( x2, y2, z2 ) );
            }

            /*
             * Knoten 1 liegt innnerhalb, Knoten 2 liegt oberhalb Knoten 1.z < Knoten 2.z des aktuell betrachteten
             * Intervalls
             */
            else if( startValue <= z1 && z1 < endValue && endValue <= z2 )
            {
              posList.add( GeometryFactory.createGM_Position( x1, y1, z1 ) );

              x = x1 + (x2 - x1) * (endValue - z1) / (z2 - z1);
              y = y1 + (y2 - y1) * (endValue - z1) / (z2 - z1);
              z = endValue;
              posList.add( GeometryFactory.createGM_Position( x, y, z ) );
            }

            /*
             * Knoten 1 liegt unterhalb, Knoten 2 liegt oberhalb Knoten 1.z < Knoten 2.z des aktuell betrachteten
             * Intervalls
             */
            else if( z1 < startValue && startValue < z2 && z1 < endValue && endValue < z2 )
            {
              x = x1 + (x2 - x1) * (startValue - z1) / (z2 - z1);
              y = y1 + (y2 - y1) * (startValue - z1) / (z2 - z1);
              z = startValue;
              posList.add( GeometryFactory.createGM_Position( x, y, z ) );

              x = x1 + (x2 - x1) * (endValue - z1) / (z2 - z1);
              y = y1 + (y2 - y1) * (endValue - z1) / (z2 - z1);
              z = endValue;
              posList.add( GeometryFactory.createGM_Position( x, y, z ) );
            }

            /*
             * Knoten 1 liegt unterhalb, Knoten 2 liegt innerhalb Knoten 1.z < Knoten 2.z des aktuell betrachteten
             * Intervalls
             */
            else if( z1 <= startValue && startValue < z2 && z2 <= endValue )
            {
              x = x1 + (x2 - x1) * (startValue - z1) / (z2 - z1);
              y = y1 + (y2 - y1) * (startValue - z1) / (z2 - z1);
              z = startValue;
              posList.add( GeometryFactory.createGM_Position( x, y, z ) );

              posList.add( GeometryFactory.createGM_Position( x2, y2, z2 ) );
            }

            /*
             * Knoten 1 liegt innerhalb, Knoten 2 liegt innerhalb Knoten 1.z < Knoten 2.z des aktuell betrachteten
             * Intervalls
             */
            else if( startValue <= z1 && z2 <= endValue && z1 < z2 )
            {
              posList.add( GeometryFactory.createGM_Position( x1, y1, z1 ) );

              posList.add( GeometryFactory.createGM_Position( x2, y2, z2 ) );
            }

            /*
             * Knoten 1 liegt oberhalb, Knoten 2 liegt innerhalb Knoten 1.z < Knoten 2.z des aktuell betrachteten
             * Intervalls
             */
            else if( startValue <= z2 && z2 < endValue && endValue <= z1 )
            {
              x = x1 + (x2 - x1) * (endValue - z1) / (z2 - z1);
              y = y1 + (y2 - y1) * (endValue - z1) / (z2 - z1);
              z = endValue;
              posList.add( GeometryFactory.createGM_Position( x, y, z ) );

              posList.add( GeometryFactory.createGM_Position( x2, y2, z2 ) );
            }

            /*
             * Knoten 1 liegt innerhalb, Knoten 2 liegt unterhalb Knoten 1.z > Knoten 2.z des aktuell betrachteten
             * Intervalls
             */
            else if( z2 < startValue && startValue < z1 && z2 < endValue && endValue < z1 )
            {
              x = x1 + (x2 - x1) * (endValue - z1) / (z2 - z1);
              y = y1 + (y2 - y1) * (endValue - z1) / (z2 - z1);
              z = endValue;
              posList.add( GeometryFactory.createGM_Position( x, y, z ) );

              x = x1 + (x2 - x1) * (startValue - z1) / (z2 - z1);
              y = y1 + (y2 - y1) * (startValue - z1) / (z2 - z1);
              z = startValue;
              posList.add( GeometryFactory.createGM_Position( x, y, z ) );
            }

            /*
             * Knoten 1 liegt innerhalb, Knoten 2 liegt unterhalb Knoten 1.z > Knoten 2.z des aktuell betrachteten
             * Intervalls
             */
            else if( z2 <= startValue && startValue < z1 && z1 <= endValue )
            {
              posList.add( GeometryFactory.createGM_Position( x1, y1, z1 ) );

              x = x1 + (x2 - x1) * (startValue - z1) / (z2 - z1);
              y = y1 + (y2 - y1) * (startValue - z1) / (z2 - z1);
              z = startValue;
              posList.add( GeometryFactory.createGM_Position( x, y, z ) );
            }

            /*
             * Knoten 1 liegt innerhalb, Knoten 2 liegt innerhalb Knoten 2.z > Knoten 1.z des aktuell betrachteten
             * Intervalls
             */
            else if( startValue <= z2 && z1 <= endValue && z2 < z1 )
            {
              posList.add( GeometryFactory.createGM_Position( x1, y1, z1 ) );

              posList.add( GeometryFactory.createGM_Position( x2, y2, z2 ) );
            }
          }

          /*
           * Markieren doppelt erzeugter Punkte und gleichzeitiges Lˆschen (Nicht-kopieren in 2te Liste aufgrund der
           * Orientierung der triangulierten Ausgangsdreiecke sind erzeugten Farbfl‰chen-Polygone ebenfalls automatisch
           * orientiert.
           */
          int numDoublePoints = 0; // Anzahl nicht doppelter Punkte in Farbklasse (b‰h, unschˆn gelˆst!)

          final List<GM_Position> posList2 = new ArrayList<GM_Position>();
          if( posList.size() < 2 )
            break;

          posList2.add( posList.get( 0 ) );

          /* Schleife ¸ber alle erzeugte Knoten */
          for( int k = 0; k < posList.size() - 1; k++ )
          {
            /* Abstand zweier Punkte berechnen */
            final double distance = posList.get( k + 1 ).getDistance( posList.get( numDoublePoints ) );

            /*
             * Wenn Abstand groﬂ genug ist, Punkt ¸bernehmen und zu dicht gelegene Punkte ignorieren
             */
            if( distance > VAL_EPS )
            {
              posList2.add( posList.get( k + 1 ) );
              numDoublePoints = numDoublePoints + 1;
            }
          }

          if( numDoublePoints > 0 )
          {
            // ersten mit letztem Punkt vegleichen
            final double distance = posList2.get( 0 ).getDistance( posList2.get( posList2.size() - 1 ) );

            if( distance < VAL_EPS )
              numDoublePoints = numDoublePoints - 1;

            if( numDoublePoints >= 2 )// mindestens 3 verschiedene Punkte
            {
              posList2.add( posList2.get( 0 ) );
              GM_Position[] posArray = posList2.toArray( new GM_Position[posList2.size()] );

              Area areaFromRing = new Area( SurfacePatchVisitableDisplayElement.areaFromRing( m_projection, 0, posArray ) );

              /* get the mean elevation of the current ring */
              double meanValue = getMeanValue( posArray );
              if( meanValue - minValue < VAL_EPS )
                meanValue = meanValue + VAL_EPS;
              else if( meanValue - maxValue < VAL_EPS )
                meanValue = meanValue - VAL_EPS;

              /* get the color from the color model and paint the polygon */
              ((Graphics2D) m_gc).fill( areaFromRing );

            }
          }
        }
      }
    }
  }

  private boolean triangleLiesInsideColorClass( final double min, final double max, final double from, final double to )
  {
    if( (from <= min && min <= to) || (from <= max && max <= to) )
      return true;
    else
      return false;
  }

  private double getMeanValue( GM_Position[] positions )
  {
    double zSum = 0;

    for( GM_Position position : positions )
    {
      zSum = zSum + position.getZ();
    }
    return zSum / positions.length;
  }

  private void paintThisSurface( final GM_SurfacePatch patch, final double elevation ) throws Exception
  {
    final Area area = calcTargetCoordinates( this.m_projection, patch );
    m_gc.setColor( m_colorModel.getColor( elevation ) );
    ((Graphics2D) m_gc).fill( area );
  }

  /**
   * calculates the Area (image or screen coordinates) where to draw the surface.
   */
  private Area calcTargetCoordinates( final GeoTransform projection, final GM_SurfacePatch patch ) throws Exception
  {
    final PolygonSymbolizer sym = m_defaultSymbolizer;
    final Stroke stroke = sym.getStroke();
    float width = 1;
    if( stroke != null )
    {
      width = (float) stroke.getWidth( null );
    }

    try
    {
      final GM_Position[] ex = patch.getExteriorRing();
      final GM_Position[][] inner = patch.getInteriorRings();

      final Area areaouter = new Area( SurfacePatchVisitableDisplayElement.areaFromRing( projection, width, ex ) );
      if( inner != null )
      {
        for( final GM_Position[] innerRing : inner )
        {
          if( innerRing != null )
            areaouter.subtract( new Area( SurfacePatchVisitableDisplayElement.areaFromRing( projection, width, innerRing ) ) );
        }
      }

      return areaouter;
    }
    catch( final Exception e )
    {
      Debug.debugException( e, "" );
    }

    return null;
  }

}
