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
package org.kalypsodeegree_impl.graphics.displayelements;

import java.awt.Graphics;
import java.awt.Graphics2D;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Triangle;
import org.kalypsodeegree.model.geometry.ISurfacePatchVisitor;
import org.kalypsodeegree_impl.graphics.sld.awt.FillPainter;
import org.kalypsodeegree_impl.graphics.sld.awt.SldAwtUtilities;
import org.kalypsodeegree_impl.graphics.sld.awt.StrokePainter;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author Gernot Belger
 * @author Thomas Jung
 */
public class SurfacePaintPolygonVisitor implements ISurfacePatchVisitor<GM_Triangle>
{
  private final Graphics m_gc;

  private final ColorMapConverter m_colorModel;

  private static final double VAL_EPS = 0.0000001;

  public SurfacePaintPolygonVisitor( final Graphics gc, final ColorMapConverter colorModel )
  {
    m_gc = gc;
    m_colorModel = colorModel;
  }

  /**
   * @see org.kalypsodeegree.model.geometry.ISurfacePatchVisitor#visit(org.kalypsodeegree.model.geometry.GM_SurfacePatch,
   *      double)
   */
  public boolean visit( final GM_Triangle triangle, final double elevationSample ) throws Exception
  {
    getTriangleSurface( triangle );
    return true;
  }

  private void getTriangleSurface( final GM_Triangle triangle )
  {
    // get value range of the triangle
    double minValue = Double.POSITIVE_INFINITY;
    double maxValue = Double.NEGATIVE_INFINITY;

    final GM_Position[] positions = triangle.getExteriorRing();

    for( final GM_Position position : positions )
    {
      if( position.getZ() < minValue )
        minValue = position.getZ();
      if( position.getZ() > maxValue )
        maxValue = position.getZ();
    }

// final BigDecimal minDecimal = new BigDecimal( minValue ).setScale( 1, BigDecimal.ROUND_FLOOR );

    final int numOfClasses = m_colorModel.getNumOfClasses();

    /* loop over all classes */
    for( int currentClass = 0; currentClass < numOfClasses; currentClass++ )
    {
      /* aktuelles von und bis setzen */
      final double startValue = m_colorModel.getFrom( currentClass );
      final double endValue = m_colorModel.getTo( currentClass );

      if( triangleLiesInsideColorClass( minValue, maxValue, startValue, endValue ) == true )
      {
        /* code below was taken from BCE-2D - bce_FarbFlaechenInAllenDreiecken and a little bit adapted */

        /* Prüfung für jede Kante und deren Knotenhöhen, Vergleich mit Intervallgrenzen von - bis */
        if( startValue <= positions[0].getZ() && positions[0].getZ() <= endValue && startValue <= positions[1].getZ() && positions[1].getZ() <= endValue && startValue <= positions[2].getZ()
            && positions[2].getZ() <= endValue )
        {
          /* paint whole triangle in one color and exit loop */

          /* get the mean elevation of the current ring */
          double meanValue = getMeanValue( positions );
          if( meanValue - minValue < VAL_EPS )
            meanValue = meanValue + VAL_EPS;
          else if( meanValue - maxValue < VAL_EPS )
            meanValue = meanValue - VAL_EPS;

          /* get the color from the color model and paint the triangle */

          // TODO: check if triangle is too small (i.e. < stroke.width) and paint it as one point
          paintSurface( positions, currentClass );
          break;
        }
        else
        {
          /* get the intersection points */
          final List<GM_Position> posList = new LinkedList<GM_Position>();

          /* loop over all arcs */
          for( int j = 0; j < positions.length - 1; j++ )
          {
            final GM_Position pos1 = positions[j];
            final GM_Position pos2 = positions[j + 1];

            final double x1 = pos1.getX();
            final double y1 = pos1.getY();
            final double z1 = pos1.getZ();

            final double x2 = pos2.getX();
            final double y2 = pos2.getY();
            final double z2 = pos2.getZ();

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
           * Markieren doppelt erzeugter Punkte und gleichzeitiges Löschen (Nicht-kopieren in 2te Liste aufgrund der
           * Orientierung der triangulierten Ausgangsdreiecke sind erzeugten Farbflächen-Polygone ebenfalls automatisch
           * orientiert.
           */
          int numDoublePoints = 0; // Anzahl nicht doppelter Punkte in Farbklasse (bäh, unschön gelöst!)

          if( posList.size() < 2 )
            continue;

          final List<GM_Position> posList2 = new ArrayList<GM_Position>();
          posList2.add( posList.get( 0 ) );

          /* Schleife über alle erzeugte Knoten */
          for( int k = 0; k < posList.size() - 1; k++ )
          {
            /* Abstand zweier Punkte berechnen */
            final double distance = posList.get( k + 1 ).getDistance( posList.get( numDoublePoints ) );

            /*
             * Wenn Abstand groß genug ist, Punkt übernehmen und zu dicht gelegene Punkte ignorieren
             */
            // TODO: Sollte sich diese Grösse nicht an der Strichstärke orientieren?
            if( distance > VAL_EPS )
            {
              posList2.add( posList.get( k + 1 ) );
              numDoublePoints = numDoublePoints + 1;
            }
          }

          if( numDoublePoints > 0 )
          {
            // ersten mit letztem Punkt vergleichen
            final double distance = posList2.get( 0 ).getDistance( posList2.get( posList2.size() - 1 ) );

            if( distance < VAL_EPS )
              numDoublePoints = numDoublePoints - 1;

            if( numDoublePoints >= 2 )// mindestens 3 verschiedene Punkte
            {
              posList2.add( posList2.get( 0 ) );
              final GM_Position[] posArray = posList2.toArray( new GM_Position[posList2.size()] );

              /* get the mean elevation of the current ring */
              double meanValue = getMeanValue( posArray );
              if( meanValue - minValue < VAL_EPS )
                meanValue = meanValue + VAL_EPS;
              else if( meanValue - maxValue < VAL_EPS )
                meanValue = meanValue - VAL_EPS;

              /* get the color from the color model and paint the polygon */

              // TODO: check if current surface is too small (i.e. < stroke.width) and if this is the case
              // only draw a point with that color or something similar
              paintSurface( posArray, currentClass );
            }
          }
        }
      }
    }
  }

  private void paintSurface( final GM_Position[] posArray, final int currentClass )
  {
    final StrokePainter strokePainter = m_colorModel.getLinePainter( currentClass );
    final FillPainter fillPainter = m_colorModel.getFillPolygonPainter( currentClass );
    final GeoTransform world2Screen = fillPainter.getWorld2Screen();

    try
    {
      SldAwtUtilities.paintRing( (Graphics2D) m_gc, posArray, world2Screen, fillPainter, strokePainter );
    }
    catch( final Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }

  }

  private boolean triangleLiesInsideColorClass( final double min, final double max, final double from, final double to )
  {
    if( (from <= min && min <= to) || (from <= max && max <= to) || (min <= from && to <= max) )
      return true;
    else
      return false;
  }

  private double getMeanValue( final GM_Position[] positions )
  {
    double zSum = 0;

    for( final GM_Position position : positions )
    {
      zSum = zSum + position.getZ();
    }
    return zSum / positions.length;
  }

}
