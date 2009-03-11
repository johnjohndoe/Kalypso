package org.kalypsodeegree_impl.tools.refinement;

import java.util.ArrayList;
import java.util.List;

import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_MultiPoint;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

public class Refinement
{

  private static final double MAX_DISTANCE = .000001;

  @SuppressWarnings("unchecked")
  public GM_Object[] doRefine( final GM_MultiSurface[] inputSurfaces, final GM_Curve inputCurve ) throws GM_Exception
  {
    final List<GM_Object> list = new ArrayList<GM_Object>();

    /* consider each surface */
    for( int i = 0; i < inputSurfaces.length; i++ )
    {
      final GM_MultiSurface polygonSurface = inputSurfaces[i];

      final GM_Object[] objects = polygonSurface.getAll();
      for( final GM_Object object : objects )
      {
        if( object instanceof GM_Surface )
        {
          final GM_Surface<GM_SurfacePatch> surface = (GM_Surface<GM_SurfacePatch>) object;

          /* clip refinement curve with each surface patch */
          for( final GM_SurfacePatch surfacePatch : surface )
          {
            final GM_Object preIntersection = inputCurve.intersection( surface );

            if( preIntersection instanceof GM_Point )
              continue;

            // convert patch to curve and get intersection points with refinment curve
            final GM_Position[] exterior = surfacePatch.getExteriorRing();
            final GM_Curve curve = GeometryFactory.createGM_Curve( exterior, surfacePatch.getCoordinateSystem() );
            final GM_Object intersection = curve.intersection( inputCurve );
            // this intersection just gives the x- and y-values, not the z!
            // so we have to compute the z-value ourselfes

            if( intersection instanceof GM_MultiPoint )
            {
              final List<GM_Point> pointList = new ArrayList<GM_Point>();

              final GM_MultiPoint multiPoint = (GM_MultiPoint) intersection;
              final GM_Point[] points = multiPoint.getAllPoints();

              for( int j = 0; j < points.length; j++ )
              {
                final GM_Point point = points[j];
                if( Double.isNaN( point.getZ() ) )
                  pointList.add( RefinementUtils.interpolateZ( point, exterior ) );
                else
                  pointList.add( point );
              }

              final GM_Point[] intersectionPoints = pointList.toArray( new GM_Point[pointList.size()] );

              /* we consider only intersections that have one or two intersection points */
              if( intersectionPoints.length == 2 )
              {
                /* split surface */
                final GM_Position[] poses = new GM_Position[intersectionPoints.length];
                for( int j = 0; j < intersectionPoints.length; j++ )
                  poses[j] = intersectionPoints[j].getPosition();

                final GM_Surface[] surfaces = RefinementUtils.splitSurfacePatch( surfacePatch, poses );
                for( int j = 0; j < surfaces.length; j++ )
                  list.add( surfaces[j] );
              }
              else
              {
                list.clear();
                return list.toArray( new GM_Object[list.size()] );
              }
            }

            else if( intersection instanceof GM_Point )
            {
              final List<GM_Point> pointList = new ArrayList<GM_Point>();
              final GM_Point point = (GM_Point) intersection;

              if( Double.isNaN( point.getZ() ) )
                pointList.add( RefinementUtils.interpolateZ( point, exterior ) );
              else
                pointList.add( point );

              // TODO: find a good second intersection point on the patch
              // right now, we take the first point on the patch that does not lie on the segment that we want to split
              final String crs = point.getCoordinateSystem();
              final GM_Curve[] segments = RefinementUtils.getPositionsAsCurves( exterior, crs );

              for( int j = 0; j < segments.length; j++ )
              {
                final GM_Curve splitSegment = segments[j];
                final GM_Object gmobject = splitSegment.intersection( point );
                if( gmobject != null || (splitSegment.distance( point ) < MAX_DISTANCE * 2) )
                {
                  final GM_Point startPoint = splitSegment.getAsLineString().getStartPoint();
                  final GM_Point endPoint = splitSegment.getAsLineString().getEndPoint();

                  GM_Curve segment2 = segments[j];
                  if( j > 0 )
                    segment2 = segments[j - 1];
                  else
                    segment2 = segments[j + 1];

                  final GM_Point startPoint2 = segment2.getAsLineString().getAsLineString().getStartPoint();
                  final GM_Point endPoint2 = segment2.getAsLineString().getAsLineString().getEndPoint();

                  if( startPoint2.equals( startPoint ) )
                    pointList.add( endPoint2 );
                  else if( endPoint2.equals( startPoint ) )
                    pointList.add( startPoint2 );
                  else if( startPoint2.equals( endPoint ) )
                    pointList.add( endPoint2 );
                  else if( endPoint2.equals( endPoint ) )
                    pointList.add( startPoint2 );

                  // TODO: check for not wanted intersections

                  break;
                }
              }

              final GM_Point[] intersectionPoints = pointList.toArray( new GM_Point[pointList.size()] );

              /* split surface */
              final GM_Position[] poses = new GM_Position[intersectionPoints.length];
              for( int j = 0; j < intersectionPoints.length; j++ )
                poses[j] = intersectionPoints[j].getPosition();

              final GM_Surface[] surfaces = RefinementUtils.splitSurfacePatch( surfacePatch, poses );
              for( int j = 0; j < surfaces.length; j++ )
                list.add( surfaces[j] );
            }
          }
        }
      }
    }
    return list.toArray( new GM_Object[list.size()] );
  }

}
