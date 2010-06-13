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
package org.kalypso.kalypsomodel1d2d.conv.results.differences;

import java.io.File;
import java.math.BigDecimal;
import java.util.LinkedList;
import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypsomodel1d2d.conv.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultType;
import org.kalypso.kalypsomodel1d2d.conv.results.TinResultWriter;
import org.kalypso.kalypsomodel1d2d.conv.results.TriangulatedSurfaceTriangleEater;
import org.kalypso.kalypsomodel1d2d.conv.results.differences.IMathOperatorDelegate.MATH_OPERATOR;
import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;
import org.kalypso.kalypsomodel1d2d.sim.MinMaxCatcher;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Triangle;
import org.kalypsodeegree.model.geometry.GM_TriangulatedSurface;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author Thomas Jung
 *
 */
public class DifferenceResultTinHandler
{
  public static IStatus generateDifferences( final GM_TriangulatedSurface[] surfaces, final MATH_OPERATOR operator, final IFile diffFile, final MinMaxCatcher minMaxCatcher, final IProgressMonitor monitor )
  {
    final String crs = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

    final File tinResultFile = diffFile.getLocation().toFile();

    try
    {
      final GMLWorkspace triangleWorkspace = FeatureFactory.createGMLWorkspace( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "TinResult" ), tinResultFile.toURL(), null ); //$NON-NLS-1$
      final GM_TriangulatedSurface surface = org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_TriangulatedSurface( crs );
      final Feature triangleFeature = triangleWorkspace.getRootFeature();
      triangleFeature.setProperty( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "triangulatedSurfaceMember" ), surface ); //$NON-NLS-1$
      triangleFeature.setProperty( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "unit" ), "[-]" ); //$NON-NLS-1$ //$NON-NLS-2$
      triangleFeature.setProperty( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "parameter" ), Messages.getString("org.kalypso.kalypsomodel1d2d.conv.results.differences.DifferenceResultTinHandler.5") ); //$NON-NLS-1$ //$NON-NLS-2$

      // Loop over master triangles
      final GM_TriangulatedSurface masterSurface = surfaces[0];
      final GM_TriangulatedSurface slaveSurface = surfaces[1];

      final TriangulatedSurfaceTriangleEater eater = new TriangulatedSurfaceTriangleEater( tinResultFile, triangleWorkspace, surface, ResultType.TYPE.DIFFERENCE, new TinResultWriter.QNameAndString[] {} );

      // monitor:
      // 70% available
      // => 70 / masterSurface.size()

      // use monitor to display progress.
      final BigDecimal maxProgress = new BigDecimal( 70 );
      final BigDecimal stepNum = new BigDecimal( masterSurface.size() );
      final BigDecimal val = stepNum.divide( maxProgress, 5, BigDecimal.ROUND_HALF_UP );
      BigDecimal monitorValue = new BigDecimal( 0 );

      for( int i = 0; i < masterSurface.size(); i++ )
      {
        final GM_Triangle triangle = masterSurface.get( i );

        if( monitor != null )
        {
          monitorValue = updateMonitor( monitor, val, monitorValue );
        }

        final List<GM_Point> nodeList = new LinkedList<GM_Point>();

        final GM_Position[] ring = triangle.getExteriorRing();

        for( int j = 0; j < ring.length - 1; j++ )
        {
          final GM_Point point = GeometryFactory.createGM_Point( ring[j], crs );

          final double o1 = point.getZ();
          final double o2 = slaveSurface.getValue( point );

          BigDecimal result = null;

          if( !Double.isNaN( o2 ) )
          {
            result = operator.getOperator().getResult( new BigDecimal( o1 ), new BigDecimal( o2 ) );

            if( minMaxCatcher != null )
              minMaxCatcher.addResult( result );
          }

          if( result != null )
          {
            final GM_Point newPoint = GeometryFactory.createGM_Point( point.getX(), point.getY(), result.doubleValue(), crs );
            nodeList.add( newPoint );
          }
        }

        if( nodeList.size() == 3 )
        {
          eater.addPoints( nodeList );

        }

      }
      if( monitor != null )
        monitor.subTask( Messages.getString("org.kalypso.kalypsomodel1d2d.conv.results.differences.DifferenceResultTinHandler.6") ); //$NON-NLS-1$

      eater.finished();

      return Status.OK_STATUS;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return StatusUtilities.statusFromThrowable( e, Messages.getString("org.kalypso.kalypsomodel1d2d.conv.results.differences.DifferenceResultTinHandler.7") ); //$NON-NLS-1$
    }
  }

  private static BigDecimal updateMonitor( final IProgressMonitor monitor, final BigDecimal val, BigDecimal monitorValue )
  {
    monitorValue = monitorValue.add( new BigDecimal( 1 ).divide( val, 4, BigDecimal.ROUND_HALF_UP ) );
    if( monitorValue.doubleValue() > 1 )
    {
      monitor.worked( 1 );
      monitorValue = new BigDecimal( 0 );
    }
    return monitorValue;
  }

}
