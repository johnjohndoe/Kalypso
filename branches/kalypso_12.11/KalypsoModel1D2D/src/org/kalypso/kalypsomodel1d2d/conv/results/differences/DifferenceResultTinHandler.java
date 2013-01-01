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
import java.io.IOException;
import java.math.BigDecimal;
import java.util.LinkedList;
import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.conv.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultType;
import org.kalypso.kalypsomodel1d2d.conv.results.TriangulatedSurfaceTriangleEater;
import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;
import org.kalypso.kalypsomodel1d2d.sim.MinMaxCatcher;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Triangle;
import org.kalypsodeegree.model.geometry.GM_TriangulatedSurface;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author Thomas Jung
 */
public class DifferenceResultTinHandler
{
  private final MinMaxCatcher m_minMaxCatcher = new MinMaxCatcher();

  private final ResultType m_parameter = ResultType.DIFFERENCE;

  private final GM_TriangulatedSurface m_master;

  private final GM_TriangulatedSurface m_slave;

  private final MathOperator m_operator;

  public DifferenceResultTinHandler( final GM_TriangulatedSurface master, final GM_TriangulatedSurface slave, final MathOperator operator )
  {
    m_master = master;
    m_slave = slave;
    m_operator = operator;
  }

  public MinMaxCatcher getMinMax( )
  {
    return m_minMaxCatcher;
  }

  public void generateDifferences( final IFile diffFile, final IProgressMonitor monitor ) throws CoreException
  {
    try
    {
      final String crs = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

      /* Create new workspace */
      final QName tinResultName = new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "TinResult" ); //$NON-NLS-1$

      final GMLWorkspace triangleWorkspace = FeatureFactory.createGMLWorkspace( tinResultName, null, null ); //$NON-NLS-1$
      final GM_TriangulatedSurface surface = GeometryFactory.createGM_TriangulatedSurface( crs );

      // FIXME: we should create binding classes for TinResult
      final Feature triangleFeature = triangleWorkspace.getRootFeature();
      triangleFeature.setProperty( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "triangulatedSurfaceMember" ), surface ); //$NON-NLS-1$
      triangleFeature.setProperty( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "unit" ), "[-]" ); //$NON-NLS-1$ //$NON-NLS-2$
      triangleFeature.setProperty( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "parameter" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.differences.DifferenceResultTinHandler.5" ) ); //$NON-NLS-1$ //$NON-NLS-2$

      generateDifferences( surface, m_parameter, monitor );

      if( surface.size() == 0 )
      {
        final IStatus status = new Status( IStatus.WARNING, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "DifferenceResultTinHandler.0" ) ); //$NON-NLS-1$
        throw new CoreException( status );
      }

      saveTin( diffFile, triangleWorkspace );
    }
    catch( GMLSchemaException | GM_Exception e )
    {
      // these exception happen if we got a problem else where, just a generic error message is sufficient
      final IStatus status = StatusUtilities.statusFromThrowable( e, Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.differences.DifferenceResultTinHandler.7" ) ); //$NON-NLS-1$
      throw new CoreException( status );
    }
  }

  private void generateDifferences( final GM_TriangulatedSurface surface, final ResultType parameter, final IProgressMonitor monitor )
  {
    final String crs = surface.getCoordinateSystem();

    // Loop over master triangles
    final TriangulatedSurfaceTriangleEater eater = new TriangulatedSurfaceTriangleEater( surface, parameter );

    // monitor:
    // 70% available
    // => 70 / masterSurface.size()

    // use monitor to display progress.
    final BigDecimal maxProgress = new BigDecimal( 70 );
    final BigDecimal stepNum = new BigDecimal( m_master.size() );
    final BigDecimal val = stepNum.divide( maxProgress, 5, BigDecimal.ROUND_HALF_UP );
    BigDecimal monitorValue = new BigDecimal( 0 );

    for( int i = 0; i < m_master.size(); i++ )
    {
      final GM_Triangle masterTriangle = m_master.get( i );

      if( monitor != null )
        monitorValue = updateMonitor( monitor, val, monitorValue );

      final List<GM_Point> nodeList = new LinkedList<>();

      final GM_Position[] masterRing = masterTriangle.getExteriorRing();
      for( int j = 0; j < masterRing.length - 1; j++ )
      {
        final GM_Point masterPoint = GeometryFactory.createGM_Point( masterRing[j], crs );

        final double masterZ = masterPoint.getZ();
        final double slaveZ = m_slave.getValue( masterPoint );

        if( !Double.isNaN( slaveZ ) )
        {
          final BigDecimal result = m_operator.getResult( new BigDecimal( masterZ ), new BigDecimal( slaveZ ) );

          // TODO: scale depends on data type.... should never be too small, because else we get situations,
          // where the scaled values of the input tins are different, but the difference value is 0.0
          final int newScale = 4;

          final BigDecimal scaledResult = result.setScale( newScale, BigDecimal.ROUND_HALF_UP );

          if( m_minMaxCatcher != null )
            m_minMaxCatcher.addResult( scaledResult );

          final GM_Point newPoint = GeometryFactory.createGM_Point( masterPoint.getX(), masterPoint.getY(), scaledResult.doubleValue(), crs );
          nodeList.add( newPoint );
        }
      }

      if( nodeList.size() == 3 )
        eater.addPoints( nodeList );
    }
    if( monitor != null )
      monitor.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.differences.DifferenceResultTinHandler.6" ) ); //$NON-NLS-1$

    eater.finished();
  }

  // FIXME: what is the meaning of this??
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

  private void saveTin( final IFile diffFile, final GMLWorkspace triangleWorkspace ) throws CoreException
  {
    try
    {
      final File tinResultFile = diffFile.getLocation().toFile();

      GmlSerializer.serializeWorkspace( tinResultFile, triangleWorkspace, "UTF-8" ); //$NON-NLS-1$
    }
    catch( IOException | GmlSerializeException e )
    {
      final IStatus status = new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "DifferenceResultTinHandler.1" ), e ); //$NON-NLS-1$
      throw new CoreException( status );
    }
    finally
    {
      diffFile.getParent().refreshLocal( IResource.DEPTH_INFINITE, new NullProgressMonitor() );
    }
  }
}