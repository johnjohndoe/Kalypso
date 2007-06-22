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
package org.kalypso.kalypsomodel1d2d.sim;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.commons.performance.TimeLogger;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler;
import org.kalypso.kalypsomodel1d2d.conv.RMA10S2GmlConv;
import org.kalypso.kalypsomodel1d2d.conv.results.NodeResultsHandler;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultType;
import org.kalypso.kalypsomodel1d2d.conv.results.TriangulatedSurfaceTriangleEater;
import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_TriangulatedSurface;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * This job processed one 2d-result file.
 * 
 * @author Gernot Belger
 */
public class ProcessResultsJob extends Job
{
  private final File m_outputDir;

  private final File m_inputFile;

  private final ISimulationDataProvider m_dataProvider;

  private final RMA10Calculation m_calculation;

  public ProcessResultsJob( final File inputFile, final File outputDir, final ISimulationDataProvider dataProvider, final RMA10Calculation calculation )
  {
    super( "1D2D-Ergebnisse auswerten: " + inputFile.getName() );

    m_inputFile = inputFile;
    m_outputDir = outputDir;
    m_dataProvider = dataProvider;
    m_calculation = calculation;
  }

  /**
   * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  protected IStatus run( final IProgressMonitor monitor )
  {
    monitor.beginTask( "Processing result file: " + m_inputFile.getName(), 10 );

    try
    {
      /* Zip .2d file to outputDir */
      final File outputZip2d = new File( m_outputDir, "original.2d.zip" );
      ZipUtilities.zip( outputZip2d, new File[] { m_inputFile }, m_inputFile.getParentFile() );
      monitor.worked( 1 );

      /* Read into NodeResults */
      read2DIntoGmlResults( m_inputFile, m_outputDir, m_dataProvider, m_calculation.getFlowModel() );
    }
    catch( final Throwable e )
    {
      return StatusUtilities.statusFromThrowable( e, "Fehler beim Lesen der Ergebnisdaten" );
    }

    // TODO Auto-generated method stub
    return Status.OK_STATUS;
  }

  public static File read2DIntoGmlResults( final File result2dFile, final File outputDir, final ISimulationDataProvider dataProvider, final IFlowRelationshipModel flowModel ) throws IOException, InvocationTargetException, GmlSerializeException, SimulationException, GM_Exception
  {
    final TimeLogger logger = new TimeLogger( "Start: lese .2d Ergebnisse" );

    if( dataProvider != null )
    {
      /* Write template sld into result folder */
      final URL resultStyleURL = (URL) dataProvider.getInputForID( "ResultStyle" );
      FileUtils.copyURLToFile( resultStyleURL, new File( outputDir, "result.sld" ) );

      final URL resultURL = (URL) dataProvider.getInputForID( "ResultMap" );
      FileUtils.copyURLToFile( resultURL, new File( outputDir, "result.gmt" ) );
    }

    final File gmlResultFile = new File( outputDir, "results.gml" );
    final File tinResultFile = new File( outputDir, "tin.gml" );

    InputStream is = null;
    try
    {
      is = new FileInputStream( result2dFile );

      /* GMLWorkspace für Ergebnisse anlegen */
      final GMLWorkspace resultWorkspace = FeatureFactory.createGMLWorkspace( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "NodeResultCollection" ), gmlResultFile.toURL(), null );

      /* .2d Datei lesen und GML füllen */
      final RMA10S2GmlConv conv = new RMA10S2GmlConv();

      // TODO: use multi-eater to write multiple triangles at once
      // move workspace creation into multi-eater
      final GMLWorkspace wspTriangleWorkspace = FeatureFactory.createGMLWorkspace( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "TinResult" ), tinResultFile.toURL(), null );
      final CS_CoordinateSystem crs = KalypsoCorePlugin.getDefault().getCoordinatesSystem();
      final GM_TriangulatedSurface surface = org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_TriangulatedSurface( crs );
      wspTriangleWorkspace.getRootFeature().setProperty( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "triangulatedSurfaceMember" ), surface );

      /* just for test purposes */
      final List<ResultType.TYPE> parameters = new ArrayList<ResultType.TYPE>();

      parameters.add( ResultType.TYPE.DEPTH );
      parameters.add( ResultType.TYPE.WATERLEVEL );
      parameters.add( ResultType.TYPE.VELOCITY );

      // final File resultHMOFile = new File( "D:/Projekte/kalypso_dev/post-processing/output.hmo" );
      // final HMOTriangleEater triangleEater = new HMOTriangleEater( resultHMOFile, parameters );
      final TriangulatedSurfaceTriangleEater triangleEater = new TriangulatedSurfaceTriangleEater( surface );
      final IRMA10SModelElementHandler handler = new NodeResultsHandler( resultWorkspace, triangleEater, flowModel );
      conv.setRMA10SModelElementHandler( handler );

      logger.takeInterimTime();
      logger.printCurrentInterim( "Beginn Parsen in : " );

      conv.parse( is );

      is.close();

      logger.takeInterimTime();
      logger.printCurrentInterim( "Fertig mit Lesen in : " );

      triangleEater.finished();

      /* GMLs in Datei schreiben */
      GmlSerializer.serializeWorkspace( gmlResultFile, resultWorkspace, "UTF-8" );
      GmlSerializer.serializeWorkspace( tinResultFile, wspTriangleWorkspace, "UTF-8" );

      return gmlResultFile;
    }
    finally
    {
      IOUtils.closeQuietly( is );

      logger.takeInterimTime();
      logger.printCurrentInterim( "Fertig mit Schreiben in : " );
    }
  }

}
