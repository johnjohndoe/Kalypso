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
import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.xml.namespace.QName;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.commons.performance.TimeLogger;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.kalypsomodel1d2d.conv.RMA10S2GmlConv;
import org.kalypso.kalypsomodel1d2d.conv.results.MultiTriangleEater;
import org.kalypso.kalypsomodel1d2d.conv.results.NodeResultsHandler;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultType;
import org.kalypso.kalypsomodel1d2d.conv.results.TriangulatedSurfaceTriangleEater;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultType.TYPE;
import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IStepResultMeta;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_TriangulatedSurface;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * TODO: remove processing of the map
 * 
 * This job processed one 2d-result file.
 * 
 * @author Gernot Belger
 */
public class ProcessResultsJob extends Job
{
  private final File m_outputDir;

  private final File m_inputFile;

  private final ISimulationDataProvider m_dataProvider;

  private static RMA10Calculation m_calculation;

  private final NodeResultMinMaxCatcher m_resultMinMaxCatcher = new NodeResultMinMaxCatcher();

  private final int m_timeStepNr;

  private static List<TYPE> m_parameters;

  private final IStepResultMeta m_stepResultMeta;

  public ProcessResultsJob( final File inputFile, final File outputDir, final ISimulationDataProvider dataProvider, final RMA10Calculation calculation, final List<ResultType.TYPE> parameter, final int timeStepNr, final ICalcUnitResultMeta unitResultMeta )
  {
    super( "1D2D-Ergebnisse auswerten: " + inputFile.getName() );

    m_inputFile = inputFile;
    m_outputDir = outputDir;
    m_dataProvider = dataProvider;
    m_calculation = calculation;
    m_parameters = parameter;
    m_timeStepNr = timeStepNr;
    m_stepResultMeta = unitResultMeta.addStepResult();

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
      // final File exeLog = new File( m_inputFile.getParentFile().toString() + "/exe.log" );
      // final File exeErr = new File( m_inputFile.getParentFile().toString() + "/exe.err" );
      // final File outputOut = new File( m_inputFile.getParentFile().toString() + "/result/Output.out" );
      final File[] files = new File[] { m_inputFile /* , exeLog, exeErr, outputOut */};
      final File outputZip2d = new File( m_outputDir, "original.2d.zip" );
      ZipUtilities.zip( outputZip2d, files, m_inputFile.getParentFile() );
      m_stepResultMeta.addDocument( "RMA-Rohdaten", "ASCII Ergebnisdatei(en) des RMA10 Rechenkerns", IDocumentResultMeta.DOCUMENTTYPE.coreDataZip, "original.2d.zip", Status.OK_STATUS );

      monitor.worked( 1 );

      /* Read into NodeResults */
      read2DIntoGmlResults( m_inputFile, m_outputDir, m_dataProvider, m_resultMinMaxCatcher, m_timeStepNr, m_stepResultMeta );
    }
    catch( final Throwable e )
    {
      return StatusUtilities.statusFromThrowable( e, "Fehler beim Lesen der Ergebnisdaten" );
    }

    return Status.OK_STATUS;
  }

  public static File read2DIntoGmlResults( final File result2dFile, final File outputDir, final ISimulationDataProvider dataProvider, final NodeResultMinMaxCatcher minMaxCatcher, final int timeStepNum, final IStepResultMeta stepResultMeta ) throws IOException, InvocationTargetException, GmlSerializeException, SimulationException, GM_Exception
  {
    final TimeLogger logger = new TimeLogger( "Start: lese .2d Ergebnisse" );

    if( dataProvider != null )
    {
      /* Write template sld into result folder */
      final URL resultStyleURL = (URL) dataProvider.getInputForID( "ResultStepTemplate" );
      ZipUtilities.unzip( resultStyleURL, outputDir );
    }

    final File gmlResultFile = new File( outputDir, "results.gml" );

    InputStream is = null;
    try
    {
      is = new FileInputStream( result2dFile );

      /* GMLWorkspace für Ergebnisse anlegen */
      final GMLWorkspace resultWorkspace = FeatureFactory.createGMLWorkspace( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "NodeResultCollection" ), gmlResultFile.toURL(), null );

      /* .2d Datei lesen und GML füllen */
      final RMA10S2GmlConv conv = new RMA10S2GmlConv();

      /* just for test purposes */
      final List<ResultType.TYPE> parameters = new ArrayList<ResultType.TYPE>();

      parameters.add( ResultType.TYPE.DEPTH );
      parameters.add( ResultType.TYPE.WATERLEVEL );
      parameters.add( ResultType.TYPE.VELOCITY );

      final CS_CoordinateSystem crs = KalypsoCorePlugin.getDefault().getCoordinatesSystem();
      final MultiTriangleEater multiEater = new MultiTriangleEater();

      for( final ResultType.TYPE parameter : m_parameters )
      {
        /* GML(s) */

        /* create TIN-Dir */
        final File tinPath = new File( outputDir, "Tin" );
        tinPath.mkdirs();

        final File tinResultFile = new File( tinPath, "tin.gml" );
        final GMLWorkspace triangleWorkspace = FeatureFactory.createGMLWorkspace( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "TinResult" ), tinResultFile.toURL(), null );
        final GM_TriangulatedSurface surface = org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_TriangulatedSurface( crs );
        final Feature triangleFeature = triangleWorkspace.getRootFeature();
        triangleFeature.setProperty( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "triangulatedSurfaceMember" ), surface );

        switch( parameter )
        {
          case DEPTH:
            triangleFeature.setProperty( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "unit" ), "m" );
            triangleFeature.setProperty( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "parameter" ), "Fließtiefe" );
            stepResultMeta.addDocument( "Fließtiefen", "TIN der Fließtiefen", IDocumentResultMeta.DOCUMENTTYPE.tinDepth, "Tin/tin_DEPTH.gml", Status.OK_STATUS );

            break;
          case VELOCITY:
            triangleFeature.setProperty( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "unit" ), "m/s" );
            triangleFeature.setProperty( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "parameter" ), "Geschwindigkeit" );
            stepResultMeta.addDocument( "Geschwindigkeiten", "TIN der tiefengemittelten Fließgeschwindigkeiten", IDocumentResultMeta.DOCUMENTTYPE.tinVelo, "Tin/tin_VELOCITY.gml", Status.OK_STATUS );

            break;
          case WATERLEVEL:
            triangleFeature.setProperty( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "unit" ), "müNN" );
            triangleFeature.setProperty( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "parameter" ), "Wasserspiegel" );
            stepResultMeta.addDocument( "Wasserspiegellagen", "TIN der Wasserspiegellagen", IDocumentResultMeta.DOCUMENTTYPE.tinWsp, "Tin/tin_WATERLEVEL.gml", Status.OK_STATUS );

            break;

          default:
            throw new UnsupportedOperationException();
        }

        final TriangulatedSurfaceTriangleEater gmlTriangleEater = new TriangulatedSurfaceTriangleEater( tinResultFile, triangleWorkspace, surface, parameter );

        multiEater.addEater( gmlTriangleEater );

        /* HMO(s) */
        /*
         * try { final File resultHMOFile = new File( "D:/Projekte/kalypso_dev/post-processing/output.hmo" ); final
         * HMOTriangleEater hmoTriangleEater = new HMOTriangleEater( resultHMOFile, parameter ); multiEater.addEater(
         * hmoTriangleEater ); } catch (Exception e) { e.printStackTrace(); }
         */
      }

      final NodeResultsHandler handler = new NodeResultsHandler( resultWorkspace, multiEater, m_calculation, minMaxCatcher );
      conv.setRMA10SModelElementHandler( handler );

      logger.takeInterimTime();
      logger.printCurrentInterim( "Beginn Parsen in : " );

      conv.parse( is );

      is.close();

      logger.takeInterimTime();
      logger.printCurrentInterim( "Fertig mit Lesen in : " );

      // finish MultiEater and engage serializer
      multiEater.finished();

      /* Node-GML in Datei schreiben */
      GmlSerializer.serializeWorkspace( gmlResultFile, resultWorkspace, "CP1252" );
      stepResultMeta.addDocument( "Knotenwerte", "Ergebnisse an den Knoten", IDocumentResultMeta.DOCUMENTTYPE.nodes, "results.gml", Status.OK_STATUS );

      final Date time = handler.getTime();
      addToResultDB( stepResultMeta, timeStepNum, outputDir, time );

      return gmlResultFile;
    }
    finally
    {
      IOUtils.closeQuietly( is );

      logger.takeInterimTime();
      logger.printCurrentInterim( "Fertig mit Schreiben in : " );
    }
  }

  private static void addToResultDB( final IStepResultMeta stepResultMeta, final int timeStepNum, final File outputDir, final Date time )
  {
    // TODO: retrieve timezone from central plugin preferences
    final DateFormat dateFormatter = DateFormat.getDateTimeInstance();

    if( outputDir.getName().equals( "steady" ) )
    {
      stepResultMeta.setName( "stationär" );
      stepResultMeta.setDescription( "stationärer Rechenlauf" );
    }
    else
    {
      final String dateString = dateFormatter.format( time );
      stepResultMeta.setName( String.format( "Zeitschritt %s (%s)", timeStepNum, dateString ) );
      stepResultMeta.setDescription( "instationärer Rechenlauf zum Zeitpunkt: " + dateString );
    }

    stepResultMeta.setStepTime( time );
    stepResultMeta.setStepNumber( timeStepNum );
  }

  public NodeResultMinMaxCatcher getMinMaxData( )
  {
    return m_resultMinMaxCatcher;

  }
}
