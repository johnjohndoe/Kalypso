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
import java.io.InputStream;
import java.math.BigDecimal;
import java.net.URL;
import java.text.DateFormat;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;

import javax.xml.namespace.QName;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.commons.performance.TimeLogger;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DDebug;
import org.kalypso.kalypsomodel1d2d.conv.RMA10S2GmlConv;
import org.kalypso.kalypsomodel1d2d.conv.results.MultiTriangleEater;
import org.kalypso.kalypsomodel1d2d.conv.results.NodeResultsHandler;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultType;
import org.kalypso.kalypsomodel1d2d.conv.results.TriangulatedSurfaceTriangleEater;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultType.TYPE;
import org.kalypso.kalypsomodel1d2d.conv.results.lengthsection.LengthSectionHandler1d;
import org.kalypso.kalypsomodel1d2d.conv.results.lengthsection.LengthSectionHandler2d;
import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IStepResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResultCollection;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;
import org.kalypso.model.wspm.schema.IWspmDictionaryConstants;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_TriangulatedSurface;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

/**
 * TODO: remove processing of the map
 * 
 * This job processed one 2d-result file. *
 * 
 * @author Gernot Belger
 */
public class ProcessResultsJob extends Job
{
  private final File m_outputDir;

  private final File m_inputFile;

  private final NodeResultMinMaxCatcher m_resultMinMaxCatcher = new NodeResultMinMaxCatcher();

  private List<TYPE> m_parameters;

  private final IStepResultMeta m_stepResultMeta;

  private final IFlowRelationshipModel m_flowModel;

  private final IControlModel1D2D m_controlModel;

  private final IFEDiscretisationModel1d2d m_discModel;

  private final Date m_stepDate;

  /**
   * @param inputFile
   *          the result 2d file
   * @param outputDir
   *          the directory in which the results get stored
   * @param flowModel
   *          the {@link IFlowRelationshipModel}
   * @param controlModel
   *          the {@link org.kalypso.kalypsomodel1d2d.schema.binding.model.ControlModel1D2D}
   * @param discModel
   * @param parameter
   *          the parameter that will be processed
   * @param unitResultMeta
   * @param stepDate
   *          The date which is determined by the result file name (i.e. step-number) and the control timeseries.
   */
  public ProcessResultsJob( final File inputFile, final File outputDir, final IFlowRelationshipModel flowModel, final IControlModel1D2D controlModel, final IFEDiscretisationModel1d2d discModel, final List<ResultType.TYPE> parameter, final Date stepDate, final ICalcUnitResultMeta unitResultMeta )
  {
    super( "1D2D-Ergebnisse auswerten: " + inputFile.getName() );

    KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s", "entering ProcessResultsJob\n" );

    m_inputFile = inputFile;
    m_outputDir = outputDir;
    m_flowModel = flowModel;
    m_controlModel = controlModel;
    m_discModel = discModel;
    m_parameters = parameter;
    m_stepDate = stepDate;

    if( unitResultMeta != null )
      m_stepResultMeta = unitResultMeta.addStepResult();
    else
      m_stepResultMeta = null;

    if( m_parameters == null )
    {
      m_parameters = new LinkedList<ResultType.TYPE>();
      m_parameters.add( ResultType.TYPE.DEPTH );
      m_parameters.add( ResultType.TYPE.TERRAIN );
      m_parameters.add( ResultType.TYPE.VELOCITY );
      m_parameters.add( ResultType.TYPE.WATERLEVEL );
    }
  }

  /**
   * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public IStatus run( final IProgressMonitor monitor )
  {
    KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s", "running ProcessResultsJob\n" );

    final String timeStepName;
    if( m_stepDate == ResultManager.STEADY_DATE )
      timeStepName = "stationärer Rechenlauf";
    else if( m_stepDate == ResultManager.MAXI_DATE )
      timeStepName = "Maximalwerte";
    else
      timeStepName = String.format( "Zeitschritt - %1$te.%1$tm.%1$tY %1$tH:%1$tM %1$tZ", m_stepDate );

    monitor.beginTask( "Ergebnisse werden ausgewertet - " + timeStepName, 10 );
    monitor.subTask( "Ergebnisse werden ausgewertet - " + timeStepName );

    try
    {
      /* Zip .2d file to outputDir */
      final File[] files = new File[] { m_inputFile };
      final File outputZip2d = new File( m_outputDir, "original.2d.zip" );
      ZipUtilities.zip( outputZip2d, files, m_inputFile.getParentFile() );
      ResultMeta1d2dHelper.addDocument( m_stepResultMeta, "RMA-Rohdaten", "ASCII Ergebnisdatei(en) des RMA10 Rechenkerns", IDocumentResultMeta.DOCUMENTTYPE.coreDataZip, new Path( "original.2d.zip" ), Status.OK_STATUS, null, null );

      ProgressUtilities.worked( monitor, 1 );

      /* Read into NodeResults */
      read2DIntoGmlResults();
    }
    catch( final Throwable e )
    {
      return StatusUtilities.statusFromThrowable( e, "Fehler beim Lesen der Ergebnisdaten" );
    }

    return Status.OK_STATUS;
  }

  public File read2DIntoGmlResults( ) throws Exception
  {
    KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s", "read results into GML...\n" );

    final Runtime runtime = Runtime.getRuntime();
    runtime.gc();

    final TimeLogger logger = new TimeLogger( "Start: lese .2d Ergebnisse" );

    final File gmlResultFile = new File( m_outputDir, "results.gml" );

    InputStream is = null;
    try
    {
      is = new FileInputStream( m_inputFile );

      /* GMLWorkspace für Ergebnisse anlegen */
      final GMLWorkspace resultWorkspace = FeatureFactory.createGMLWorkspace( INodeResultCollection.QNAME, gmlResultFile.toURL(), null );
      final URL lsObsUrl = LengthSectionHandler2d.class.getResource( "resources/lengthSectionTemplate.gml" );

      final String componentID = IWspmDictionaryConstants.LS_COMPONENT_STATION;
      final LengthSectionHandler1d lsHandler = new LengthSectionHandler1d( componentID, lsObsUrl );

      /* .2d Datei lesen und GML füllen */
      final RMA10S2GmlConv conv = new RMA10S2GmlConv();

      final String crs = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
      final MultiTriangleEater multiEater = new MultiTriangleEater();

      for( final ResultType.TYPE parameter : m_parameters )
      {
        /* GML(s) */

        if( parameter == ResultType.TYPE.TERRAIN )
        {
          /* create TIN-Dir for FEM terrain model */
          final String calcUnitPath = m_outputDir.getParent();

          final File modelPath = new File( calcUnitPath, "model" );
          if( !modelPath.exists() )
          {
            modelPath.mkdirs();

            final File modelTinPath = new File( modelPath, "Tin" );
            modelTinPath.mkdirs();

            final File tinResultFile = new File( modelTinPath, "tin.gml" );
            final GMLWorkspace triangleWorkspace = FeatureFactory.createGMLWorkspace( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "TinResult" ), tinResultFile.toURL(), null );
            final GM_TriangulatedSurface surface = org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_TriangulatedSurface( crs );
            final Feature triangleFeature = triangleWorkspace.getRootFeature();
            triangleFeature.setProperty( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "triangulatedSurfaceMember" ), surface );

            final TriangulatedSurfaceTriangleEater gmlTriangleEater = new TriangulatedSurfaceTriangleEater( tinResultFile, triangleWorkspace, surface, parameter );

            multiEater.addEater( gmlTriangleEater );
          }
        }
        else
        {
          /* create TIN-Dir for results */
          final File tinPath = new File( m_outputDir, "Tin" );
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

              break;
            case VELOCITY:
              triangleFeature.setProperty( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "unit" ), "m/s" );
              triangleFeature.setProperty( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "parameter" ), "Geschwindigkeit" );

            case VELOCITY_X:
              triangleFeature.setProperty( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "unit" ), "m/s" );
              triangleFeature.setProperty( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "parameter" ), "Geschwindigkeitskomponente X" );
              break;

            case VELOCITY_Y:
              triangleFeature.setProperty( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "unit" ), "m/s" );
              triangleFeature.setProperty( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "parameter" ), "Geschwindigkeitskomponente Y" );

              break;
            case WATERLEVEL:
              triangleFeature.setProperty( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "unit" ), "müNN" );
              triangleFeature.setProperty( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "parameter" ), "Wasserspiegel" );

              break;

            case SHEARSTRESS:
              triangleFeature.setProperty( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "unit" ), "N/m²" );
              triangleFeature.setProperty( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "parameter" ), "Sohlschubspannung" );

              break;

            default:
              throw new UnsupportedOperationException();
          }

          final TriangulatedSurfaceTriangleEater gmlTriangleEater = new TriangulatedSurfaceTriangleEater( tinResultFile, triangleWorkspace, surface, parameter );

          multiEater.addEater( gmlTriangleEater );
        }
      }

      final NodeResultsHandler handler = new NodeResultsHandler( resultWorkspace, multiEater, m_flowModel, m_controlModel, m_discModel, m_resultMinMaxCatcher, lsHandler );
      conv.setRMA10SModelElementHandler( handler );

      logger.takeInterimTime();
      logger.printCurrentInterim( "Starte Auswertung (" + m_inputFile.getName() + ") : " );

      conv.parse( is );

      is.close();

      logger.takeInterimTime();
      logger.printCurrentInterim( "Fertig mit Einlesen in : " );

      // finish MultiEater and engage serializer
      KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s", "finishing result eater..." );
      multiEater.finished();
      KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s", " done.\n" );

      /* Node-GML in Datei schreiben */
      GmlSerializer.serializeWorkspace( gmlResultFile, resultWorkspace, "CP1252" );

      /* LengthSection in Datei schreiben */

      ICalculationUnit1D[] calcUnits = lsHandler.getCalcUnits();

      for( ICalculationUnit1D calcUnit : calcUnits )
      {
        final File lsObsFile = new File( m_outputDir, "lengthSection_" + calcUnit.getGmlID() + ".gml" );

        final IObservation<TupleResult> lsObs = lsHandler.getObservation( calcUnit );
        GMLWorkspace lsObsWorkspace = lsHandler.getWorkspace( calcUnit );
        if( lsObs.getResult().size() > 0 )
        {
          ObservationFeatureFactory.toFeature( lsObs, lsObsWorkspace.getRootFeature() );
          GmlSerializer.serializeWorkspace( lsObsFile, lsObsWorkspace, "CP1252" );

          /* length section entry in result db */
          // TODO: use station range for min max...
          ResultMeta1d2dHelper.addDocument( m_stepResultMeta, "Längsschnitt " + calcUnit.getName(), "1d-Längsschnitt", IDocumentResultMeta.DOCUMENTTYPE.lengthSection, new Path( lsObsFile.getName() ), Status.OK_STATUS, new BigDecimal( 0 ), new BigDecimal( 0 ) );
        }

      }

      logger.takeInterimTime();
      logger.printCurrentInterim( "Fertig mit Ergebnisse schreiben in : " );

      BigDecimal min;
      BigDecimal max;

      for( final ResultType.TYPE parameter : m_parameters )
      {
        /* GML(s) */

        /* result db */

        switch( parameter )
        {
          case TERRAIN:

            final ICalcUnitResultMeta calcUnitResult = (ICalcUnitResultMeta) m_stepResultMeta.getParent();

            final IFeatureWrapperCollection<IResultMeta> children = calcUnitResult.getChildren();

            /* check if there exists already an entry for terrainTin */
            boolean terrainExists = false;

            for( final IResultMeta resultMeta : children )
            {
              if( resultMeta instanceof IDocumentResultMeta )
              {
                final IDocumentResultMeta document = (IDocumentResultMeta) resultMeta;
                if( document.getDocumentType() == IDocumentResultMeta.DOCUMENTTYPE.tinTerrain )
                  terrainExists = true;
              }
            }

            /* if not, add it */
            if( terrainExists == false )
            {
              min = new BigDecimal( m_resultMinMaxCatcher.getMinTerrain() ).setScale( 3, BigDecimal.ROUND_HALF_UP );
              max = new BigDecimal( m_resultMinMaxCatcher.getMaxTerrain() ).setScale( 3, BigDecimal.ROUND_HALF_UP );
              ResultMeta1d2dHelper.addDocument( calcUnitResult, "Modellhöhen", "TIN der Modellhöhen", IDocumentResultMeta.DOCUMENTTYPE.tinTerrain, new Path( "model/Tin/tin_TERRAIN.gml" ), Status.OK_STATUS, min, max );
            }

            break;

          case DEPTH:

            min = new BigDecimal( m_resultMinMaxCatcher.getMinDepth() ).setScale( 3, BigDecimal.ROUND_HALF_UP );
            max = new BigDecimal( m_resultMinMaxCatcher.getMaxDepth() ).setScale( 3, BigDecimal.ROUND_HALF_UP );
            ResultMeta1d2dHelper.addDocument( m_stepResultMeta, "Fließtiefen", "TIN der Fließtiefen", IDocumentResultMeta.DOCUMENTTYPE.tinDepth, new Path( "Tin/tin_DEPTH.gml" ), Status.OK_STATUS, min, max );
            break;

          case VELOCITY:

            min = new BigDecimal( m_resultMinMaxCatcher.getMinVelocityAbs() ).setScale( 3, BigDecimal.ROUND_HALF_UP );
            max = new BigDecimal( m_resultMinMaxCatcher.getMaxVelocityAbs() ).setScale( 3, BigDecimal.ROUND_HALF_UP );
            ResultMeta1d2dHelper.addDocument( m_stepResultMeta, "Geschwindigkeiten", "TIN der tiefengemittelten Fließgeschwindigkeiten", IDocumentResultMeta.DOCUMENTTYPE.tinVelo, new Path( "Tin/tin_VELOCITY.gml" ), Status.OK_STATUS, min, max );

            break;

          case VELOCITY_X:

            min = new BigDecimal( m_resultMinMaxCatcher.getMinVelocityAbs() ).setScale( 3, BigDecimal.ROUND_HALF_UP );
            max = new BigDecimal( m_resultMinMaxCatcher.getMaxVelocityAbs() ).setScale( 3, BigDecimal.ROUND_HALF_UP );
            ResultMeta1d2dHelper.addDocument( m_stepResultMeta, "Geschwindigkeitskomponente X", "TIN der tiefengemittelten Geschwindigkeitskomponenten in X-Richtung", IDocumentResultMeta.DOCUMENTTYPE.tinVelo, new Path( "Tin/tin_VELOCITY_X.gml" ), Status.OK_STATUS, min, max );

            break;

          case VELOCITY_Y:

            min = new BigDecimal( m_resultMinMaxCatcher.getMinVelocityAbs() ).setScale( 3, BigDecimal.ROUND_HALF_UP );
            max = new BigDecimal( m_resultMinMaxCatcher.getMaxVelocityAbs() ).setScale( 3, BigDecimal.ROUND_HALF_UP );
            ResultMeta1d2dHelper.addDocument( m_stepResultMeta, "Geschwindigkeitskomponente Y", "TIN der tiefengemittelten Geschwindigkeitskomponenten in Y-Richtung", IDocumentResultMeta.DOCUMENTTYPE.tinVelo, new Path( "Tin/tin_VELOCITY_Y.gml" ), Status.OK_STATUS, min, max );

            break;

          case WATERLEVEL:

            min = new BigDecimal( m_resultMinMaxCatcher.getMinWaterlevel() ).setScale( 3, BigDecimal.ROUND_HALF_UP );
            max = new BigDecimal( m_resultMinMaxCatcher.getMaxWaterlevel() ).setScale( 3, BigDecimal.ROUND_HALF_UP );
            ResultMeta1d2dHelper.addDocument( m_stepResultMeta, "Wasserspiegellagen", "TIN der Wasserspiegellagen", IDocumentResultMeta.DOCUMENTTYPE.tinWsp, new Path( "Tin/tin_WATERLEVEL.gml" ), Status.OK_STATUS, min, max );

            break;

          case SHEARSTRESS:

            min = new BigDecimal( m_resultMinMaxCatcher.getMinShearStress() ).setScale( 3, BigDecimal.ROUND_HALF_UP );
            max = new BigDecimal( m_resultMinMaxCatcher.getMaxShearStress() ).setScale( 3, BigDecimal.ROUND_HALF_UP );
            ResultMeta1d2dHelper.addDocument( m_stepResultMeta, "Sohlschubspannung", "TIN der Sohlschubspannungen", IDocumentResultMeta.DOCUMENTTYPE.tinShearStress, new Path( "Tin/tin_SHEARSTRESS.gml" ), Status.OK_STATUS, min, max );

            break;

          default:
            throw new UnsupportedOperationException();
        }
      }

      min = new BigDecimal( m_resultMinMaxCatcher.getMinVelocityAbs() ).setScale( 3, BigDecimal.ROUND_HALF_UP );
      max = new BigDecimal( m_resultMinMaxCatcher.getMaxVelocityAbs() ).setScale( 3, BigDecimal.ROUND_HALF_UP );
      ResultMeta1d2dHelper.addDocument( m_stepResultMeta, "Vektoren", "Ergebnisse an den Knoten", IDocumentResultMeta.DOCUMENTTYPE.nodes, new Path( "results.gml" ), Status.OK_STATUS, min, max );

      /* HMO(s) */
      /*
       * try { final File resultHMOFile = new File( "D:/Projekte/kalypso_dev/post-processing/output.hmo" ); final
       * HMOTriangleEater hmoTriangleEater = new HMOTriangleEater( resultHMOFile, parameter ); multiEater.addEater(
       * hmoTriangleEater ); } catch (Exception e) { e.printStackTrace(); }
       */

      final Date time = handler.getTime();

      // TODO: maybe check if time and stepTime are equal?

      addToResultDB( m_stepResultMeta, m_stepDate, m_outputDir, time );

      return gmlResultFile;
    }
    finally
    {
      IOUtils.closeQuietly( is );

      logger.takeInterimTime();
      logger.printCurrentInterim( "Fertig mit Schreiben in : " );

      runtime.gc();
    }
  }

  private static void addToResultDB( final IStepResultMeta stepResultMeta, final Date stepDate, final File outputDir, final Date time )
  {
    // TODO: retrieve time zone from central plugin preferences
    final DateFormat dateFormatter = DateFormat.getDateTimeInstance( DateFormat.SHORT, DateFormat.LONG );
    dateFormatter.setTimeZone( KalypsoGisPlugin.getDefault().getDisplayTimeZone() );

    if( ResultManager.STEADY_DATE.equals( stepDate ) )
    {
      stepResultMeta.setName( "stationär" );
      stepResultMeta.setDescription( "stationärer Rechenlauf" );
      stepResultMeta.setStepType( IStepResultMeta.STEPTYPE.steady );
      stepResultMeta.setStepTime( null );
    }
    else if( ResultManager.MAXI_DATE.equals( stepDate ) )
    {
      stepResultMeta.setName( "Maximalwerte" );
      stepResultMeta.setDescription( "Maximalwerte des intationären Rechenlaufs" );
      stepResultMeta.setStepType( IStepResultMeta.STEPTYPE.maximum );
      stepResultMeta.setStepTime( null );
    }
    else
    {
      // TODO: check for right time zone
      final String dateString = dateFormatter.format( stepDate );
      stepResultMeta.setName( String.format( "Zeitschritt (%s)", dateString ) );
      stepResultMeta.setDescription( "instationärer Rechenlauf zum Zeitpunkt: " + dateString );
      stepResultMeta.setStepType( IStepResultMeta.STEPTYPE.unsteady );
      stepResultMeta.setStepTime( stepDate );
    }

    stepResultMeta.setPath( new Path( outputDir.getName() ) );
  }

  public NodeResultMinMaxCatcher getMinMaxData( )
  {
    return m_resultMinMaxCatcher;
  }
}
