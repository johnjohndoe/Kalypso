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
package org.kalypso.kalypsomodel1d2d.sim;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import javax.xml.datatype.XMLGregorianCalendar;

import org.apache.commons.io.IOUtils;
import org.apache.commons.vfs2.FileObject;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.io.VFSUtilities;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.commons.performance.TimeLogger;
import org.kalypso.commons.vfs.FileSystemManagerWrapper;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DDebug;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.conv.SWANDataConverterHelper;
import org.kalypso.kalypsomodel1d2d.conv.results.ITriangleEater;
import org.kalypso.kalypsomodel1d2d.conv.results.MultiTriangleEater;
import org.kalypso.kalypsomodel1d2d.conv.results.NodeResultHelper;
import org.kalypso.kalypsomodel1d2d.conv.results.NodeResultsHandler;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultType;
import org.kalypso.kalypsomodel1d2d.conv.results.SWANResultsReader;
import org.kalypso.kalypsomodel1d2d.conv.results.lengthsection.LengthSectionHandler1d;
import org.kalypso.kalypsomodel1d2d.conv.results.lengthsection.LengthSectionHandler2d;
import org.kalypso.kalypsomodel1d2d.conv.telemac.SerafinReader;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IStepResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResultCollection;
import org.kalypso.kalypsomodel1d2d.schema.dict.Kalypso1D2DDictConstants;
import org.kalypso.kalypsomodel1d2d.sim.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.model.wspm.schema.IWspmDictionaryConstants;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

/**
 * @author ig
 */
public class ProcessResultTelemacOperation implements ICoreRunnableWithProgress
{
  private static FileSystemManagerWrapper m_vfsManager;

  private File m_outputDir;

  private FileObject m_inputFile;

  private final NodeResultMinMaxCatcher resultMinMaxCatcher = new NodeResultMinMaxCatcher();

  private List<ResultType> m_parameters;

  private ICalcUnitResultMeta m_unitResultMeta;

  private IFlowRelationshipModel m_flowModel;

  private IControlModel1D2D m_controlModel;

  private IFEDiscretisationModel1d2d m_discModel;

  private Date m_stepDate;

  private FileObject m_inputResFileSWAN;

  private boolean m_boolDoFullEvaluate = false;

  private Map<String, Map<GM_Position, Double>> m_mapResults = null;

//  private Map<String, Map<GM_Position, Double>> m_mapResultsTelemac = null;

  private Date[] m_stepDates;

  private HashMap<Date, Integer> m_mapStepToDates = null;

  /**
   * @param inputFile
   *          the result slf file
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
  public ProcessResultTelemacOperation( final FileObject file, final File outputDir, final IFlowRelationshipModel flowModel, final IControlModel1D2D controlModel, final IFEDiscretisationModel1d2d discModel, final List<ResultType> parameter, final Date[] allDates, final ICalcUnitResultMeta unitResultMeta )
  {
    init( file, outputDir, null, flowModel, controlModel, discModel, parameter, allDates, unitResultMeta, true );

  }

  public ProcessResultTelemacOperation( final FileObject file, final File outputDir, final FileObject fileResSWAN, final IFlowRelationshipModel flowModel, final IControlModel1D2D controlModel, final IFEDiscretisationModel1d2d discModel, final List<ResultType> parameter, final Date[] allDates, final ICalcUnitResultMeta unitResultMeta, final boolean doFullEvaluate )
  {
    init( file, outputDir, fileResSWAN, flowModel, controlModel, discModel, parameter, allDates, unitResultMeta, doFullEvaluate );
  }

  private void init( final FileObject file, final File outputDir, final FileObject fileResSWAN, final IFlowRelationshipModel flowModel, final IControlModel1D2D controlModel, final IFEDiscretisationModel1d2d discModel, final List<ResultType> parameter, final Date[] allDates, final ICalcUnitResultMeta unitResultMeta, final boolean boolDoFullEvaluate )
  {
    KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s", Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.2" ) ); //$NON-NLS-1$ //$NON-NLS-2$

    m_inputFile = file;
    m_outputDir = outputDir;
    m_flowModel = flowModel;
    m_controlModel = controlModel;
    m_discModel = discModel;
    m_parameters = parameter;
    m_stepDates = allDates;
    m_inputResFileSWAN = fileResSWAN;

    m_boolDoFullEvaluate = boolDoFullEvaluate;

    if( unitResultMeta != null )
      m_unitResultMeta = unitResultMeta;
    else
      throw new UnsupportedOperationException();
//      stepResultMeta = null;

    if( m_parameters == null )
    {
      m_parameters = new LinkedList<>();
      m_parameters.add( ResultType.DEPTH );
//      m_parameters.add( ResultType.TERRAIN );
      m_parameters.add( ResultType.VELOCITY );
      m_parameters.add( ResultType.WATERLEVEL );
      if( fileResSWAN != null )
      {
        m_parameters.add( ResultType.WAVEHSIG );
        m_parameters.add( ResultType.WAVEDIR );
        m_parameters.add( ResultType.WAVEPER );
      }
    }
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.4" ) ); //$NON-NLS-1$

    final String timeStepName = createTimeStepName();

    monitor.beginTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.8" ) + timeStepName, 10 ); //$NON-NLS-1$
    monitor.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.9" ) + timeStepName ); //$NON-NLS-1$
    List<IStatus> executionsStatus = new ArrayList<>();
    try
    {
      m_vfsManager = VFSUtilities.getNewManager();

      executionsStatus.add( zipInputFile() );

      ProgressUtilities.worked( monitor, 1 );

      executionsStatus.add( readActWaveRes() );

      ProgressUtilities.worked( monitor, 1 );

      executionsStatus.add( readTelemacResults() );

      // FIXME: only add this metadata if we added a zip file?
      ResultMeta1d2dHelper.addDocument( m_unitResultMeta.addStepResult(), ResultMeta1d2dHelper.TELEMAC_RAW_DATA_META_NAME, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.12" ), IDocumentResultMeta.DOCUMENTTYPE.coreDataZip, new Path( ResultMeta1d2dHelper.ORIGINAL_SLF_FILE_NAME + ".zip" ), Status.OK_STATUS, null, null ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

      ProgressUtilities.worked( monitor, 1 );
    }
    catch( final Throwable e )
    {
      final String filename = m_inputFile.getName().getBaseName();
      final String msg = Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.14", filename ); //$NON-NLS-1$
      return new Status( IStatus.ERROR, PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), msg, e );
    }
    finally
    {
      if( m_vfsManager != null )
        m_vfsManager.close();
    }

    final IStatus[] multiStati = executionsStatus.toArray( new IStatus[executionsStatus.size()] );

    final MultiStatus multiStatus = new MultiStatus( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), ISimulation1D2DConstants.CODE_POST, multiStati, "", null ); //$NON-NLS-1$
    if( multiStatus.isOK() )
      return new Status( IStatus.OK, KalypsoModel1D2DPlugin.PLUGIN_ID, ISimulation1D2DConstants.CODE_POST, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ResultManager.7" ), null ); //$NON-NLS-1$

    if( multiStatus.matches( IStatus.CANCEL ) )
      return new Status( IStatus.WARNING, KalypsoModel1D2DPlugin.PLUGIN_ID, ISimulation1D2DConstants.CODE_POST, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ResultManager.8" ), null ); //$NON-NLS-1$

    if( multiStatus.matches( IStatus.WARNING ) )
      return new MultiStatus( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), ISimulation1D2DConstants.CODE_POST, multiStati, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ResultManager.9" ), null ); //$NON-NLS-1$

    if( multiStatus.matches( IStatus.ERROR ) )
      return new MultiStatus( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), ISimulation1D2DConstants.CODE_POST, multiStati, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ResultManager.10" ), null ); //$NON-NLS-1$

    return new MultiStatus( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), ISimulation1D2DConstants.CODE_POST, multiStati, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ResultManager.11" ), null ); //$NON-NLS-1$
//    return Status.OK_STATUS;
  }

  private String createTimeStepName( )
  {
    if( m_stepDate == ResultManager.STEADY_DATE )
      return Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.5" ); //$NON-NLS-1$

    if( m_stepDate == ResultManager.MAXI_DATE )
      return Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.6" ); //$NON-NLS-1$

    return Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.7", m_stepDate ); //$NON-NLS-1$
  }

  private IStatus readTelemacResults( )
  {
    final SerafinReader convTelemac = new SerafinReader();
    final FileObject inputFile = getOrUnzipResult( m_inputFile, m_outputDir );
    if( inputFile == null )
    {
      return new Status( IStatus.ERROR, PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), "error - no result file found!" );
    }
    try
    {
      convTelemac.setFile( new File( inputFile.getURL().toURI() ) );
      convTelemac.doReadAll();
    }
    catch( URISyntaxException | IOException e1 )
    {
      e1.printStackTrace();
      final String msg = "error while reading telemac results file";
      return new Status( IStatus.ERROR, PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), msg, e1 );
    }
//    Map<String, Map<String, List<Double>>> mapResults = convTelemac.doRead();

    for( Date stepDate : m_stepDates )
    {
//    stepDate = findStepDate( file );

//    m_geoLog.formatLog( IStatus.INFO, CODE_RUNNING_FINE, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ResultManager.14" ), inputFile.getURL().toString() ); //$NON-NLS-1$

      final String outDirName = NodeResultHelper.createOutDirName( stepDate );

      final File resultOutputDir = new File( m_outputDir, outDirName );
      resultOutputDir.mkdirs();
      KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s", Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.16" ) ); //$NON-NLS-1$ //$NON-NLS-2$

      final Runtime runtime = Runtime.getRuntime();
      runtime.gc();

      final TimeLogger logger = new TimeLogger( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.17" ) ); //$NON-NLS-1$

      final File gmlZipResultFile = new File( resultOutputDir, "results.gz" ); //$NON-NLS-1$
      IStepResultMeta stepResultMeta = m_unitResultMeta.addStepResult();
      try
      {
        /* GMLWorkspace für Ergebnisse anlegen */
        final GMLWorkspace resultWorkspace = FeatureFactory.createGMLWorkspace( INodeResultCollection.QNAME, gmlZipResultFile.toURI().toURL(), null );
        final URL lsObsUrl = LengthSectionHandler2d.class.getResource( "resources/lengthSectionTemplate.gml" ); //$NON-NLS-1$

        final String componentID = IWspmDictionaryConstants.LS_COMPONENT_STATION;
        final LengthSectionHandler1d lsHandler = new LengthSectionHandler1d( componentID, lsObsUrl );

        /* .2d Datei lesen und GML füllen */

        final String crs = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
        final MultiTriangleEater multiEater = new MultiTriangleEater();
        if( m_boolDoFullEvaluate )
        {
          for( final ResultType parameter : m_parameters )
          {
            /* GML(s) */
            if( parameter == ResultType.TERRAIN && !ResultMeta1d2dHelper.containsTerrain( stepResultMeta ) )
            {
              /* create TIN-Dir for FEM terrain model */
              // TODO: obscure, why go outside our output dir... TODO: refaktor it!
              final String calcUnitPath = resultOutputDir.getParent();

              final File modelPath = new File( calcUnitPath, "model" ); //$NON-NLS-1$
              if( !modelPath.exists() )
              {
                modelPath.mkdirs();

                final File modelTinPath = new File( modelPath, "Tin" ); //$NON-NLS-1$
                modelTinPath.mkdirs();

                final File tinResultFile = new File( modelTinPath, "tin.gz" ); //$NON-NLS-1$
                final ITriangleEater gmlTriangleEater = NodeResultHelper.createTinEater( tinResultFile, parameter, crs );
                multiEater.addEater( gmlTriangleEater );
              }
            }
            else
            {
              /* create TIN-Dir for results */
              final File tinPath = new File( resultOutputDir, "Tin" ); //$NON-NLS-1$
              tinPath.mkdirs();

              final File tinZipResultFile = new File( tinPath, "tin.gz" ); //$NON-NLS-1$
              final ITriangleEater tinEater = NodeResultHelper.createTinEater( tinZipResultFile, parameter, crs );
              multiEater.addEater( tinEater );
            }
          }
        }

        final NodeResultsHandler handler = new NodeResultsHandler( resultWorkspace, multiEater, m_flowModel, m_controlModel, m_discModel, resultMinMaxCatcher, lsHandler, m_mapResults );
        convTelemac.setModelElementHandler( handler );

        logger.takeInterimTime();
        logger.printCurrentInterim( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.54", m_inputFile.getName() ) ); //$NON-NLS-1$

        System.out.println( stepDate );
        int iStep = resolveStepNr( stepDate );
        convTelemac.feedHandler( iStep );

        logger.takeInterimTime();
        logger.printCurrentInterim( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.56" ) ); //$NON-NLS-1$

        if( m_boolDoFullEvaluate )
        {
          convTelemac.feedHandlerWithTriangles();
        }
        // finish MultiEater and engage serializer
        KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s", Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.58" ) ); //$NON-NLS-1$ //$NON-NLS-2$
        multiEater.finished();
        KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s", Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.60" ) ); //$NON-NLS-1$ //$NON-NLS-2$

        /* Node-GML in Datei schreiben */
        GmlSerializer.serializeWorkspace( gmlZipResultFile, resultWorkspace, "UTF-8" ); //$NON-NLS-1$UTF-8

        /* LengthSection in Datei schreiben */

        final ICalculationUnit1D[] calcUnits = lsHandler.getCalcUnits();

        for( final ICalculationUnit1D calcUnit : calcUnits )
        {
          final File lsObsFile = new File( resultOutputDir, "lengthSection_" + calcUnit.getId() + ".gml" ); //$NON-NLS-1$ //$NON-NLS-2$

          final IObservation<TupleResult> lsObs = lsHandler.getObservation( calcUnit );
          final GMLWorkspace lsObsWorkspace = lsHandler.getWorkspace( calcUnit );
          if( lsObs.getResult().size() > 0 )
          {
            ObservationFeatureFactory.toFeature( lsObs, lsObsWorkspace.getRootFeature() );
            GmlSerializer.serializeWorkspace( lsObsFile, lsObsWorkspace, "UTF-8" ); //$NON-NLS-1$

            /* length section entry in result db */
            // TODO: use station range for min max...
            ResultMeta1d2dHelper.addDocument( stepResultMeta, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.65" ) + calcUnit.getName(), Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.66" ), IDocumentResultMeta.DOCUMENTTYPE.lengthSection, new Path( lsObsFile.getName() ), Status.OK_STATUS, new BigDecimal( 0 ), new BigDecimal( 0 ) ); //$NON-NLS-1$ //$NON-NLS-2$
          }
        }

        logger.takeInterimTime();
        logger.printCurrentInterim( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.67" ) ); //$NON-NLS-1$

        BigDecimal min;
        BigDecimal max;

        if( m_boolDoFullEvaluate )
        {
          for( final ResultType parameter : m_parameters )
          {
            /* GML(s) */

            /* result db */

            switch( parameter )
            {
              case TERRAIN:
                if( stepResultMeta != null && !ResultMeta1d2dHelper.containsTerrain( stepResultMeta ) )
                {
                  /* check if there exists already an entry for terrainTin */
                  final ICalcUnitResultMeta calcUnitResult = (ICalcUnitResultMeta)stepResultMeta.getOwner();

                  min = new BigDecimal( resultMinMaxCatcher.getMinTerrain() ).setScale( 3, BigDecimal.ROUND_HALF_UP );
                  max = new BigDecimal( resultMinMaxCatcher.getMaxTerrain() ).setScale( 3, BigDecimal.ROUND_HALF_UP );
                  ResultMeta1d2dHelper.addDocument( calcUnitResult, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.68" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.69" ), IDocumentResultMeta.DOCUMENTTYPE.tinTerrain, new Path( "model/Tin/tin_TERRAIN.gz" ), Status.OK_STATUS, min, max ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                }

                break;

              case DEPTH:
                // TODO: Handle minimum at infinity
                min = new BigDecimal( resultMinMaxCatcher.getMinDepth() ).setScale( 3, BigDecimal.ROUND_HALF_UP );
                max = new BigDecimal( resultMinMaxCatcher.getMaxDepth() ).setScale( 3, BigDecimal.ROUND_HALF_UP );
                ResultMeta1d2dHelper.addDocument( stepResultMeta, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.71" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.1" ), IDocumentResultMeta.DOCUMENTTYPE.tinDepth, new Path( "Tin/tin_DEPTH.gz" ), Status.OK_STATUS, min, max ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                break;

              case VELOCITY:
                min = new BigDecimal( resultMinMaxCatcher.getMinVelocityAbs() ).setScale( 3, BigDecimal.ROUND_HALF_UP );
                max = new BigDecimal( resultMinMaxCatcher.getMaxVelocityAbs() ).setScale( 3, BigDecimal.ROUND_HALF_UP );
                ResultMeta1d2dHelper.addDocument( stepResultMeta, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.74" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.75" ), IDocumentResultMeta.DOCUMENTTYPE.tinVelo, new Path( "Tin/tin_VELOCITY.gz" ), Status.OK_STATUS, min, max ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                break;

              case VELOCITY_X:
                min = new BigDecimal( resultMinMaxCatcher.getMinVelocityAbs() ).setScale( 3, BigDecimal.ROUND_HALF_UP );
                max = new BigDecimal( resultMinMaxCatcher.getMaxVelocityAbs() ).setScale( 3, BigDecimal.ROUND_HALF_UP );
                ResultMeta1d2dHelper.addDocument( stepResultMeta, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.77" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.78" ), IDocumentResultMeta.DOCUMENTTYPE.tinVelo, new Path( "Tin/tin_VELOCITY_X.gz" ), Status.OK_STATUS, min, max ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                break;

              case VELOCITY_Y:
                min = new BigDecimal( resultMinMaxCatcher.getMinVelocityAbs() ).setScale( 3, BigDecimal.ROUND_HALF_UP );
                max = new BigDecimal( resultMinMaxCatcher.getMaxVelocityAbs() ).setScale( 3, BigDecimal.ROUND_HALF_UP );
                ResultMeta1d2dHelper.addDocument( stepResultMeta, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.80" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.81" ), IDocumentResultMeta.DOCUMENTTYPE.tinVelo, new Path( "Tin/tin_VELOCITY_Y.gz" ), Status.OK_STATUS, min, max ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

                break;

              case WATERLEVEL:
                min = new BigDecimal( resultMinMaxCatcher.getMinWaterlevel() ).setScale( 3, BigDecimal.ROUND_HALF_UP );
                max = new BigDecimal( resultMinMaxCatcher.getMaxWaterlevel() ).setScale( 3, BigDecimal.ROUND_HALF_UP );
                ResultMeta1d2dHelper.addDocument( stepResultMeta, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.83" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.84" ), IDocumentResultMeta.DOCUMENTTYPE.tinWsp, new Path( "Tin/tin_WATERLEVEL.gz" ), Status.OK_STATUS, min, max ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                break;

              case SHEARSTRESS:
                min = new BigDecimal( resultMinMaxCatcher.getMinShearStress() ).setScale( 3, BigDecimal.ROUND_HALF_UP );
                max = new BigDecimal( resultMinMaxCatcher.getMaxShearStress() ).setScale( 3, BigDecimal.ROUND_HALF_UP );
                ResultMeta1d2dHelper.addDocument( stepResultMeta, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.86" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.87" ), IDocumentResultMeta.DOCUMENTTYPE.tinShearStress, new Path( "Tin/tin_SHEARSTRESS.gz" ), Status.OK_STATUS, min, max ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                break;

              default:
                throw new UnsupportedOperationException();
            }
          }
        }

        // we will set all min max results to the node results meta also
        ResultMeta1d2dHelper.addDocument( stepResultMeta, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.89" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.90" ), IDocumentResultMeta.DOCUMENTTYPE.nodes, new Path( "results.gz" ), Status.OK_STATUS, resultMinMaxCatcher ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

        // TODO: maybe check if time and stepTime are equal?
        if( stepResultMeta != null )
          ResultMeta1d2dHelper.addToResultDB( stepResultMeta, stepDate, resultOutputDir );

//        return gmlZipResultFile;
      }
//      catch( CoreException | IOException | GmlSerializeException | GMLSchemaException e )
      catch( Exception e )
      {
        e.printStackTrace();
        final String msg = "error while reading telemac results";
        return new Status( IStatus.ERROR, PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), msg, e );
      }
      finally
      {
//      IOUtils.closeQuietly( is );

        logger.takeInterimTime();
        logger.printCurrentInterim( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.92" ) ); //$NON-NLS-1$

        runtime.gc();
      }
    }
    return Status.OK_STATUS;

  }

  private int resolveStepNr( Date stepDate )
  {
    if( m_mapStepToDates == null )
    {
      initStepToDates();
    }
    return m_mapStepToDates.get( stepDate );
  }

  private void initStepToDates( )
  {
    m_mapStepToDates = new HashMap<>();

    final TupleResult result = m_controlModel.getTimeSteps().getResult();
    final IComponent componentTime = ComponentUtilities.findComponentByID( result.getComponents(), Kalypso1D2DDictConstants.DICT_COMPONENT_TIME );
    int indexOfComponent = result.indexOfComponent( componentTime );
    for( int stepNr = 1; stepNr < result.size(); ++stepNr )
    {
      final XMLGregorianCalendar stepCal = (XMLGregorianCalendar)result.get( stepNr ).getValue( indexOfComponent );
      m_mapStepToDates.put( DateUtilities.toDate( stepCal ), stepNr - 1 );
    }
  }

  private IStatus zipInputFile( )
  {
    final InputStream contentStream = null;
    try
    {
      /* Zip .2d file to outputDir */
      if( !m_inputFile.getName().getBaseName().toLowerCase().endsWith( ".zip" ) ) //$NON-NLS-1$
      {
        final File outputZip2d = new File( m_outputDir, ResultMeta1d2dHelper.ORIGINAL_SLF_FILE_NAME + ".zip" ); //$NON-NLS-1$
        ZipUtilities.zip( outputZip2d, new File[] { new File( m_inputFile.getURL().toURI() ) }, new File( m_inputFile.getParent().getURL().toURI() ) );
      }
    }
    catch( IOException | URISyntaxException e )
    {
      e.printStackTrace();
      return new Status( IStatus.ERROR, PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), "could not zip file", e );
    }
    finally
    {
      IOUtils.closeQuietly( contentStream );
    }
    return Status.OK_STATUS;
  }

  private IStatus readActWaveRes( )
  {
    if( m_stepDate == ResultManager.STEADY_DATE || m_stepDate == ResultManager.MAXI_DATE || m_inputResFileSWAN == null )
      return Status.OK_STATUS;

    try
    {
      final FileObject lResFile = getOrUnzipResult( m_inputResFileSWAN, m_outputDir );

      KalypsoModel1D2DPlugin.getDefault().getLog().log( new Status( IStatus.INFO, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "ProcessResultsJob.0" ) + lResFile ) ); //$NON-NLS-1$

      // only read the *.mat files
      if( lResFile.getName().getFriendlyURI().endsWith( ISimulation1D2DConstants.SIM_SWAN_MAT_RESULT_EXT ) )
      {
        final SWANResultsReader lSWANResultsReader = new SWANResultsReader( lResFile );
        final String timeStringFormatedForSWANOutput = SWANDataConverterHelper.getTimeStringFormatedForSWANOutput( m_stepDate );
        m_mapResults = lSWANResultsReader.readMatResultsFile( timeStringFormatedForSWANOutput );
        KalypsoModel1D2DPlugin.getDefault().getLog().log( new Status( IStatus.INFO, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "ProcessResultsJob.1" ) + timeStringFormatedForSWANOutput ) ); //$NON-NLS-1$

        ResultMeta1d2dHelper.addDocument( m_unitResultMeta.addStepResult(), ResultMeta1d2dHelper.SWAN_RAW_DATA_META_NAME, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.13" ), IDocumentResultMeta.DOCUMENTTYPE.coreDataZip, new Path( "../" + ISimulation1D2DConstants.SIM_SWAN_TRIANGLE_FILE + ".zip" ), Status.OK_STATUS, null, null ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
      }
    }
    catch( final Throwable e )
    {
      e.printStackTrace();
      return new Status( IStatus.ERROR, PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), "could not zip file", e );
    }
    return Status.OK_STATUS;

  }

  public FileObject getTelemacBinaryResFile( )
  {
    return m_inputFile;
  }

  private FileObject getOrUnzipResult( final FileObject input, final File outputDir )
  {
    FileObject inputFile = input;
    FileObject outputFile = input;
    if( inputFile.getName().getFriendlyURI().endsWith( "zip" ) ) //$NON-NLS-1$
    {
      try
      {
        ZipUtilities.unzip( new File( inputFile.getURL().toURI() ), new File( outputDir.toURI() ) );
        List<String> listNames = ZipUtilities.getFilesNamesFromZip( inputFile.getURL() );
        if( listNames.size() > 1 || listNames.isEmpty() )
        {
          // should not be
          return null;
        }
        outputFile = m_vfsManager.resolveFile( outputDir, listNames.get( 0 ) ); //$NON-NLS-1$
      }
      catch( IOException | URISyntaxException e )
      {
        e.printStackTrace();
        return null;
      }
    }
    return outputFile;
  }

  public NodeResultMinMaxCatcher getMinMaxData( )
  {
    return resultMinMaxCatcher;
  }
}
