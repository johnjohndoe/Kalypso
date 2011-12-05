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
import java.io.InputStream;
import java.math.BigDecimal;
import java.net.URL;
import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.zip.ZipOutputStream;

import javax.xml.namespace.QName;

import org.apache.commons.io.IOUtils;
import org.apache.commons.vfs.FileObject;
import org.apache.commons.vfs.FileType;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.kalypso.commons.io.VFSUtilities;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.commons.performance.TimeLogger;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DDebug;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.conv.RMA10S2GmlConv;
import org.kalypso.kalypsomodel1d2d.conv.SWANDataConverterHelper;
import org.kalypso.kalypsomodel1d2d.conv.results.ITriangleEater;
import org.kalypso.kalypsomodel1d2d.conv.results.MultiTriangleEater;
import org.kalypso.kalypsomodel1d2d.conv.results.NodeResultsHandler;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultType;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultType.TYPE;
import org.kalypso.kalypsomodel1d2d.conv.results.SWANResultsReader;
import org.kalypso.kalypsomodel1d2d.conv.results.TinResultWriter.QNameAndString;
import org.kalypso.kalypsomodel1d2d.conv.results.TriangulatedSurfaceDirectTriangleEater;
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
import org.kalypso.kalypsomodel1d2d.sim.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.model.wspm.schema.IWspmDictionaryConstants;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

/**
 * TODO: remove processing of the map This job processed one 2d-result file. *
 * 
 * @author Gernot Belger
 */
public class ProcessResultsJob extends Job
{
  private File m_outputDir;

  private FileObject m_inputFile;

  private NodeResultMinMaxCatcher m_resultMinMaxCatcher = new NodeResultMinMaxCatcher();

  private List<TYPE> m_parameters;

  private IStepResultMeta m_stepResultMeta;

  private IFlowRelationshipModel m_flowModel;

  private IControlModel1D2D m_controlModel;

  private IFEDiscretisationModel1d2d m_discModel;

  private Date m_stepDate;

  private FileObject m_inputResFileSWAN;

  private boolean m_boolDoFullEvaluate = false;

  private Map<String, Map<GM_Position, Double>> m_mapResults;

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
  public ProcessResultsJob( final FileObject file, final File outputDir, final IFlowRelationshipModel flowModel, final IControlModel1D2D controlModel, final IFEDiscretisationModel1d2d discModel, final List<ResultType.TYPE> parameter, final Date stepDate, final ICalcUnitResultMeta unitResultMeta )
  {
    super( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.0" ) + file.getName() ); //$NON-NLS-1$
    init( file, null, outputDir, flowModel, controlModel, discModel, parameter, stepDate, unitResultMeta, true );
  }

  private void init( final FileObject file, final FileObject fileResSWAN, final File outputDir, final IFlowRelationshipModel flowModel, final IControlModel1D2D controlModel, final IFEDiscretisationModel1d2d discModel, final List<TYPE> parameter, final Date stepDate, final ICalcUnitResultMeta unitResultMeta, final boolean boolDoFullEvaluate )
  {
    //    super( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.0" ) + file.getName() ); //$NON-NLS-1$
    KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s", Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.2" ) ); //$NON-NLS-1$ //$NON-NLS-2$

    m_inputFile = file;
    m_outputDir = outputDir;
    m_flowModel = flowModel;
    m_controlModel = controlModel;
    m_discModel = discModel;
    m_parameters = parameter;
    m_stepDate = stepDate;
    m_inputResFileSWAN = fileResSWAN;

    // m_mapSWANResults = mapResults;
    m_boolDoFullEvaluate = boolDoFullEvaluate;

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
      if( fileResSWAN != null )
      {
        m_parameters.add( ResultType.TYPE.WAVEHSIG );
        m_parameters.add( ResultType.TYPE.WAVEDIR );
        m_parameters.add( ResultType.TYPE.WAVEPER );
      }
    }

  }

  public ProcessResultsJob( final FileObject file, final FileObject fileResSWAN, final File outputDir, final IFlowRelationshipModel flowModel, final IControlModel1D2D controlModel, final IFEDiscretisationModel1d2d discModel, final List<TYPE> parameter, final Date stepDate, final ICalcUnitResultMeta unitResultMeta, final boolean doFullEvaluate )
  {
    super( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.0" ) + file.getName() ); //$NON-NLS-1$
    init( file, fileResSWAN, outputDir, flowModel, controlModel, discModel, parameter, stepDate, unitResultMeta, doFullEvaluate );
  }

  /**
   * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public IStatus run( final IProgressMonitor monitor )
  {
    KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.4" ) ); //$NON-NLS-1$

    final String timeStepName;
    if( m_stepDate == ResultManager.STEADY_DATE )
      timeStepName = Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.5" ); //$NON-NLS-1$
    else if( m_stepDate == ResultManager.MAXI_DATE )
      timeStepName = Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.6" ); //$NON-NLS-1$
    else
      timeStepName = Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.7", m_stepDate ); //$NON-NLS-1$

    monitor.beginTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.8" ) + timeStepName, 10 ); //$NON-NLS-1$
    monitor.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.9" ) + timeStepName ); //$NON-NLS-1$

    // TODO: simultaneous parsing and zipping caused problems
    // the last few lines of the 2d-file were just ignored
    try
    {
      InputStream contentStream = null;

      ZipOutputStream zos = null;
      try
      {
        /* Zip .2d file to outputDir */
        if( !m_inputFile.getName().getBaseName().toLowerCase().endsWith( ".2d.zip" ) ) //$NON-NLS-1$
        {
          final File outputZip2d = new File( m_outputDir, ResultMeta1d2dHelper.ORIGINAL_2D_FILE_NAME + ".zip" ); //$NON-NLS-1$
          ZipUtilities.zip( outputZip2d, new File[] { new File( m_inputFile.getURL().toURI() ) }, new File( m_inputFile.getParent().getURL().toURI() ) );
        }
      }
      finally
      {
        IOUtils.closeQuietly( zos );
        IOUtils.closeQuietly( contentStream );
      }
      ProgressUtilities.worked( monitor, 1 );

      try
      {
        /* Read into NodeResults */
        contentStream = FileUtilities.getInputStreamFromFileObject( m_inputFile );

        readActSWANRes();
        read2DIntoGmlResults( contentStream );
      }
      finally
      {
        IOUtils.closeQuietly( contentStream );
        m_inputFile.close();
      }

      ResultMeta1d2dHelper.addDocument( m_stepResultMeta, ResultMeta1d2dHelper.RMA_RAW_DATA_META_NAME, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.12" ), IDocumentResultMeta.DOCUMENTTYPE.coreDataZip, new Path( ResultMeta1d2dHelper.ORIGINAL_2D_FILE_NAME + ".zip" ), Status.OK_STATUS, null, null ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
      if( m_mapResults != null )
      {
        ResultMeta1d2dHelper.addDocument( m_stepResultMeta, ResultMeta1d2dHelper.SWAN_RAW_DATA_META_NAME, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.13" ), IDocumentResultMeta.DOCUMENTTYPE.coreDataZip, new Path( "../" + ISimulation1D2DConstants.SIM_SWAN_TRIANGLE_FILE + ".zip" ), Status.OK_STATUS, null, null ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
      }

      ProgressUtilities.worked( monitor, 1 );
    }
    catch( final Throwable e )
    {
      final String msg = Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.14" ); //$NON-NLS-1$
      return new Status( IStatus.ERROR, PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), msg, e );
    }

    return Status.OK_STATUS;
  }

  private void readActSWANRes( )
  {
    m_mapResults = null;
    SWANResultsReader lSWANResultsReader = null;
    if( m_stepDate != ResultManager.STEADY_DATE && m_stepDate != ResultManager.MAXI_DATE && m_inputResFileSWAN != null )
    {
      try
      {
        FileObject lResFile = m_inputResFileSWAN;
        if( m_inputResFileSWAN.getName().getFriendlyURI().endsWith( "zip" ) ) { //$NON-NLS-1$
          ZipUtilities.unzip( new File( m_inputResFileSWAN.getURL().toURI() ), new File( m_outputDir.toURI() ) );
          lResFile = VFSUtilities.getNewManager().resolveFile( m_outputDir, ISimulation1D2DConstants.SIM_SWAN_TRIANGLE_FILE + "." + ISimulation1D2DConstants.SIM_SWAN_MAT_RESULT_EXT );
        }
        // check if the given object is the directory with swan results.
        else if( m_inputResFileSWAN.getType().equals( FileType.FOLDER ) )
        {
          lResFile = m_inputResFileSWAN.getChild( ISimulation1D2DConstants.SIM_SWAN_TRIANGLE_FILE + "." + ISimulation1D2DConstants.SIM_SWAN_MAT_RESULT_EXT ); //$NON-NLS-1$
        }
        KalypsoModel1D2DPlugin.getDefault().getLog().log( StatusUtilities.createInfoStatus( "Reading SWAN Result file: " + lResFile ) );
        // only read the *.mat files
        if( lResFile.getName().getFriendlyURI().endsWith( ISimulation1D2DConstants.SIM_SWAN_MAT_RESULT_EXT ) )
        {
          lSWANResultsReader = new SWANResultsReader( lResFile );
          String timeStringFormatedForSWANOutput = SWANDataConverterHelper.getTimeStringFormatedForSWANOutput( m_stepDate );
          m_mapResults = lSWANResultsReader.readMatResultsFile( timeStringFormatedForSWANOutput );
          KalypsoModel1D2DPlugin.getDefault().getLog().log( StatusUtilities.createInfoStatus( "read data for: " + timeStringFormatedForSWANOutput ) );
        }
      }
      catch( final Throwable e )
      {
        e.printStackTrace();
      }
    }
  }

  public File read2DIntoGmlResults( final InputStream is ) throws Exception
  {
    KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s", Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.16" ) ); //$NON-NLS-1$ //$NON-NLS-2$

    final Runtime runtime = Runtime.getRuntime();
    runtime.gc();

    final TimeLogger logger = new TimeLogger( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.17" ) ); //$NON-NLS-1$

    final File gmlZipResultFile = new File( m_outputDir, "results.gz" ); //$NON-NLS-1$

    try
    {
      /* GMLWorkspace für Ergebnisse anlegen */
      final GMLWorkspace resultWorkspace = FeatureFactory.createGMLWorkspace( INodeResultCollection.QNAME, gmlZipResultFile.toURI().toURL(), null );
      final URL lsObsUrl = LengthSectionHandler2d.class.getResource( "resources/lengthSectionTemplate.gml" ); //$NON-NLS-1$

      final String componentID = IWspmDictionaryConstants.LS_COMPONENT_STATION;
      final LengthSectionHandler1d lsHandler = new LengthSectionHandler1d( componentID, lsObsUrl );

      /* .2d Datei lesen und GML füllen */
      final RMA10S2GmlConv conv = new RMA10S2GmlConv();

      final String crs = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
      final MultiTriangleEater multiEater = new MultiTriangleEater();
      if( m_boolDoFullEvaluate )
      {
        for( final ResultType.TYPE parameter : m_parameters )
        {
          /* GML(s) */
          if( parameter == ResultType.TYPE.TERRAIN )
          {
            /* create TIN-Dir for FEM terrain model */
            // TODO: obscure, why go outside our output dir... TODO: refaktor it!
            final String calcUnitPath = m_outputDir.getParent();

            final File modelPath = new File( calcUnitPath, "model" ); //$NON-NLS-1$
            if( !modelPath.exists() )
            {
              modelPath.mkdirs();

              final File modelTinPath = new File( modelPath, "Tin" ); //$NON-NLS-1$
              modelTinPath.mkdirs();

              final File tinResultFile = new File( modelTinPath, "tin.gz" ); //$NON-NLS-1$
              final ITriangleEater gmlTriangleEater = createTinEater( tinResultFile, parameter, crs );
              multiEater.addEater( gmlTriangleEater );
            }
          }
          else
          {
            /* create TIN-Dir for results */
            final File tinPath = new File( m_outputDir, "Tin" ); //$NON-NLS-1$
            tinPath.mkdirs();

            final File tinZipResultFile = new File( tinPath, "tin.gz" ); //$NON-NLS-1$
            final ITriangleEater tinEater = createTinEater( tinZipResultFile, parameter, crs );
            multiEater.addEater( tinEater );
          }
        }
      }

      final NodeResultsHandler handler = new NodeResultsHandler( resultWorkspace, multiEater, m_flowModel, m_controlModel, m_discModel, m_resultMinMaxCatcher, lsHandler, m_mapResults );
      conv.setRMA10SModelElementHandler( handler );

      logger.takeInterimTime();
      logger.printCurrentInterim( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.54", m_inputFile.getName() ) ); //$NON-NLS-1$

      conv.parse( is );

      logger.takeInterimTime();
      logger.printCurrentInterim( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.56" ) ); //$NON-NLS-1$

      // finish MultiEater and engage serializer
      KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s", Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.58" ) ); //$NON-NLS-1$ //$NON-NLS-2$
      multiEater.finished();
      KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s", Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.60" ) ); //$NON-NLS-1$ //$NON-NLS-2$

      /* Node-GML in Datei schreiben */
      GmlSerializer.serializeWorkspace( gmlZipResultFile, resultWorkspace, "UTF-8" ); //$NON-NLS-1$

      /* LengthSection in Datei schreiben */

      final ICalculationUnit1D[] calcUnits = lsHandler.getCalcUnits();

      for( final ICalculationUnit1D calcUnit : calcUnits )
      {
        final File lsObsFile = new File( m_outputDir, "lengthSection_" + calcUnit.getGmlID() + ".gml" ); //$NON-NLS-1$ //$NON-NLS-2$

        final IObservation<TupleResult> lsObs = lsHandler.getObservation( calcUnit );
        final GMLWorkspace lsObsWorkspace = lsHandler.getWorkspace( calcUnit );
        if( lsObs.getResult().size() > 0 )
        {
          ObservationFeatureFactory.toFeature( lsObs, lsObsWorkspace.getRootFeature() );
          GmlSerializer.serializeWorkspace( lsObsFile, lsObsWorkspace, "UTF-8" ); //$NON-NLS-1$

          /* length section entry in result db */
          // TODO: use station range for min max...
          ResultMeta1d2dHelper.addDocument( m_stepResultMeta, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.65" ) + calcUnit.getName(), Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.66" ), IDocumentResultMeta.DOCUMENTTYPE.lengthSection, new Path( lsObsFile.getName() ), Status.OK_STATUS, new BigDecimal( 0 ), new BigDecimal( 0 ) ); //$NON-NLS-1$ //$NON-NLS-2$
        }

      }

      logger.takeInterimTime();
      logger.printCurrentInterim( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.67" ) ); //$NON-NLS-1$

      BigDecimal min;
      BigDecimal max;

      if( m_boolDoFullEvaluate )
      {
        for( final ResultType.TYPE parameter : m_parameters )
        {
          /* GML(s) */

          /* result db */

          switch( parameter )
          {
            case TERRAIN:
              if( m_stepResultMeta != null )
              {
                /* check if there exists already an entry for terrainTin */
                final ICalcUnitResultMeta calcUnitResult = (ICalcUnitResultMeta) m_stepResultMeta.getParent();

                min = new BigDecimal( m_resultMinMaxCatcher.getMinTerrain() ).setScale( 3, BigDecimal.ROUND_HALF_UP );
                max = new BigDecimal( m_resultMinMaxCatcher.getMaxTerrain() ).setScale( 3, BigDecimal.ROUND_HALF_UP );
                ResultMeta1d2dHelper.addDocument( calcUnitResult, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.68" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.69" ), IDocumentResultMeta.DOCUMENTTYPE.tinTerrain, new Path( "model/Tin/tin_TERRAIN.gz" ), Status.OK_STATUS, min, max ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
              }

              break;

            case DEPTH:
              // TODO: Handle minimum at infinity
              min = new BigDecimal( m_resultMinMaxCatcher.getMinDepth() ).setScale( 3, BigDecimal.ROUND_HALF_UP );
              max = new BigDecimal( m_resultMinMaxCatcher.getMaxDepth() ).setScale( 3, BigDecimal.ROUND_HALF_UP );
              ResultMeta1d2dHelper.addDocument( m_stepResultMeta, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.71" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.1" ), IDocumentResultMeta.DOCUMENTTYPE.tinDepth, new Path( "Tin/tin_DEPTH.gz" ), Status.OK_STATUS, min, max ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
              break;

            case VELOCITY:
              min = new BigDecimal( m_resultMinMaxCatcher.getMinVelocityAbs() ).setScale( 3, BigDecimal.ROUND_HALF_UP );
              max = new BigDecimal( m_resultMinMaxCatcher.getMaxVelocityAbs() ).setScale( 3, BigDecimal.ROUND_HALF_UP );
              ResultMeta1d2dHelper.addDocument( m_stepResultMeta, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.74" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.75" ), IDocumentResultMeta.DOCUMENTTYPE.tinVelo, new Path( "Tin/tin_VELOCITY.gz" ), Status.OK_STATUS, min, max ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
              break;

            case VELOCITY_X:
              min = new BigDecimal( m_resultMinMaxCatcher.getMinVelocityAbs() ).setScale( 3, BigDecimal.ROUND_HALF_UP );
              max = new BigDecimal( m_resultMinMaxCatcher.getMaxVelocityAbs() ).setScale( 3, BigDecimal.ROUND_HALF_UP );
              ResultMeta1d2dHelper.addDocument( m_stepResultMeta, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.77" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.78" ), IDocumentResultMeta.DOCUMENTTYPE.tinVelo, new Path( "Tin/tin_VELOCITY_X.gz" ), Status.OK_STATUS, min, max ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
              break;

            case VELOCITY_Y:
              min = new BigDecimal( m_resultMinMaxCatcher.getMinVelocityAbs() ).setScale( 3, BigDecimal.ROUND_HALF_UP );
              max = new BigDecimal( m_resultMinMaxCatcher.getMaxVelocityAbs() ).setScale( 3, BigDecimal.ROUND_HALF_UP );
              ResultMeta1d2dHelper.addDocument( m_stepResultMeta, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.80" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.81" ), IDocumentResultMeta.DOCUMENTTYPE.tinVelo, new Path( "Tin/tin_VELOCITY_Y.gz" ), Status.OK_STATUS, min, max ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

              break;

            case WATERLEVEL:
              min = new BigDecimal( m_resultMinMaxCatcher.getMinWaterlevel() ).setScale( 3, BigDecimal.ROUND_HALF_UP );
              max = new BigDecimal( m_resultMinMaxCatcher.getMaxWaterlevel() ).setScale( 3, BigDecimal.ROUND_HALF_UP );
              ResultMeta1d2dHelper.addDocument( m_stepResultMeta, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.83" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.84" ), IDocumentResultMeta.DOCUMENTTYPE.tinWsp, new Path( "Tin/tin_WATERLEVEL.gz" ), Status.OK_STATUS, min, max ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
              break;

            case SHEARSTRESS:
              min = new BigDecimal( m_resultMinMaxCatcher.getMinShearStress() ).setScale( 3, BigDecimal.ROUND_HALF_UP );
              max = new BigDecimal( m_resultMinMaxCatcher.getMaxShearStress() ).setScale( 3, BigDecimal.ROUND_HALF_UP );
              ResultMeta1d2dHelper.addDocument( m_stepResultMeta, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.86" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.87" ), IDocumentResultMeta.DOCUMENTTYPE.tinShearStress, new Path( "Tin/tin_SHEARSTRESS.gz" ), Status.OK_STATUS, min, max ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
              break;

            default:
              throw new UnsupportedOperationException();
          }
        }
      }
      // min = new BigDecimal( m_resultMinMaxCatcher.getMinVelocityAbs() ).setScale( 3, BigDecimal.ROUND_HALF_UP );
      // max = new BigDecimal( m_resultMinMaxCatcher.getMaxVelocityAbs() ).setScale( 3, BigDecimal.ROUND_HALF_UP );
      //      ResultMeta1d2dHelper.addDocument( m_stepResultMeta, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.89" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.90" ), IDocumentResultMeta.DOCUMENTTYPE.nodes, new Path( "results.gz" ), Status.OK_STATUS, min, max ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
      // we will set all min max results to the node results meta also
      ResultMeta1d2dHelper.addDocument( m_stepResultMeta, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.89" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.90" ), IDocumentResultMeta.DOCUMENTTYPE.nodes, new Path( "results.gz" ), Status.OK_STATUS, m_resultMinMaxCatcher ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

      /* HMO(s) */
      /*
       * try { final File resultHMOFile = new File( "D:/Projekte/kalypso_dev/post-processing/output.hmo" ); final
       * HMOTriangleEater hmoTriangleEater = new HMOTriangleEater( resultHMOFile, parameter ); multiEater.addEater(
       * hmoTriangleEater ); } catch (Exception e) { e.printStackTrace(); }
       */

      // TODO: maybe check if time and stepTime are equal?
      if( m_stepResultMeta != null )
        addToResultDB( m_stepResultMeta, m_stepDate, m_outputDir );

      return gmlZipResultFile;
    }
    finally
    {
      IOUtils.closeQuietly( is );

      logger.takeInterimTime();
      logger.printCurrentInterim( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.92" ) ); //$NON-NLS-1$

      runtime.gc();
    }
  }

  private ITriangleEater createTinEater( final File tinResultFile, final TYPE parameter, final String crs ) throws CoreException
  // , MalformedURLException, GM_Exception
  {
    // TODO: for debug purpose only...
    // final boolean fast = true;

    final List<QNameAndString> properties = new ArrayList<QNameAndString>();

    switch( parameter )
    {
      case DEPTH:
        properties.add( new QNameAndString( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "unit" ), "m" ) ); //$NON-NLS-1$ //$NON-NLS-2$
        properties.add( new QNameAndString( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "parameter" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.33" ) ) ); //$NON-NLS-1$ //$NON-NLS-2$
        break;

      case VELOCITY:
        properties.add( new QNameAndString( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "unit" ), "m/s" ) ); //$NON-NLS-1$ //$NON-NLS-2$
        properties.add( new QNameAndString( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "parameter" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.37" ) ) ); //$NON-NLS-1$ //$NON-NLS-2$
        break;

      case VELOCITY_X:
        properties.add( new QNameAndString( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "unit" ), "m/s" ) ); //$NON-NLS-1$ //$NON-NLS-2$
        properties.add( new QNameAndString( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "parameter" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.41" ) ) ); //$NON-NLS-1$ //$NON-NLS-2$
        break;

      case VELOCITY_Y:
        properties.add( new QNameAndString( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "unit" ), "m/s" ) ); //$NON-NLS-1$ //$NON-NLS-2$
        properties.add( new QNameAndString( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "parameter" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.45" ) ) ); //$NON-NLS-1$ //$NON-NLS-2$
        break;

      case WATERLEVEL:
        properties.add( new QNameAndString( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "unit" ), "m�NN" ) ); //$NON-NLS-1$ //$NON-NLS-2$
        properties.add( new QNameAndString( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "parameter" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.49" ) ) ); //$NON-NLS-1$ //$NON-NLS-2$
        break;

      case SHEARSTRESS:
        properties.add( new QNameAndString( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "unit" ), "N/m�" ) ); //$NON-NLS-1$ //$NON-NLS-2$
        properties.add( new QNameAndString( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "parameter" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.53" ) ) ); //$NON-NLS-1$ //$NON-NLS-2$
        break;

      case DIFFERENCE:
        properties.add( new QNameAndString( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "unit" ), "-" ) ); //$NON-NLS-1$ //$NON-NLS-2$
        properties.add( new QNameAndString( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "parameter" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.111" ) ) ); //$NON-NLS-1$ //$NON-NLS-2$
        break;

      case TERRAIN:
        properties.add( new QNameAndString( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "unit" ), "m" ) ); //$NON-NLS-1$ //$NON-NLS-2$
        properties.add( new QNameAndString( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "parameter" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.112" ) ) ); //$NON-NLS-1$ //$NON-NLS-2$
        break;
      case WAVEDIR:
        break;
      case WAVEHSIG:
        break;
      case WAVEPER:
        break;
    }

    final QNameAndString[] props = properties.toArray( new QNameAndString[properties.size()] );
    // if( fast )
    return new TriangulatedSurfaceDirectTriangleEater( tinResultFile, parameter, crs, props );

    /*
     * 
     * this part of code was already "dead code", the fast variable is set constant to true
     * 
     * GMLWorkspace triangleWorkspace = null; try { triangleWorkspace = FeatureFactory.createGMLWorkspace( new QName(
     * UrlCatalog1D2D.MODEL_1D2DResults_NS, "TinResult" ), tinResultFile.toURI().toURL(), null ); } catch(
     * GMLSchemaException e ) { e.printStackTrace(); } final GM_TriangulatedSurface surface =
     * org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_TriangulatedSurface( crs ); final Feature
     * triangleFeature = triangleWorkspace.getRootFeature(); triangleFeature.setProperty( new QName(
     * UrlCatalog1D2D.MODEL_1D2DResults_NS, "triangulatedSurfaceMember" ), surface ); //$NON-NLS-1$ return new
     * TriangulatedSurfaceTriangleEater( tinResultFile, triangleWorkspace, surface, parameter, props );
     */

  }

  // /**
  // * Checks, if there exists already an entry for terrainTin
  // */
  // private boolean existsTerrain( final ICalcUnitResultMeta calcUnitResult )
  // {
  // final IFeatureWrapperCollection<IResultMeta> children = calcUnitResult.getChildren();
  // for( final IResultMeta resultMeta : children )
  // {
  // if( resultMeta instanceof IDocumentResultMeta )
  // {
  // final IDocumentResultMeta document = (IDocumentResultMeta) resultMeta;
  // if( document.getDocumentType() == IDocumentResultMeta.DOCUMENTTYPE.tinTerrain )
  // return true;
  // }
  // }
  //
  // return false;
  // }

  private static void addToResultDB( final IStepResultMeta stepResultMeta, final Date stepDate, final File outputDir )
  {
    // TODO: retrieve time zone from central plugin preferences
    final DateFormat dateFormatter = DateFormat.getDateTimeInstance( DateFormat.SHORT, DateFormat.LONG );
    dateFormatter.setTimeZone( KalypsoCorePlugin.getDefault().getTimeZone() );

    if( ResultManager.STEADY_DATE.equals( stepDate ) )
    {
      stepResultMeta.setName( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.93" ) ); //$NON-NLS-1$
      stepResultMeta.setDescription( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.94" ) ); //$NON-NLS-1$
      stepResultMeta.setStepType( IStepResultMeta.STEPTYPE.steady );
      stepResultMeta.setStepTime( null );
    }
    else if( ResultManager.MAXI_DATE.equals( stepDate ) )
    {
      stepResultMeta.setName( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.95" ) ); //$NON-NLS-1$
      stepResultMeta.setDescription( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.96" ) ); //$NON-NLS-1$
      stepResultMeta.setStepType( IStepResultMeta.STEPTYPE.maximum );
      stepResultMeta.setStepTime( null );
    }
    else
    {
      // TODO: check for right time zone
      final String dateString = dateFormatter.format( stepDate );
      stepResultMeta.setName( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.97", dateString ) ); //$NON-NLS-1$
      stepResultMeta.setDescription( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob.98" ) + dateString ); //$NON-NLS-1$
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
