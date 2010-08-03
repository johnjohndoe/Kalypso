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
package org.kalypso.model.hydrology.internal.preprocessing;

import java.io.File;
import java.io.IOException;
import java.util.Calendar;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Logger;

import javax.xml.datatype.XMLGregorianCalendar;

import org.apache.commons.io.FileUtils;
import org.eclipse.core.runtime.OperationCanceledException;
import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.convert.namodel.NAConfiguration;
import org.kalypso.convert.namodel.NAControlConverter;
import org.kalypso.convert.namodel.NAModellConverter;
import org.kalypso.convert.namodel.NaNodeResultProvider;
import org.kalypso.convert.namodel.NaSimulationData;
import org.kalypso.convert.namodel.manager.HydroHash;
import org.kalypso.convert.namodel.manager.IDManager;
import org.kalypso.convert.namodel.manager.LzsimManager;
import org.kalypso.gmlschema.annotation.IAnnotation;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.internal.NaAsciiDirs;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.visitors.TransformVisitor;

/**
 * Converts KalypsoHydrology gml files to Kalypso-NA ascii files.
 * 
 * @author Gernot Belger
 */
public class NAModelPreprocessor
{
  private static String[] CATCHMENT_FACTOR_PARAMETER_TARGET = { "retob", "retint", "aigw" }; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

  private final static String[][] CATCHMENT_FACTORS_PARAMETER = { new String[] { "retob", "faktorRetobRetint" }, new String[] { "retint", "faktorRetobRetint" }, new String[] { "aigw", "faktorAigw" } }; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$

  private File m_preprocessedAsciiDir;

  private final IDManager m_idManager;

  private Date m_simulationStart = null;

  private final NaSimulationData m_simulationData;

  private final NAConfiguration m_conf;

  private final Logger m_logger;

  private final NaAsciiDirs m_asciiDirs;

  public NAModelPreprocessor( final NAConfiguration conf, final NaAsciiDirs asciiDirs, final IDManager idManager, final NaSimulationData simulationData, final Logger logger )
  {
    m_conf = conf;
    m_asciiDirs = asciiDirs;
    m_idManager = idManager;
    m_simulationData = simulationData;
    m_logger = logger;
  }

  /**
   * Sets the directory of preprocessed ASCII files.<br>
   * Optional: must be called before {@link #process(ISimulationMonitor)}.<br/>
   * If this directory is set, ASCII files in this directory will be used instead of recreating them from gml.<br/>
   * Original comment:<br>
   * While optimization, you can recycle files from a former run. implement here to copy the files to your tmp dir and
   * while generating files you should check if files already exist, and on your option do not generate them.<br/>
   * WARNING: never use result files or files that vary during optimization.<br/>
   */
  public void setPreprocessedFilesDir( final File preprocessedAsciiDir )
  {
    m_preprocessedAsciiDir = preprocessedAsciiDir;
  }

  public void process( final ISimulationMonitor monitor ) throws NAPreprocessorException, OperationCanceledException
  {
    monitor.setMessage( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.15" ) ); //$NON-NLS-1$
    checkCancel( monitor );

    copyPreprocessedDirs();
    checkCancel( monitor );

    monitor.setMessage( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.23" ) ); //$NON-NLS-1$
    generateAscii();
    checkCancel( monitor );

    writeStartCondition();
    checkCancel( monitor );

    // Create "out_we.nat", else Kalypso-NA will not run
    m_asciiDirs.outWeNatDir.mkdirs();
  }

  private void checkCancel( final ISimulationMonitor monitor )
  {
    if( monitor.isCanceled() )
      throw new OperationCanceledException();
  }

  /* During optimization, use previously processed files to improve performance */
  // TODO: check if we can replace this by a more general caching mechanism based on the file-date of the input gml
  // file.
  private void copyPreprocessedDirs( ) throws NAPreprocessorException
  {
    try
    {
      if( m_preprocessedAsciiDir == null )
        return;

      final NaAsciiDirs inputAsciiDirs = new NaAsciiDirs( m_preprocessedAsciiDir );

      if( inputAsciiDirs.klimaDatDir.exists() )
        FileUtils.copyDirectory( inputAsciiDirs.klimaDatDir, m_asciiDirs.asciiDir );

      if( inputAsciiDirs.zuflussDir.exists() )
        FileUtils.copyDirectory( inputAsciiDirs.zuflussDir, m_asciiDirs.asciiDir );

      if( inputAsciiDirs.hydroTopDir.exists() )
        FileUtils.copyDirectory( inputAsciiDirs.hydroTopDir, m_asciiDirs.asciiDir );
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      throw new NAPreprocessorException( "Failed to copy preprocessed ascii files", e );
    }
  }

  private void writeStartCondition( ) throws NAPreprocessorException
  {
    final GMLWorkspace lzsimWorkspace = m_simulationData.getLzsimWorkspace();
    if( lzsimWorkspace == null )
    {
      final String msg = Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.26", m_simulationStart ); //$NON-NLS-1$
      m_logger.info( msg );
      return;
    }

    try
    {
      final HydroHash hydroHash = m_conf.getHydroHash();
      LzsimManager.writeLzsimFiles( m_idManager, hydroHash, m_asciiDirs.lzsimDir, lzsimWorkspace );
    }
    catch( final Exception e )
    {
      throw new NAPreprocessorException( "Failed to write start condition", e );
    }
  }

// TODO: split generateASCII (the real one) into several smaller blocks
  private void generateAscii( ) throws NAPreprocessorException
  {
    try
    {
      generateASCII();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new NAPreprocessorException( "Failed to generate Kalypso-NA ASCII files", e );
    }
  }

  private void generateASCII( ) throws Exception
  {
    final GMLWorkspace metaWorkspace = m_simulationData.getMetaWorkspace();
    final GMLWorkspace controlWorkspace = m_simulationData.getControlWorkspace();
    final GMLWorkspace modelWorkspace = m_simulationData.getModelWorkspace();
    final GMLWorkspace hydrotopWorkspace = m_simulationData.getHydrotopWorkspace();
    final GMLWorkspace sudsWorkspace = m_simulationData.getSudsWorkspace();
    final GMLWorkspace parameterWorkspace = m_simulationData.getParameterWorkspace();
    final GMLWorkspace synthNWorkspace = m_simulationData.getSynthNWorkspace();

    final Feature metaFE = metaWorkspace.getRootFeature();

    final NaNodeResultProvider nodeResultProvider = new NaNodeResultProvider( modelWorkspace, controlWorkspace, m_conf.getZMLContext() );
    m_conf.setNodeResultProvider( nodeResultProvider );
    updateModelWithExtraVChannel( modelWorkspace, nodeResultProvider );

    // Grrr... this does not belong here... move it where the workspaces are loaded
    final Feature[] hydroFES = hydrotopWorkspace.getFeatures( hydrotopWorkspace.getGMLSchema().getFeatureType( NaModelConstants.HYDRO_ELEMENT_FT ) );
    String targetCS = null;
    for( int i = 0; i < hydroFES.length && targetCS == null; i++ )
    {
      // FIXME: performance: is it really necessary to check ALL geometries here???
      final GM_Object geom = (GM_Object) hydroFES[i].getProperty( NaModelConstants.HYDRO_PROP_GEOM );
      if( geom != null && geom.getCoordinateSystem() != null )
        targetCS = geom.getCoordinateSystem();
    }
    if( targetCS != null )
    {
      final TransformVisitor visitor = new TransformVisitor( targetCS );
      modelWorkspace.accept( visitor, "/", FeatureVisitor.DEPTH_INFINITE ); //$NON-NLS-1$
    }

    // setting duration of simulation...
    // start
    m_simulationStart = DateUtilities.toDate( (XMLGregorianCalendar) metaFE.getProperty( NaModelConstants.CONTROL_STARTSIMULATION ) );
    m_conf.setSimulationStart( m_simulationStart );
    // start forecast

    final Date startForecastDate = DateUtilities.toDate( (XMLGregorianCalendar) metaFE.getProperty( NaModelConstants.CONTROL_FORECAST ) );
    m_conf.setSimulationForecasetStart( startForecastDate );
    // end of simulation
    int hoursForecast = 0; // default length of forecast hours
    final Integer hoursOfForecast = (Integer) metaFE.getProperty( NaModelConstants.CONTROL_HOURS_FORECAST_PROP );
    if( hoursOfForecast != null )
      hoursForecast = hoursOfForecast.intValue();
    final Calendar c = Calendar.getInstance();
    c.setTime( startForecastDate );
    c.add( Calendar.HOUR, hoursForecast );
    final Date endDate = c.getTime();
    m_conf.setSimulationEnd( endDate );

    // calculate timestep
    final Integer minutesOfTimeStep = (Integer) metaFE.getProperty( NaModelConstants.CONTROL_MINUTES_TIMESTEP_PROP );
    final int minutesTimeStep;
    if( minutesOfTimeStep != null && minutesOfTimeStep.intValue() != 0 )
      minutesTimeStep = minutesOfTimeStep.intValue();
    else
      minutesTimeStep = 60;

    m_conf.setMinutesOfTimeStep( minutesTimeStep );

    // choose precipitation form and parameters
    final Boolean pns = (Boolean) metaFE.getProperty( NaModelConstants.CONTROL_PNS_PROP );
    m_conf.setUsePrecipitationForm( pns == null ? false : pns );
    if( m_conf.isUsePrecipitationForm() )
    {
      // the GUI asks for return period [a] - the fortran kernal needs annuality [1/a]
      m_conf.setAnnuality( 1d / (Double) metaFE.getProperty( NaModelConstants.CONTROL_XJAH_PROP ) );
      final Double durationMinutes = (Double) metaFE.getProperty( NaModelConstants.CONTROL_XWAHL2_PROP );
      final Double durationHours = durationMinutes / 60d;
      m_conf.setDuration( durationHours );
      m_conf.setForm( (String) metaFE.getProperty( NaModelConstants.CONTROL_IPVER_PROP ) );
    }

    // set rootnode
    m_conf.setRootNodeID( (String) controlWorkspace.getRootFeature().getProperty( NaModelConstants.NACONTROL_ROOTNODE_PROP ) );

    // generate modell files
    // FIXME: remove these setters/getters from m_conf
    m_conf.setModelWorkspace( modelWorkspace );
    m_conf.setParameterWorkspace( parameterWorkspace );
    m_conf.setHydrotopeWorkspace( hydrotopWorkspace );
    m_conf.setSynthNWorkspace( synthNWorkspace );
    m_conf.setSudsWorkspace( sudsWorkspace );

    // generate control files
    NAControlConverter.featureToASCII( m_conf, m_asciiDirs.startDir, controlWorkspace, modelWorkspace );

    // update model with factor values from control
    updateFactorParameter( modelWorkspace );

    final NAModellConverter main = new NAModellConverter( m_conf, m_logger );
    main.write();
  }

  /**
   * update workspace and do some tricks in order to fix some things the fortran-kernel can not handle for now
   * 
   * @param workspace
   * @throws Exception
   */
  private void updateModelWithExtraVChannel( final GMLWorkspace workspace, final NaNodeResultProvider nodeResultProvider ) throws Exception
  {
    updateGWNet( workspace );
    updateNode2NodeNet( workspace );
    updateZuflussNet( workspace );
    updateResultAsZuflussNet( workspace, nodeResultProvider );
  }

  /**
   * updates workspace, so that interflow and channelflow dependencies gets optimized <br>
   * groundwater flow can now run in opposite direction to channel flow
   * 
   * @param workspace
   */
  private void updateGWNet( final GMLWorkspace workspace )
  {
    final IFeatureType catchmentFT = workspace.getGMLSchema().getFeatureType( NaModelConstants.CATCHMENT_ELEMENT_FT );
    final IFeatureType vChannelFT = workspace.getGMLSchema().getFeatureType( NaModelConstants.V_CHANNEL_ELEMENT_FT );
    final Feature[] features = workspace.getFeatures( catchmentFT );
    final IRelationType entwaesserungsStrangMemberRT = (IRelationType) catchmentFT.getProperty( NaModelConstants.LINK_CATCHMENT_CHANNEL );
    for( final Feature catchmentFE : features )
    {
      final Feature orgChannelFE = workspace.resolveLink( catchmentFE, entwaesserungsStrangMemberRT );
      if( orgChannelFE == null )
        continue;
      final IRelationType downStreamNodeMemberRT = (IRelationType) vChannelFT.getProperty( NaModelConstants.LINK_CHANNEL_DOWNSTREAMNODE );
      final Feature nodeFE = workspace.resolveLink( orgChannelFE, downStreamNodeMemberRT );
      final Feature newChannelFE = workspace.createFeature( catchmentFE, entwaesserungsStrangMemberRT, vChannelFT );
      // set new relation: catchment -> new V-channel
      try
      {
        workspace.setFeatureAsComposition( catchmentFE, entwaesserungsStrangMemberRT, newChannelFE, true );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }
      // set new relation: new V-channel -> downstream node
      try
      {
        final IRelationType downStreamNodeMemberRT2 = (IRelationType) newChannelFE.getFeatureType().getProperty( NaModelConstants.LINK_CHANNEL_DOWNSTREAMNODE );
        workspace.addFeatureAsAggregation( newChannelFE, downStreamNodeMemberRT2, 1, nodeFE.getId() );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }
    }
  }

  /**
   * before: <code>
   * 
   *     o(existing)
   * 
   * </code> after: <code>
   * 
   *  |new Channel3|
   *     |
   *     V
   *     o(new Node2)  (return value)
   *     |
   *     V
   *  |new Channel1|
   *     |
   *     V
   *     o(existing)
   * 
   * </code>
   */
  private Feature buildVChannelNet( final GMLWorkspace workspace, final Feature existingNode ) throws Exception
  {
    final IFeatureType nodeColFT = workspace.getGMLSchema().getFeatureType( NaModelConstants.NODE_COLLECTION_FT );
    final IFeatureType nodeFT = workspace.getGMLSchema().getFeatureType( NaModelConstants.NODE_ELEMENT_FT );
    final IRelationType nodeMemberRT = (IRelationType) nodeColFT.getProperty( NaModelConstants.NODE_MEMBER_PROP );
    final IFeatureType vChannelFT = workspace.getGMLSchema().getFeatureType( NaModelConstants.V_CHANNEL_ELEMENT_FT );

    final IFeatureType channelColFT = workspace.getGMLSchema().getFeatureType( NaModelConstants.NA_CHANNEL_COLLECTION_FT );
    final IRelationType channelMemberRT = (IRelationType) channelColFT.getProperty( NaModelConstants.CHANNEL_MEMBER_PROP );
    final Feature channelColFE = workspace.getFeatures( channelColFT )[0];
    final Feature nodeColFE = workspace.getFeatures( workspace.getGMLSchema().getFeatureType( NaModelConstants.NODE_COLLECTION_FT ) )[0];

    // add to collections:
    final Feature newChannelFE1 = workspace.createFeature( channelColFE, channelMemberRT, vChannelFT );
    workspace.addFeatureAsComposition( channelColFE, channelMemberRT, 0, newChannelFE1 );
    final Feature newChannelFE3 = workspace.createFeature( channelColFE, channelMemberRT, vChannelFT );
    workspace.addFeatureAsComposition( channelColFE, channelMemberRT, 0, newChannelFE3 );
    final Feature newNodeFE2 = workspace.createFeature( nodeColFE, nodeMemberRT, nodeFT );
    workspace.addFeatureAsComposition( nodeColFE, nodeMemberRT, 0, newNodeFE2 );
    final IRelationType downStreamNodeMemberRT = (IRelationType) vChannelFT.getProperty( NaModelConstants.LINK_CHANNEL_DOWNSTREAMNODE );

    // 3 -> 2
    workspace.setFeatureAsAggregation( newChannelFE3, downStreamNodeMemberRT, newNodeFE2.getId(), true );
    // 2 -> 1
    final IRelationType downStreamChannelMemberRT = (IRelationType) nodeFT.getProperty( NaModelConstants.LINK_NODE_DOWNSTREAMCHANNEL );
    workspace.setFeatureAsAggregation( newNodeFE2, downStreamChannelMemberRT, newChannelFE1.getId(), true );
    // 1 -> existing

    // final IRelationType downStreamNodeMemberRT1 = (IRelationType) vChannelFT.getProperty( "downStreamNodeMember" );
    workspace.setFeatureAsAggregation( newChannelFE1, downStreamNodeMemberRT, existingNode.getId(), true );
    return newNodeFE2;
  }

  private void update( final Feature[] features )
  {
    for( final Feature feature : features )
    {
      for( int _p = 0; _p < CATCHMENT_FACTOR_PARAMETER_TARGET.length; _p++ ) // iterate parameters
      {
        final String[] factors = CATCHMENT_FACTORS_PARAMETER[_p];
        double value = 1.0; // initial value
        for( final String element : factors )
          // iterate factors
          value *= FeatureHelper.getAsDouble( feature, element, 1.0 );
        // set parameter
        final String targetPropName = CATCHMENT_FACTOR_PARAMETER_TARGET[_p];
        // FeatureProperty valueProp = FeatureFactory.createFeatureProperty( targetPropName, new Double( value ) );
        feature.setProperty( targetPropName, new Double( value ) );
      }
    }
  }

  /**
   * before: <br>
   * <code>
   * 
   * Node1 O <---  O Node2
   * 
   * </code> after: <br>
   * <code>
   * 
   * Node1 O <--- newVChannel <-- newNode O <-- newVChannel
   *                                      A
   *                                      |
   *                                      O-- Node2
   * 
   * </code>
   * 
   * @param workspace
   * @throws Exception
   */
  private void updateNode2NodeNet( final GMLWorkspace workspace ) throws Exception
  {
    final IFeatureType kontEntnahmeFT = workspace.getGMLSchema().getFeatureType( NaModelConstants.NODE_VERZW_ENTNAHME );
    final IFeatureType ueberlaufFT = workspace.getGMLSchema().getFeatureType( NaModelConstants.NODE_VERZW_UEBERLAUF );
    final IFeatureType verzweigungFT = workspace.getGMLSchema().getFeatureType( NaModelConstants.NODE_VERZW_VERZWEIGUNG );
    final IFeatureType nodeFT = workspace.getGMLSchema().getFeatureType( NaModelConstants.NODE_ELEMENT_FT );
    final Feature[] features = workspace.getFeatures( nodeFT );
    final IRelationType branchingMemberRT = (IRelationType) nodeFT.getProperty( NaModelConstants.NODE_BRANCHING_MEMBER_PROP );
    for( final Feature nodeFE : features )
    {
      final Feature branchingFE = workspace.resolveLink( nodeFE, branchingMemberRT );
      if( branchingFE != null )
      {
        final IFeatureType branchFT = branchingFE.getFeatureType();
        final IRelationType branchingNodeMemberRT = (IRelationType) branchFT.getProperty( NaModelConstants.NODE_BRANCHING_NODE_MEMBER_PROP );
        if( branchFT == kontEntnahmeFT || branchFT == ueberlaufFT || branchFT == verzweigungFT )
        {
          final Feature targetNodeFE = workspace.resolveLink( branchingFE, branchingNodeMemberRT );
          if( targetNodeFE == null )
          {
            final String relationLabel = branchingNodeMemberRT.getAnnotation().getLabel();
            final String branchingFElabel = FeatureHelper.getAnnotationValue( branchingFE, IAnnotation.ANNO_LABEL );
            final String message = String.format( "'%s' not set for '%s' in Node '%s'", relationLabel, branchingFElabel, nodeFE.getName() );
            throw new SimulationException( message );
          }

          final Feature newNodeFE = buildVChannelNet( workspace, targetNodeFE );
          workspace.setFeatureAsComposition( branchingFE, branchingNodeMemberRT, newNodeFE, true );
        }
      }
    }
  }

  /**
   * TODO scetch<br>
   * if results exists (from a former simulation) for a node, use this results as input, later the upstream nodes will
   * be ignored for calculation
   * 
   * @param workspace
   * @throws Exception
   */
  private void updateResultAsZuflussNet( final GMLWorkspace workspace, final NaNodeResultProvider nodeResultprovider ) throws Exception
  {
    final IFeatureType nodeFT = workspace.getGMLSchema().getFeatureType( NaModelConstants.NODE_ELEMENT_FT );
    final IFeatureType abstractChannelFT = workspace.getGMLSchema().getFeatureType( NaModelConstants.CHANNEL_ABSTRACT_FT );
    final Feature[] features = workspace.getFeatures( nodeFT );
    for( final Feature nodeFE : features )
    {
      if( nodeResultprovider.resultExists( nodeFE ) )
      {
        final Object resultValue = nodeFE.getProperty( NaModelConstants.NODE_RESULT_TIMESERIESLINK_PROP );
        // disconnect everything upstream (channel -> node)
        final IRelationType downStreamNodeMemberRT = (IRelationType) abstractChannelFT.getProperty( NaModelConstants.LINK_CHANNEL_DOWNSTREAMNODE );
        final Feature[] channelFEs = workspace.resolveWhoLinksTo( nodeFE, abstractChannelFT, downStreamNodeMemberRT );
        for( final Feature element : channelFEs )
        {
          final Feature newEndNodeFE = workspace.createFeature( element, downStreamNodeMemberRT, nodeFT );
          workspace.setFeatureAsComposition( element, downStreamNodeMemberRT, newEndNodeFE, true );
        }
        // add as zufluss
        final Feature newNodeFE = buildVChannelNet( workspace, nodeFE );
        // final FeatureProperty zuflussProp = FeatureFactory.createFeatureProperty( "zuflussZR", resultValue );
        newNodeFE.setProperty( NaModelConstants.NODE_ZUFLUSS_ZR_PROP, resultValue );
        newNodeFE.setProperty( NaModelConstants.NODE_SYNTHETIC_ZUFLUSS_ZR_PROP, nodeFE.getProperty( NaModelConstants.NODE_SYNTHETIC_ZUFLUSS_ZR_PROP ) );
      }
    }
  }

  /**
   * put one more VChannel to each Q-source, so that this discharge will appear in the result of the connected node <br>
   * zml inflow <br>
   * before: <br>
   * <code>
   * |Channel| <- o(1) <- input.zml <br>
   * </code><br>
   * now: <br>
   * <code>
   * |Channel| <- o(1) <- |VChannel (new)| <- o(new) <- |VChannel (new)|
   *                                          A- input.zml <br>
   * </code> constant inflow <br>
   * before: <br>
   * <code>
   * |Channel| <- o(1) <- Q(constant) <br>
   * </code><br>
   * now: <br>
   * <code>
   * |Channel| <- o(1) <- |VChannel (new)| <- o(new) <- |VChannel (new)|
   *                                          A- Q(constant)<br>
   * </code>
   * 
   * @param workspace
   * @throws Exception
   */
  private void updateZuflussNet( final GMLWorkspace workspace ) throws Exception
  {

    final IFeatureType kontZuflussFT = workspace.getGMLSchema().getFeatureType( NaModelConstants.NODE_VERZW_ZUFLUSS );
    final IFeatureType nodeFT = workspace.getGMLSchema().getFeatureType( NaModelConstants.NODE_ELEMENT_FT );
    final Feature[] features = workspace.getFeatures( nodeFT );
    final IRelationType branchingMemberRT = (IRelationType) nodeFT.getProperty( NaModelConstants.NODE_BRANCHING_MEMBER_PROP );
    for( final Feature nodeFE : features )
    {
      final Object zuflussValue = nodeFE.getProperty( NaModelConstants.NODE_ZUFLUSS_ZR_PROP );
      if( zuflussValue != null )
      {
        // update zufluss
        final Feature newNode = buildVChannelNet( workspace, nodeFE );
        // nove zufluss-property to new node
        // nodeFE.setProperty( FeatureFactory.createFeatureProperty( "zuflussZR", null ) );
        // newNode.setProperty( FeatureFactory.createFeatureProperty( "zuflussZR", zuflussValue ) );
        nodeFE.setProperty( NaModelConstants.NODE_ZUFLUSS_ZR_PROP, null );
        newNode.setProperty( NaModelConstants.NODE_ZUFLUSS_ZR_PROP, zuflussValue );
        newNode.setProperty( NaModelConstants.NODE_SYNTHETIC_ZUFLUSS_ZR_PROP, nodeFE.getProperty( NaModelConstants.NODE_SYNTHETIC_ZUFLUSS_ZR_PROP ) );
      }
      final Feature branchingFE = workspace.resolveLink( nodeFE, branchingMemberRT );
      if( branchingFE != null && branchingFE.getFeatureType() == kontZuflussFT )
      {
        // update zufluss
        final Feature newNode = buildVChannelNet( workspace, nodeFE );
        // nove constant-inflow to new node
        workspace.setFeatureAsComposition( nodeFE, branchingMemberRT, null, true );
        workspace.setFeatureAsComposition( newNode, branchingMemberRT, branchingFE, true );
      }
    }
  }

  /**
   * some parameter have factors that must be processed before generating asciifiles, as these factors do not occur in
   * ascci-format
   * 
   * @param modellWorkspace
   */
  private void updateFactorParameter( final GMLWorkspace modellWorkspace )
  {
    // Catchments
    final Feature[] catchmentFEs = modellWorkspace.getFeatures( modellWorkspace.getGMLSchema().getFeatureType( NaModelConstants.CATCHMENT_ELEMENT_FT ) );
    update( catchmentFEs );

    // KMChannels
    final Feature[] kmChanneFEs = modellWorkspace.getFeatures( modellWorkspace.getGMLSchema().getFeatureType( NaModelConstants.KM_CHANNEL_ELEMENT_FT ) );
    for( final Feature feature : kmChanneFEs )
    {
      final double rkfFactor = FeatureHelper.getAsDouble( feature, NaModelConstants.KM_CHANNEL_FAKTOR_RKF_PROP, 1.0 );
      final double rnfFactor = FeatureHelper.getAsDouble( feature, NaModelConstants.KM_CHANNEL_FAKTOR_RNF_PROP, 1.0 );
      final List< ? > kmParameter = (List< ? >) feature.getProperty( NaModelConstants.KM_CHANNEL_PARAMETER_MEMBER );
      final Iterator< ? > iterator = kmParameter.iterator();
      while( iterator.hasNext() )
      {
        final Feature kmParameterFE = (Feature) iterator.next();
        // rnf
        final double _rnf = rnfFactor * FeatureHelper.getAsDouble( kmParameterFE, NaModelConstants.KM_CHANNEL_RNF_PROP, 1.0 );
        // FeatureProperty rnfProp = FeatureFactory.createFeatureProperty( "rnf", new Double( _rnf ) );
        kmParameterFE.setProperty( NaModelConstants.KM_CHANNEL_RNF_PROP, new Double( _rnf ) );
        // rkf
        final double _rkf = rkfFactor * FeatureHelper.getAsDouble( kmParameterFE, NaModelConstants.KM_CHANNEL_RKF_PROP, 1.0 );
        // FeatureProperty rkfProp = FeatureFactory.createFeatureProperty( "rkf", new Double( _rkf ) );
        kmParameterFE.setProperty( NaModelConstants.KM_CHANNEL_RKF_PROP, new Double( _rkf ) );
      }
    }
  }

}
