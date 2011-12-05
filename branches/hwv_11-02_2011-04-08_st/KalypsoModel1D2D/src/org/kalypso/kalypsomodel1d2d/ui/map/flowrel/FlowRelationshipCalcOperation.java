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
package org.kalypso.kalypsomodel1d2d.ui.map.flowrel;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.nio.charset.Charset;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.ObjectUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.FlowRelationUtilitites;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBuildingFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IFlowRelation1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.ITeschkeFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.dict.Kalypso1D2DDictConstants;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationship;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.ProfileFeatureFactory;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.tuhh.core.gml.PolynomeProperties;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypso.model.wspm.tuhh.schema.gml.QIntervallResult;
import org.kalypso.model.wspm.tuhh.schema.gml.QIntervallResultCollection;
import org.kalypso.model.wspm.tuhh.schema.schemata.IWspmTuhhQIntervallConstants;
import org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.observation.result.TupleResultUtilities;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.simulation.core.NullSimulationMonitor;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.simulation.core.util.DefaultSimulationDataProvider;
import org.kalypso.simulation.core.util.DefaultSimulationResultEater;
import org.kalypso.simulation.core.util.SimulationUtilitites;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeaturesChangedModellEvent;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.IFeatureProviderFactory;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPathException;

/**
 * @author Gernot Belger
 */
public class FlowRelationshipCalcOperation implements IAdaptable
{
  private final static class NullSimulationMonitorExtension extends NullSimulationMonitor
  {
    private final SubMonitor m_monitor;

    private int m_status;

    private String m_text;

    public NullSimulationMonitorExtension( final IProgressMonitor monitor )
    {
      m_monitor = SubMonitor.convert( monitor, 100 );
    }

    /**
     * @see org.kalypso.simulation.core.NullSimulationMonitor#isCanceled()
     */
    @Override
    public boolean isCanceled( )
    {
      return m_monitor.isCanceled();
    }

    /**
     * @see org.kalypso.simulation.core.NullSimulationMonitor#setProgress(int)
     */
    @Override
    public void setProgress( final int progress )
    {
      // TODO: implement
      super.setProgress( progress );
    }

    /**
     * @see org.kalypso.simulation.core.NullSimulationMonitor#setFinishInfo(int, java.lang.String)
     */
    @Override
    public void setFinishInfo( final int status, final String text )
    {
      m_status = status;
      m_text = text;
    }

    /**
     * @see org.kalypso.simulation.core.NullSimulationMonitor#getFinishStatus()
     */
    @Override
    public int getFinishStatus( )
    {
      return m_status;
    }

    /**
     * @see org.kalypso.simulation.core.NullSimulationMonitor#getFinishText()
     */
    @Override
    public String getFinishText( )
    {
      return m_text;
    }
  }

  private final IFEDiscretisationModel1d2d m_discModel;

  private final IFlowRelationshipModel m_flowRelModel;

  private boolean m_running;

  private final OutputStream m_outputStream;

  private QIntervallResult m_result;

  private final IFlowRelation1D m_flowRel;

  private final TuhhCalculation m_calculation;

  private IStatus m_status;

  private String m_consoleText;

  public FlowRelationshipCalcOperation( final TuhhCalculation calculation, final IFlowRelation1D flowRel, final IFlowRelationshipModel flowModel, final IFEDiscretisationModel1d2d discModel, final OutputStream outputStream )
  {
    m_calculation = calculation;
    m_flowRel = flowRel;
    m_flowRelModel = flowModel;
    m_discModel = discModel;
    m_outputStream = outputStream;
    m_status = StatusUtilities.createStatus( IStatus.INFO, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.FlowRelationshipCalcOperation.0" ) + m_flowRel.getName(), null ); //$NON-NLS-1$
  }

  public void execute( final IProgressMonitor monitor )
  {
    try
    {
      m_running = true;

      m_status = StatusUtilities.createStatus( IStatus.INFO, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.FlowRelationshipCalcOperation.1" ) + m_flowRel.getName(), null ); //$NON-NLS-1$
      m_result = calculateFlowRel( m_calculation, m_flowRel, monitor );
      m_status = StatusUtilities.createStatus( IStatus.OK, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.FlowRelationshipCalcOperation.2" ) + m_flowRel.getName(), null ); //$NON-NLS-1$
    }
    catch( final CoreException e )
    {
      m_status = StatusUtilities.createStatus( IStatus.ERROR, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.FlowRelationshipCalcOperation.3" ) + m_flowRel.getName(), e ); //$NON-NLS-1$
    }
    catch( final InvocationTargetException e )
    {
      m_status = StatusUtilities.createStatus( IStatus.ERROR, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.FlowRelationshipCalcOperation.4" ) + m_flowRel.getName(), e.getTargetException() ); //$NON-NLS-1$
    }
    finally
    {
      m_running = false;
    }
  }

  private QIntervallResult calculateFlowRel( final TuhhCalculation calculation, final IFlowRelation1D flowRel, final IProgressMonitor monitor ) throws CoreException, InvocationTargetException
  {
    if( flowRel instanceof ITeschkeFlowRelation )
    {
      final IProfileFeature teschkeProfile = ((ITeschkeFlowRelation) flowRel).getProfile();
      if( teschkeProfile == null )
        throw new CoreException( StatusUtilities.createStatus( IStatus.ERROR, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.FlowRelationshipCalcOperation.5" ) + teschkeProfile.getName(), null ) ); //$NON-NLS-1$

      return runCalculation( calculation, flowRel, new IProfil[] { teschkeProfile.getProfil() }, monitor );
    }
    else if( flowRel instanceof IBuildingFlowRelation )
    {
      // find element and nodes
      final IBuildingFlowRelation building = (IBuildingFlowRelation) flowRel;

      final IFE1D2DNode upstreamNode = FlowRelationUtilitites.findNeighborNode( building, m_discModel, true );
      final IFE1D2DNode downstreamNode = FlowRelationUtilitites.findNeighborNode( building, m_discModel, false );

      final IFlowRelationship upRel = m_flowRelModel.findFlowrelationship( upstreamNode.getPoint().getPosition(), 0.01 );
      final IFlowRelationship downRel = m_flowRelModel.findFlowrelationship( downstreamNode.getPoint().getPosition(), 0.01 );

      final ITeschkeFlowRelation tUp = (ITeschkeFlowRelation) upRel;
      final ITeschkeFlowRelation tDown = (ITeschkeFlowRelation) downRel;

      // find adjacent teschke parameters
      final IProfileFeature downProfile = tDown.getProfile();
      final IProfileFeature buildingProfile = building.getProfile();
      final IProfileFeature upProfile = tUp.getProfile();

      if( downProfile == null )
        throw new CoreException( StatusUtilities.createStatus( IStatus.ERROR, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.FlowRelationshipCalcOperation.6" ) + tDown.getName(), null ) ); //$NON-NLS-1$
      if( upProfile == null )
        throw new CoreException( StatusUtilities.createStatus( IStatus.ERROR, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.FlowRelationshipCalcOperation.7" ) + tUp.getName(), null ) ); //$NON-NLS-1$
      if( buildingProfile == null )
        throw new CoreException( StatusUtilities.createStatus( IStatus.ERROR, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.FlowRelationshipCalcOperation.8" ) + building.getName(), null ) ); //$NON-NLS-1$

      final IProfil downStreamProfil = downProfile.getProfil();
      final IProfil buildingProfil = buildingProfile.getProfil();
      final IProfil upStreamProfil = upProfile.getProfil();

      return runCalculation( calculation, flowRel, new IProfil[] { downStreamProfil, buildingProfil, upStreamProfil }, monitor );
    }

    throw new CoreException( StatusUtilities.createStatus( IStatus.ERROR, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.FlowRelationshipCalcOperation.9" ) + flowRel + Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.FlowRelationshipCalcOperation.10" ), null ) ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  private QIntervallResult runCalculation( final TuhhCalculation templateCalculation, final IFlowRelation1D flowRel, final IProfil[] profiles, final IProgressMonitor monitor ) throws InvocationTargetException
  {
    File tmpDir = null;
    try
    {
      tmpDir = SimulationUtilitites.createSimulationTmpDir( "" + System.currentTimeMillis() ); //$NON-NLS-1$

      final TuhhCalculation calculation = createCalculation( flowRel, templateCalculation, profiles );

      // Prepare wspm model
      final File modelFile = new File( tmpDir, "modell.gml" ); //$NON-NLS-1$
      final GMLWorkspace calcWorkspace = calculation.getWorkspace();
      GmlSerializer.serializeWorkspace( modelFile, calcWorkspace, Charset.defaultCharset().name() );

      // prepare calcjob
      final WspmTuhhCalcJob wspmTuhhCalcJob = new WspmTuhhCalcJob( new PrintStream( m_outputStream ) );
      final DefaultSimulationDataProvider inputProvider = new DefaultSimulationDataProvider();
      inputProvider.put( WspmTuhhCalcJob.INPUT_MODELL_GML, modelFile.toURI().toURL() );
      inputProvider.put( WspmTuhhCalcJob.INPUT_CALC_PATH, new GMLXPath( calculation ).toString() );
      // eps-thinning is big, as we do not need the tin result and bigger is faster
      inputProvider.put( WspmTuhhCalcJob.INPUT_EPS_THINNING, "100.0" ); //$NON-NLS-1$

      final DefaultSimulationResultEater resultEater = new DefaultSimulationResultEater();
      final NullSimulationMonitorExtension simMonitor = new NullSimulationMonitorExtension( monitor );

      wspmTuhhCalcJob.run( tmpDir, inputProvider, resultEater, simMonitor );

      if( simMonitor.getFinishStatus() != IStatus.OK )
        throw new CoreException( StatusUtilities.createStatus( simMonitor.getFinishStatus(), simMonitor.getFinishText(), null ) );

      // read simulation log
      final File logFile = (File) resultEater.getResult( WspmTuhhCalcJob.OUTPUT_SIMULATION_LOG );
      m_consoleText = FileUtils.readFileToString( logFile, Charset.defaultCharset().name() );

      // read interval results and remember them
      final File qintervallFile = (File) resultEater.getResult( WspmTuhhCalcJob.OUTPUT_QINTERVALL_RESULT );
      final GMLWorkspace qresultsWorkspace = GmlSerializer.createGMLWorkspace( qintervallFile, calcWorkspace.getFeatureProviderFactory() );
      final QIntervallResultCollection qResultCollection = new QIntervallResultCollection( qresultsWorkspace.getRootFeature() );

      final List< ? > resultList = qResultCollection.getQResultFeatures();
      for( final Object o : resultList )
      {
        final QIntervallResult qresult = new QIntervallResult( FeatureHelper.getFeature( qresultsWorkspace, o ) );
        // HACK: we set a scale here in order to get a right comparison with the station value that was read from the
        // profile. if a rounded station value occurs in the flow relation, the result of the comparison is always
        // false, because the station value of the flow relation gets rounded and the one of the profile gets not
        // rounded (read from string with fixed length).
        // TODO: implement the right setting of the station value for the flow relation with a fixed scale of 4!
        final BigDecimal station = flowRel.getStation().setScale( 4, BigDecimal.ROUND_HALF_UP );

        // REMARK: sometimes it could be, that the user wants to assign a profile to a new created flow relation. in
        // this case he is able to to this and to calculate the data, but the assignment will never happen, if the
        // station is not equal to the station of the assigned profile.
        if( ObjectUtils.equals( station, qresult.getStation() ) )
          return qresult;
      }

      throw new CoreException( StatusUtilities.createStatus( IStatus.ERROR, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.FlowRelationshipCalcOperation.14" ), null ) ); //$NON-NLS-1$
    }
    catch( final InvocationTargetException e )
    {
      throw e;
    }
    catch( final GMLSchemaException e )
    {
      throw new InvocationTargetException( e );
    }
    catch( final IOException e )
    {
      throw new InvocationTargetException( e );
    }
    catch( final GmlSerializeException e )
    {
      throw new InvocationTargetException( e );
    }
    catch( final SimulationException e )
    {
      throw new InvocationTargetException( e );
    }
    catch( final GMLXPathException e )
    {
      throw new InvocationTargetException( e );
    }
    catch( final Exception e )
    {
      throw new InvocationTargetException( e );
    }
    finally
    {
      SimulationUtilitites.clearTmpDir( tmpDir );
    }
  }

  public boolean isRunning( )
  {
    return m_running;
  }

  private TuhhCalculation createCalculation( final IFlowRelation1D flowRel, final TuhhCalculation template, final IProfil[] profiles ) throws GMLSchemaException, InvocationTargetException
  {
    final IFeatureProviderFactory factory = flowRel.getFeature().getWorkspace().getFeatureProviderFactory();

    // Create empty project with one calculation and one reach
    final TuhhWspmProject project = TuhhWspmProject.create( null, factory );
    final TuhhCalculation calculation = project.createReibConstCalculation();
    final boolean direction = true; // TODO: depends on building stuff
    final WspmWaterBody waterBody = project.createWaterBody( "someWater", direction ); //$NON-NLS-1$
    project.createNewReach( waterBody.getName(), direction );
    final TuhhReach reach = TuhhWspmProject.createNewReachForWaterBody( waterBody );
    reach.setName( "someReach" ); //$NON-NLS-1$

    // Add Profiles to reach
    double minStation = Double.MAX_VALUE;
    double maxStation = -Double.MAX_VALUE;
    for( final IProfil profil : profiles )
    {
      final double station = profil.getStation();

      final IProfileFeature profile = waterBody.createNewProfile();
      ProfileFeatureFactory.toFeature( profil, profile );
      reach.createProfileSegment( profile, station );

      minStation = Math.min( minStation, station );
      maxStation = Math.max( maxStation, station );
    }

    // Initialize calculation
    calculation.setCalcCreation( "fe1d2d", new Date() ); //$NON-NLS-1$
    calculation.setDescription( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.FlowRelationshipCalcOperation.18" ) ); //$NON-NLS-1$
    calculation.setName( "1dparameters" ); //$NON-NLS-1$
    calculation.setReachRef( reach );
    calculation.setSubReachDef( minStation - 1, maxStation + 1 );

    /* Copy control parameters from template */
    calculation.setVersion( template.getVersion() );
    final Double minQ = template.getMinQ();
    final Double maxQ = template.getMaxQ();
    final Double step = template.getQStep();
    if( minQ == null || maxQ == null || step == null )
      throw new InvocationTargetException( new CoreException( StatusUtilities.createStatus( IStatus.ERROR, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.FlowRelationshipCalcOperation.20" ), null ) ) ); //$NON-NLS-1$

    calculation.setQRange( minQ, maxQ, step );
    final boolean calcBuildings;
    if( flowRel instanceof ITeschkeFlowRelation )
      calcBuildings = false;
    else
      calcBuildings = true;

    calculation.setWaterlevelParameters( template.getIterationType(), template.getVerzoegerungsverlust(), template.getReibungsverlust(), calcBuildings, calcBuildings, template.isUseExtremeRoughness() );
    calculation.setStartSlope( template.getStartSlope() );

    final PolynomeProperties polynomeProperties = calculation.getPolynomeProperties();
    final PolynomeProperties templateProperties = template.getPolynomeProperties();
    polynomeProperties.setAlphaLimit( templateProperties.getAlphaLimit() );
    polynomeProperties.setAlphaSlope( templateProperties.getAlphaSlope() );
    polynomeProperties.setAreaSlope( templateProperties.getAreaSlope() );
    polynomeProperties.setRunoffSlope( templateProperties.getRunoffSlope() );
    polynomeProperties.setDeegree( templateProperties.getDeegree() );
    polynomeProperties.setIgnoreOutlier( templateProperties.getIgnoreOutlier() );
    polynomeProperties.setTripleForAll( templateProperties.getTripleForAll() );
    polynomeProperties.setTripleMode( templateProperties.getTripleMode() );
    polynomeProperties.setWeightSplinePoint( templateProperties.getWeightSplinePoint() );

    return calculation;
  }

  public static void copyTeschkeData( final ITeschkeFlowRelation flowRel, final QIntervallResult qresult ) throws Exception
  {
    final Feature feature = flowRel.getFeature();
    final Feature flowRelParentFeature = feature;
    final GMLWorkspace flowRelworkspace = flowRelParentFeature.getWorkspace();
    final IFeatureType flowRelFT = flowRelworkspace.getGMLSchema().getFeatureType( ITeschkeFlowRelation.QNAME );
    final IRelationType flowRelObsRelation = (IRelationType) flowRelFT.getProperty( ITeschkeFlowRelation.QNAME_PROP_POINTSOBSERVATION );
    final IRelationType flowRelPolynomeRelation = (IRelationType) flowRelFT.getProperty( ITeschkeFlowRelation.QNAME_PROP_POLYNOMES );

    /* clone observation */
    final Feature pointObsFeature = (Feature) qresult.getFeature().getProperty( QIntervallResult.QNAME_P_QIntervallResult_pointsMember );
    if( pointObsFeature != null )
      FeatureHelper.cloneFeature( feature, flowRelObsRelation, pointObsFeature );

    /* clone polynomials */
    flowRel.getPolynomials().clear(); // clear polynomials in case, the relation already existed
    final List< ? > polynomeFeatures = qresult.getPolynomialFeatures();
    for( final Object object : polynomeFeatures )
    {
      final GMLWorkspace qresultsWorkspace = qresult.getFeature().getWorkspace();
      final Feature polynomeFeature = FeatureHelper.getFeature( qresultsWorkspace, object );
      if( polynomeFeature != null )
      {
        FeatureHelper.cloneFeature( feature, flowRelPolynomeRelation, polynomeFeature );
      }
    }

    flowRelworkspace.fireModellEvent( new FeaturesChangedModellEvent( flowRelworkspace, new Feature[] { feature } ) );
  }

  public static void copyBuildingData( final IBuildingFlowRelation buildingRelation, final QIntervallResult qresult )
  {
    /* copy building parameter from one observation to the other */
    final Feature buildingFeature = buildingRelation.getFeature();
    final GMLWorkspace buildingWorkspace = buildingFeature.getWorkspace();
    final IObservation<TupleResult> buildingObservation = buildingRelation.getBuildingObservation();
    final IObservation<TupleResult> qresultBuildingObs = qresult.getBuildingObservation( false );
    final TupleResult qresultResult = qresultBuildingObs.getResult();
    final TupleResult buildingResult = buildingObservation.getResult();
    buildingResult.clear(); // Clear in case if relation already existed
    final Map<String, String> componentMap = new HashMap<String, String>();
    componentMap.put( IWspmTuhhQIntervallConstants.DICT_COMPONENT_RUNOFF, Kalypso1D2DDictConstants.DICT_COMPONENT_DISCHARGE );
    componentMap.put( IWspmTuhhQIntervallConstants.DICT_COMPONENT_WATERLEVEL_DOWNSTREAM, Kalypso1D2DDictConstants.DICT_COMPONENT_WATERLEVEL_DOWNSTREAM );
    componentMap.put( IWspmTuhhQIntervallConstants.DICT_COMPONENT_WATERLEVEL_UPSTREAM, Kalypso1D2DDictConstants.DICT_COMPONENT_WATERLEVEL_UPSTREAM );
    TupleResultUtilities.copyValues( qresultResult, buildingResult, componentMap );
    buildingRelation.setBuildingObservation( buildingObservation );

    buildingWorkspace.fireModellEvent( new FeaturesChangedModellEvent( buildingWorkspace, new Feature[] { buildingFeature } ) );
  }

  public IStatus getStatus( )
  {
    return m_status;
  }

  public void applyResult( ) throws Exception
  {
    m_flowRel.setCalculation( m_calculation );

    if( m_status == null || m_result == null || !m_status.isOK() )
      return;

    if( m_flowRel instanceof ITeschkeFlowRelation )
      FlowRelationshipCalcOperation.copyTeschkeData( (ITeschkeFlowRelation) m_flowRel, m_result );
    else
      FlowRelationshipCalcOperation.copyBuildingData( (IBuildingFlowRelation) m_flowRel, m_result );
  }

  /**
   * @see org.eclipse.core.runtime.IAdaptable#getAdapter(java.lang.Class)
   */
  @Override
  public Object getAdapter( @SuppressWarnings("rawtypes") final Class adapter )
  {
    if( adapter == IStatus.class )
      return m_status;

    return null;
  }

  public IFlowRelation1D getFlowRelation1D( )
  {
    return m_flowRel;
  }

  public void setConsoleText( final String consoleText )
  {
    m_consoleText = consoleText;
  }

  public String getConsoleText( )
  {
    return m_consoleText;
  }

  public QIntervallResult getResult( )
  {
    return m_result;
  }
}
