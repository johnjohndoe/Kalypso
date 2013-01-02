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
package org.kalypso.kalypsomodel1d2d.imports;

import java.io.File;
import java.math.BigDecimal;
import java.net.URL;
import java.util.SortedMap;
import java.util.TreeMap;

import javax.vecmath.Vector2d;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.contribs.eclipse.core.resources.FolderUtilities;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper;
import org.kalypso.kalypsomodel1d2d.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta.DOCUMENTTYPE;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IStepResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResultCollection;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.visitors.ProfileVisitors;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.schema.gml.ProfileCacherFeaturePropertyFunction;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhStationComparator;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * Imports the results of a wspm water level calculation as stationary result into the result database of 1d2d.<br/>
 * This result can then be used to pseudo-restart a 1d-instationary calculation.
 * 
 * @author Gernot Belger
 */
public class Restart1DImporter
{
  private final IScenarioResultMeta m_resultMeta;

  private final IFolder m_scenarioFolder;

  public Restart1DImporter( final IScenarioResultMeta resultMeta, final IFolder scenarioFolder )
  {
    m_resultMeta = resultMeta;
    m_scenarioFolder = scenarioFolder;
  }

  public void doImport( final IFile lengthSectionFile, final String lsComponentStation, final String lsComponentWaterlevel, final String lsComponentVelocity, final String lsComponentKennung, final IProgressMonitor monitor ) throws CoreException
  {
    final SubMonitor progress = SubMonitor.convert( monitor, Messages.getString( "org.kalypso.kalypsomodel1d2d.imports.Restart1DImporter.0" ), 100 ); //$NON-NLS-1$

    try
    {
      final String projectName = lengthSectionFile.getProject().getName();
      final String calcName = lengthSectionFile.getParent().getParent().getParent().getName();

      /* Read length section */
      final IObservation<TupleResult> lengthSectionObs = readLengthSection( lengthSectionFile );
      final TuhhCalculation wspmCalculation = readWspmProject( lengthSectionFile.getProject(), calcName );

      /* Create meta data structure if not existent */
      final ICalcUnitResultMeta calcUnitResult = createCalcUnitResult( m_resultMeta, projectName );
      final IStepResultMeta stepResultMeta = createStepResult( calcUnitResult, calcName );
      final IDocumentResultMeta vectorDocument = createDocument( projectName, calcName, stepResultMeta );

      /* create the real folder, if necessary */
      final IPath fullPath = stepResultMeta.getFullPath();
      final IFolder stepFolder = m_scenarioFolder.getFolder( fullPath );
      FolderUtilities.mkdirs( stepFolder );

      /* Now really create the results.gml */
      final GMLWorkspace resultWorkspace = FeatureFactory.createGMLWorkspace( INodeResultCollection.QNAME, null, null );
      final INodeResultCollection nodeResults = (INodeResultCollection)resultWorkspace.getRootFeature().getAdapter( INodeResultCollection.class );

      final SortedMap<BigDecimal, GM_Point> profilesByStation = indexProfiles( wspmCalculation.getReach() );
      createNodeResults( vectorDocument, nodeResults, lengthSectionObs, lsComponentStation, lsComponentWaterlevel, lsComponentVelocity, lsComponentKennung, profilesByStation );

      final IFile resultFile = stepFolder.getFile( "results.gml" ); //$NON-NLS-1$
      final File resultJavaFile = resultFile.getLocation().toFile();
      GmlSerializer.serializeWorkspace( resultJavaFile, resultWorkspace, "UTF-8" ); //$NON-NLS-1$
      resultFile.refreshLocal( IResource.DEPTH_ONE, progress.newChild( 10 ) );
      ProgressUtilities.worked( progress, 0 );

      ProgressUtilities.worked( progress, 10 );
    }
    catch( final Throwable e )
    {
      e.printStackTrace();

      /* If anything happens, do not proceed */
      throw new CoreException( StatusUtilities.statusFromThrowable( e, Messages.getString( "org.kalypso.kalypsomodel1d2d.imports.Restart1DImporter.3" ) ) ); //$NON-NLS-1$
    }
  }

  private SortedMap<BigDecimal, GM_Point> indexProfiles( final TuhhReach reach ) throws Exception
  {
    final boolean isDirectionUpstreams = reach.getWaterBody().isDirectionUpstreams();
    /* Sort with(!) flow direction in order to get vectors right */
    final SortedMap<BigDecimal, GM_Point> map = new TreeMap<>( new TuhhStationComparator( !isDirectionUpstreams ) );

    final TuhhReachProfileSegment[] reachProfileSegments = reach.getReachProfileSegments();
    for( final TuhhReachProfileSegment segment : reachProfileSegments )
    {
      final IProfileFeature profileMember = segment.getProfileMember();
      final String crs = profileMember.getSrsName();
      final BigDecimal station = profileMember.getBigStation();
      final IProfile profil = profileMember.getProfile();

      final IProfileRecord sohlPoint = ProfileVisitors.findLowestPoint( profil );

      final GM_Point point = ProfileCacherFeaturePropertyFunction.convertPoint( profil, sohlPoint, crs );
      map.put( station, point );
    }

    return map;
  }

  private TuhhCalculation readWspmProject( final IProject project, final String calcName ) throws Exception
  {
    final IFile file = project.getFile( "modell.gml" ); //$NON-NLS-1$
    final URL modelURL = ResourceUtilities.createURL( file );
    final GMLWorkspace modelWorkspace = GmlSerializer.createGMLWorkspace( modelURL, null );
    final TuhhWspmProject tuhhWspmProject = (TuhhWspmProject)modelWorkspace.getRootFeature();

    final IFeatureBindingCollection<TuhhCalculation> calculations = tuhhWspmProject.getCalculations();
    for( final TuhhCalculation tuhhCalculation : calculations )
    {
      if( tuhhCalculation.getName().equals( calcName ) )
        return tuhhCalculation;
    }

    throw new IllegalArgumentException( Messages.getString( "org.kalypso.kalypsomodel1d2d.imports.Restart1DImporter.5" ) + calcName ); //$NON-NLS-1$
  }

  /**
   * Search for calcUnit corresponding to this 1D-project.
   * <p>
   * Creates a new one if not already exists
   * </p>
   */
  private ICalcUnitResultMeta createCalcUnitResult( final IScenarioResultMeta scenarioResultMeta, final String projectName )
  {
    final String name = Messages.getString( "org.kalypso.kalypsomodel1d2d.imports.Restart1DImporter.6", projectName ); //$NON-NLS-1$
    final String description = Messages.getString( "org.kalypso.kalypsomodel1d2d.imports.Restart1DImporter.7", name ); //$NON-NLS-1$

    /* Search for calcUnitMeta with same name (Is this strong enough?) */
    for( final IResultMeta resultMeta : scenarioResultMeta.getChildren() )
    {
      if( resultMeta instanceof ICalcUnitResultMeta )
      {
        final ICalcUnitResultMeta calcUnitMeta = (ICalcUnitResultMeta)resultMeta;
        if( calcUnitMeta.getName().equals( name ) )
          return calcUnitMeta;
      }
    }

    /* Else, create a new calcUnitMeta, using a random File name */
    final String calcUnitID = "restart1dStationary" + System.currentTimeMillis(); //$NON-NLS-1$

    final ICalcUnitResultMeta calcUnitResult = m_resultMeta.getChildren().addNew( ICalcUnitResultMeta.QNAME, ICalcUnitResultMeta.class );
    calcUnitResult.setCalcStartTime( null );
    calcUnitResult.setCalcEndTime( null );
    calcUnitResult.setCalcUnit( calcUnitID );
    calcUnitResult.setName( name );
    calcUnitResult.setDescription( description );
    calcUnitResult.setPath( Path.fromPortableString( calcUnitID ) );
    return calcUnitResult;
  }

  private IStepResultMeta createStepResult( final ICalcUnitResultMeta calcUnitResult, final String name )
  {
    /* Search for existing one */
    /* Search for calcUnitMeta with same name (Is this strong enough?) */
    for( final IResultMeta resultMeta : calcUnitResult.getChildren() )
    {
      if( resultMeta instanceof IStepResultMeta )
      {
        final IStepResultMeta stepMeta = (IStepResultMeta)resultMeta;
        if( stepMeta.getName().equals( name ) )
          return stepMeta;
      }
    }

    final String description = Messages.getString( "org.kalypso.kalypsomodel1d2d.imports.Restart1DImporter.9", name ); //$NON-NLS-1$

    final IStepResultMeta stepResultMeta = calcUnitResult.getChildren().addNew( IStepResultMeta.QNAME, IStepResultMeta.class );
    stepResultMeta.setDescription( description );
    stepResultMeta.setName( name );
    stepResultMeta.setStatus( Status.OK_STATUS );
    stepResultMeta.setStepTime( null );
    stepResultMeta.setStepType( IStepResultMeta.STEPTYPE.steady );
    stepResultMeta.setPath( Path.fromPortableString( name ) );
    return stepResultMeta;
  }

  private IDocumentResultMeta createDocument( final String projectName, final String calcName, final IStepResultMeta stepResultMeta )
  {
    final String description = Messages.getString( "org.kalypso.kalypsomodel1d2d.imports.Restart1DImporter.10", projectName, calcName ); //$NON-NLS-1$

    for( final IResultMeta resultMeta : stepResultMeta.getChildren() )
    {
      if( resultMeta instanceof IDocumentResultMeta )
      {
        final IDocumentResultMeta docMeta = (IDocumentResultMeta)resultMeta;
        if( docMeta.getDocumentType() == DOCUMENTTYPE.nodes )
          return docMeta;
      }
    }

    // TODO: message box if this document was already existent?
    return ResultMeta1d2dHelper.addDocument( stepResultMeta, "vektoren", description, DOCUMENTTYPE.nodes, Path.fromPortableString( "results.gml" ), Status.OK_STATUS, null, null ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  private IObservation<TupleResult> readLengthSection( final IFile lengthSectionFile ) throws Exception
  {
    final URL lsURL = ResourceUtilities.createURL( lengthSectionFile );
    final GMLWorkspace lsWorkspace = GmlSerializer.createGMLWorkspace( lsURL, null );
    final Feature rootFeature = lsWorkspace.getRootFeature();
    return ObservationFeatureFactory.toObservation( rootFeature );
  }

  private void createNodeResults( final IDocumentResultMeta vectorDocument, final INodeResultCollection nodeResults, final IObservation<TupleResult> lengthSectionObs, final String lsComponentStation, final String lsComponentWaterlevel, final String lsComponentVelocity, final String lsComponentType, final SortedMap<BigDecimal, GM_Point> profilesByStation )
  {
    final TupleResult lsResult = lengthSectionObs.getResult();
    final IComponent[] components = lsResult.getComponents();
    final IComponent stationComp = ComponentUtilities.findComponentByID( components, lsComponentStation );
    final IComponent waterlevelComp = ComponentUtilities.findComponentByID( components, lsComponentWaterlevel );
    final IComponent velocityComp = ComponentUtilities.findComponentByID( components, lsComponentVelocity );
    final IComponent typeComp = ComponentUtilities.findComponentByID( components, lsComponentType );

    /* Sort record in same direction as profiles */
    final SortedMap<BigDecimal, IRecord> recordsByStation = new TreeMap<>( profilesByStation.comparator() );

    /* We also filter the interpolated profiles */
    for( final IRecord record : lsResult )
    {
      final String type = (String)record.getValue( typeComp );
      if( !"i".equals( type ) ) //$NON-NLS-1$
        recordsByStation.put( (BigDecimal)record.getValue( stationComp ), record );
    }

    BigDecimal min = BigDecimal.valueOf( Double.MAX_VALUE );
    BigDecimal max = BigDecimal.valueOf( -Double.MAX_VALUE );

    final IRecord[] records = recordsByStation.values().toArray( new IRecord[recordsByStation.size()] );
    final int lsSize = records.length;
    for( int i = 0; i < lsSize; i++ )
    {
      final IRecord prevRecord = i > 0 ? records[i - 1] : null;
      final IRecord currentRecord = records[i];
      final IRecord nextRecord = i < lsSize - 1 ? records[i + 1] : null;

      final BigDecimal prevStation = prevRecord == null ? null : (BigDecimal)prevRecord.getValue( stationComp );
      final BigDecimal currentStation = (BigDecimal)currentRecord.getValue( stationComp );
      final BigDecimal nextStation = nextRecord == null ? null : (BigDecimal)nextRecord.getValue( stationComp );

      final GM_Point prevPoint = prevStation == null ? null : profilesByStation.get( prevStation );
      final GM_Point currentPoint = profilesByStation.get( currentStation );
      final GM_Point nextPoint = nextStation == null ? null : profilesByStation.get( nextStation );

      final BigDecimal prevWaterlevel = prevRecord == null ? null : (BigDecimal)prevRecord.getValue( waterlevelComp );
      final BigDecimal currentWaterlevel = (BigDecimal)currentRecord.getValue( waterlevelComp );
      final BigDecimal prevVelocity = prevRecord == null ? null : (BigDecimal)prevRecord.getValue( velocityComp );
      final BigDecimal currentVelocity = (BigDecimal)currentRecord.getValue( velocityComp );

      /* Calculate midside node if possible */
      if( prevRecord != null )
      {
        final double midVelocity = (prevVelocity.doubleValue() + currentVelocity.doubleValue()) / 2d;
        final Vector2d midsideVector2d = vectorFrom2Points( prevPoint, currentPoint );
        midsideVector2d.normalize();
        midsideVector2d.scale( midVelocity );

        final GM_Point midPoint = GeometryUtilities.createGM_PositionAtCenter( prevPoint, currentPoint );
        final BigDecimal midWaterlevel = prevWaterlevel.add( currentWaterlevel ).divide( new BigDecimal( "2" ), BigDecimal.ROUND_HALF_UP ); //$NON-NLS-1$

        createNodeResult( nodeResults, Messages.getString( "org.kalypso.kalypsomodel1d2d.imports.Restart1DImporter.15" ) + prevStation + " - " + currentStation, Messages.getString( "org.kalypso.kalypsomodel1d2d.imports.Restart1DImporter.17" ), midPoint, midWaterlevel, midsideVector2d, true ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
      }

      /* Calculate the direction */
      final Vector2d vector2d = vectorFrom3Points( prevPoint, currentPoint, nextPoint );
      vector2d.scale( currentVelocity.doubleValue() );

      createNodeResult( nodeResults, Messages.getString( "org.kalypso.kalypsomodel1d2d.imports.Restart1DImporter.18" ) + currentStation, "", currentPoint, currentWaterlevel, vector2d, false ); //$NON-NLS-1$ //$NON-NLS-2$

      min = min.min( currentVelocity );
      max = max.max( currentVelocity );
    }

    vectorDocument.setMinValue( min );
    vectorDocument.setMaxValue( max );
  }

  private void createNodeResult( final INodeResultCollection nodeResults, final String name, final String desc, final GM_Point location, final BigDecimal waterlevel, final Vector2d vector, final boolean isMidside )
  {
    final INodeResult nodeResult = nodeResults.getNodeResults().addNew( INodeResult.FEATURE_NODE_RESULT, INodeResult.class );
    nodeResult.setName( name );
    nodeResult.setDescription( desc );
    // nodeResult.setCalcId( -1 );
    nodeResult.setMidSide( isMidside );
    nodeResult.setDepth( Double.NaN );
    // nodeResult.setDry( -1 );
    nodeResult.setLocation( location.getX(), location.getY(), location.getZ(), location.getCoordinateSystem() );
    nodeResult.setResultValues( vector.x, vector.y, Double.NaN, waterlevel.doubleValue() );
    nodeResult.setVirtualDepth( nodeResult.getDepth() );
  }

  private Vector2d vectorFrom3Points( final GM_Point prevPoint, final GM_Point currentPoint, final GM_Point nextPoint )
  {
    if( prevPoint == null && nextPoint == null )
      return null;

    if( prevPoint == null )
      return vectorFrom2Points( currentPoint, nextPoint );

    if( nextPoint == null )
      return vectorFrom2Points( prevPoint, currentPoint );

    return vectorFrom2Points( prevPoint, nextPoint );
  }

  private Vector2d vectorFrom2Points( final GM_Point prevPoint, final GM_Point nextPoint )
  {
    final double difX = nextPoint.getX() - prevPoint.getX();
    final double difY = nextPoint.getY() - prevPoint.getY();

    final Vector2d vector2d = new Vector2d( difX, difY );
    vector2d.normalize();
    return vector2d;
  }
}