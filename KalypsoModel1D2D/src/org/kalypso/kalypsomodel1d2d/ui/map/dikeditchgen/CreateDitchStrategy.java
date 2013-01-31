/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.kalypsomodel1d2d.ui.map.dikeditchgen;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.apache.commons.lang3.tuple.Pair;
import org.eclipse.core.databinding.DataBindingContext;
import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.databinding.observable.value.IValueChangeListener;
import org.eclipse.core.databinding.observable.value.ValueChangeEvent;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.jts.JTSUtilities;
import org.kalypso.kalypsomodel1d2d.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypsodeegree.model.elevation.IElevationModel;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_MultiCurve;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Triangle;
import org.kalypsodeegree.model.geometry.GM_TriangulatedSurface;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.CoordinateFilter;
import com.vividsolutions.jts.geom.CoordinateList;
import com.vividsolutions.jts.geom.CoordinateSequence;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.LineSegment;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.noding.SegmentString;
import com.vividsolutions.jts.simplify.DouglasPeuckerSimplifier;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * @author kurzbach
 */
public class CreateDitchStrategy extends AbstractCreateStructuredNetworkStrategy implements CreateStructuredNetworkStrategy
{
  protected CreateDitchControl m_createDitchControl;

  private final DataBindingContext m_bindingContext = new DataBindingContext();

  private final GM_MultiSurface m_upperBankline = GeometryFactory.createGM_MultiSurface( new GM_Polygon[0], null );

  @Override
  public Control createControl( final Composite body, final FormToolkit toolkit, final IMapModell mapModell )
  {
    m_createDitchControl = new CreateDitchControl( body, toolkit, mapModell );
    return m_createDitchControl;
  }

  @Override
  public IStatus createMesh( final TriangulationBuilder tinBuilder ) throws CoreException
  {
    IObservableValue modelTinObservable = null;
    try
    {
      // clean up previous data
      final GM_MultiCurve currentBreaklines = tinBuilder.getBreaklines();
      if( currentBreaklines != null )
        currentBreaklines.removeAll();
      m_upperBankline.removeAll();

      final GM_MultiSurface boundaries = tinBuilder.getBoundaries();
      if( boundaries != null )
        boundaries.removeAll();

      // observe tin for interpolation
      final IValueChangeListener interpolateElevationListener = getInterpolateElevationListener( tinBuilder, m_upperBankline );
      modelTinObservable = BeansObservables.observeValue( m_bindingContext.getValidationRealm(), tinBuilder, TriangulationBuilder.PROPERTY_TIN );
      modelTinObservable.addValueChangeListener( interpolateElevationListener );

      // add outer boundary
      addBoundary( tinBuilder );

      // add network breaklines
      addBreaklines( tinBuilder );

      tinBuilder.finish();
    }
    catch( final GM_Exception e )
    {
      throw new CoreException( StatusUtilities.statusFromThrowable( e ) );
    }
    finally
    {
      if( modelTinObservable != null )
        modelTinObservable.dispose();
    }

    return Status.OK_STATUS;
  }

  private static IValueChangeListener getInterpolateElevationListener( final TriangulationBuilder tinBuilder, final GM_MultiSurface upperBankline ) throws CoreException
  {
    final IElevationModel elevationModel = getElevationModel();
    final IValueChangeListener tinAssignedListener = new IValueChangeListener()
    {
      @Override
      public void handleValueChange( final ValueChangeEvent event )
      {
        final Shell shell = Display.getCurrent().getActiveShell();
        final IStatus result = ProgressUtilities.busyCursorWhile( new ICoreRunnableWithProgress()
        {

          @Override
          public IStatus execute( final IProgressMonitor pm )
          {
            try
            {
              final GM_TriangulatedSurface mesh = tinBuilder.getTin();
              if( mesh == null )
                return Status.OK_STATUS;

              final String coordinateSystem = mesh.getCoordinateSystem();
              final List<GM_Triangle> triangles = new ArrayList<>( mesh.size() );
              final SubMonitor monitor = SubMonitor.convert( pm, Messages.getString( "CreateDitchStrategy.0" ), triangles.size() ); //$NON-NLS-1$
              for( final GM_Triangle triangle : mesh )
              {
                ProgressUtilities.worked( monitor, 1 );
                final GM_Position[] pos = triangle.getExteriorRing();
                for( int i = 0; i < 3; i++ )
                {
                  final GM_Point p = GeometryFactory.createGM_Point( pos[i], coordinateSystem );
                  if( upperBankline.contains( p ) )
                    continue;
                  final double z = elevationModel.getElevation( p );
                  pos[i] = GeometryFactory.createGM_Position( p.getX(), p.getY(), z );
                }
                final GM_Triangle newTriangle = GeometryFactory.createGM_Triangle( pos[0], pos[1], pos[2], coordinateSystem );
                triangles.add( newTriangle );
              }
              mesh.clear();
              mesh.addAll( triangles );
            }
            catch( final Exception e )
            {
              return StatusUtilities.statusFromThrowable( e );
            }
            return Status.OK_STATUS;
          }
        } );
        if( !result.isOK() )
          StatusDialog.open( shell, result, Messages.getString( "CreateDitchStrategy.0" ) ); //$NON-NLS-1$
      }
    };
    return tinAssignedListener;
  }

  private static IElevationModel getElevationModel( ) throws CoreException
  {
    final IScenarioDataProvider caseDataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();
    final ITerrainModel model = caseDataProvider.getModel( ITerrainModel.class.getName() );
    final IElevationModel elevationModel = model.getTerrainElevationModelSystem();
    return elevationModel;
  }

  private void addBoundary( final TriangulationBuilder tinBuilder )
  {
    final FeatureList boundaryFeatures = m_createDitchControl.getBoundaryFeatures();
    if( boundaryFeatures == null || boundaryFeatures.isEmpty() )
      return;

    final Feature[] features = boundaryFeatures.toFeatures( new Feature[boundaryFeatures.size()] );
    for( Feature feature : features )
    {
      final GM_Object geometry = (GM_Object)feature.getProperty( m_createDitchControl.getBoundaryGeometryProperty() );
      if( geometry instanceof GM_Polygon )
        tinBuilder.addBoundary( (GM_Polygon)geometry );
      else if( geometry instanceof GM_MultiSurface )
      {
        final GM_Polygon[] allSurfaces = ((GM_MultiSurface)geometry).getAllSurfaces();
        for( GM_Polygon polygon : allSurfaces )
          tinBuilder.addBoundary( polygon );
      }
    }
  }

  private LineString[] getLines( final Feature linearFeature, final int densifyFactor ) throws GM_Exception
  {
    // convert to jts, add Z, simplify and densify
    final IPropertyType geomProperty = m_createDitchControl.getNetworkGeometryProperty();
    final GM_Object geometry = (GM_Object)linearFeature.getProperty( geomProperty );
    final Geometry linestring = JTSAdapter.export( geometry ).getGeometryN( 0 );
    linestring.apply( new CoordinateFilter()
    {

      @Override
      public void filter( final Coordinate coord )
      {
        if( coord.z < -100 )
          coord.z = Double.NaN;
      }
    } );

    final Pair<Double, Double> startEnd = getWidth( linearFeature );
    final double minWidth = Math.min( startEnd.getLeft(), startEnd.getRight() );
    final LineString[] results = new LineString[linestring.getNumGeometries()];
    for( int i = 0; i < results.length; i++ )
    {
      final LineString simplified = (LineString)DouglasPeuckerSimplifier.simplify( linestring.getGeometryN( i ), minWidth / 20 );
      final LineString linestringZ = JTSUtilities.interpolateMissingZ( simplified );
      results[i] = densify( linestringZ, startEnd.getLeft(), startEnd.getRight(), densifyFactor );
    }
    return results;
  }

  private Pair<Double, Double> getWidth( final Feature linearFeature )
  {
    final IPropertyType startWidthProperty = m_createDitchControl.getNetworkStartWidthProperty();
    final IPropertyType endWidthProperty = m_createDitchControl.getNetworkEndWidthProperty();
    final double startWidth = ((Number)linearFeature.getProperty( startWidthProperty )).doubleValue();
    final double endWidth = ((Number)linearFeature.getProperty( endWidthProperty )).doubleValue();
    final Pair<Double, Double> startEnd = Pair.of( startWidth, endWidth );
    return startEnd;
  }

  private void addBreaklines( final TriangulationBuilder tinBuilder ) throws GM_Exception, CoreException
  {

    final FeatureList network = m_createDitchControl.getNetworkFeatures();
    if( network == null || network.isEmpty() )
      return;

    final double innerWidthFraction = m_createDitchControl.getInnerWidthFraction();
    final int innerDensifyFactor = innerWidthFraction <= 0.5 ? 1 : 1;
    final int outerDensifyFactor = innerWidthFraction <= 0.5 ? 1 : 1;

    final Feature firstFeature = network.getResolved( 0 );
    final GM_Object firstGeometry = (GM_Object)firstFeature.getProperty( m_createDitchControl.getNetworkGeometryProperty() );
    final String networkCoordinateSystem = firstGeometry.getCoordinateSystem();
    final int networkSize = network.size();

    // get outermost ring for upper bankline
    final IElevationModel elevationModel = getElevationModel();
    final InterpolateElevationFilter interpolateElevationFilter = new InterpolateElevationFilter( networkCoordinateSystem, elevationModel );
    final Collection<SegmentString> outerSegStrList = new ArrayList<>( networkSize );
    double minimumWidth = Double.MAX_VALUE;
    for( int i = 0; i < networkSize; i++ )
    {
      final Feature linearFeature = network.getResolved( i );
      final LineString[] lines = getLines( linearFeature, outerDensifyFactor );
      final Pair<Double, Double> startEnd = getWidth( linearFeature );
      final double minWidth = Math.min( startEnd.getLeft(), startEnd.getRight() );
      minimumWidth = minWidth < minimumWidth ? minWidth : minimumWidth;
      for( int j = 0; j < lines.length; j++ )
        LineStringBufferBuilder.addRawBufferLines( lines[j], startEnd.getLeft(), startEnd.getRight(), startEnd.getLeft(), startEnd.getRight(), outerSegStrList );
    }
    final Geometry outerBuffered = LineStringBufferBuilder.buffer( outerSegStrList );

    m_upperBankline.removeAll();
    for( int i = 0; i < outerBuffered.getNumGeometries(); i++ )
    {
      final Polygon outerFiltered = JTSUtilities.removeCoincidentPoints( (Polygon)outerBuffered.getGeometryN( i ), minimumWidth / 2, minimumWidth / 10 );
      outerFiltered.apply( interpolateElevationFilter );
      final GM_Polygon outerPoly = (GM_Polygon)JTSAdapter.wrap( outerFiltered, networkCoordinateSystem );
      m_upperBankline.addSurface( outerPoly );
      tinBuilder.addBoundary( outerPoly );
    }

    // temporary elevation model for ensuring minimum depth inside ditch
//    final double minAngle = tinBuilder.getMinAngle();
//    final double maxArea = tinBuilder.getMaxArea();
//    final boolean noSteinerOnBoundary = tinBuilder.getNoSteinerOnBoundary();
//    tinBuilder.setNoSteinerOnBoundary( true );
//    tinBuilder.setMinAngle( 0 );
//    tinBuilder.setMaxArea( 0 );
//    tinBuilder.finish();
//    tinBuilder.setNoSteinerOnBoundary( noSteinerOnBoundary );
//    tinBuilder.setMinAngle( minAngle );
//    tinBuilder.setMaxArea( maxArea );

    final Collection<SegmentString> innerSegStrList = new ArrayList<>( networkSize );
    for( int i = 0; i < networkSize; i++ )
    {
      final Feature linearFeature = network.getResolved( i );
      final LineString[] lines = getLines( linearFeature, innerDensifyFactor );
      for( int j = 0; j < lines.length; j++ )
      {
        final GM_Curve edgeCurve = (GM_Curve)JTSAdapter.wrap( lines[j], networkCoordinateSystem );
        tinBuilder.addBreakLine( edgeCurve );
        final Pair<Double, Double> startEnd = getWidth( linearFeature );
        LineStringBufferBuilder.addRawBufferLines( lines[j], startEnd.getLeft() * innerWidthFraction, startEnd.getRight() * innerWidthFraction, startEnd.getLeft() * innerWidthFraction, startEnd.getRight()
            * innerWidthFraction, innerSegStrList );
      }
    }
    final Geometry innerBuffered = LineStringBufferBuilder.buffer( innerSegStrList );
    for( int i = 0; i < innerBuffered.getNumGeometries(); i++ )
    {
      final Polygon innerZ = JTSUtilities.interpolateMissingZ( (Polygon)innerBuffered.getGeometryN( i ) );
      final Polygon innerFiltered = JTSUtilities.removeCoincidentPoints( innerZ, minimumWidth / 2 * innerWidthFraction, minimumWidth * innerWidthFraction / 10 );
      final GM_Polygon inner = (GM_Polygon)JTSAdapter.wrap( innerFiltered, networkCoordinateSystem );
      addPolygonAsBreakline( tinBuilder, inner );
    }
  }

  private static LineString densify( final LineString lineString, final double distanceToleranceStart, final double distanceToleranceEnd, final int densifyFactor )
  {
    final double totalLength = lineString.getLength();
    final CoordinateSequence coordinateSequence = lineString.getCoordinateSequence();
    final int pointCount = coordinateSequence.size();
    final double diff = distanceToleranceEnd - distanceToleranceStart;
    double currentStartTolerance = distanceToleranceStart;
    final LineSegment seg = new LineSegment();
    final CoordinateList coordList = new CoordinateList();
    for( int i = 0; i < pointCount - 1; i++ )
    {
      seg.p0 = coordinateSequence.getCoordinate( i );
      seg.p1 = coordinateSequence.getCoordinate( i + 1 );
      coordList.add( seg.p0, false );
      final double len = seg.getLength();
      final double currentDiff = len / totalLength * diff;
      final double currentEndTolerance = currentStartTolerance + currentDiff;
      final double meanDistanceTolerance = (currentStartTolerance + currentEndTolerance) / 2;
      final int densifiedSegCount = densifyFactor * ((int)(len / meanDistanceTolerance) + 1);
      final double inc = (currentEndTolerance - currentStartTolerance) / (densifiedSegCount - 1);
      final double normLength = currentStartTolerance * densifiedSegCount + densifiedSegCount * (densifiedSegCount - 1) / 2 * inc;
      for( int j = 1; j < densifiedSegCount; j++ )
      {
        final double segFract = (currentStartTolerance * j + j * (j - 1) / 2 * inc) / normLength;
        final Coordinate p = seg.pointAlong( segFract );
        p.z = seg.p0.z + segFract * (seg.p1.z - seg.p0.z);
        coordList.add( p, false );
      }
      currentStartTolerance = currentEndTolerance;
    }
    coordList.add( coordinateSequence.getCoordinate( pointCount - 1 ), false );
    return lineString.getFactory().createLineString( coordList.toCoordinateArray() );
  }
}
