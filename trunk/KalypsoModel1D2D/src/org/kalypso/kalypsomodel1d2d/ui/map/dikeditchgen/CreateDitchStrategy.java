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
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.jts.JTSUtilities;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypsodeegree.model.elevation.ElevationException;
import org.kalypsodeegree.model.elevation.IElevationModel;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Triangle;
import org.kalypsodeegree.model.geometry.GM_TriangulatedSurface;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.densify.Densifier;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.CoordinateFilter;
import com.vividsolutions.jts.geom.Geometry;
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
  private CreateDitchControl m_createDitchControl;

  private final DataBindingContext m_bindingContext = new DataBindingContext();

  private IValueChangeListener m_tinAssigned = null;

  private IObservableValue m_modelTin;

  @Override
  public void createMesh( final TriangulationBuilder tinBuilder )
  {
    try
    {
      if( m_modelTin != null )
        m_modelTin.dispose();

      m_modelTin = BeansObservables.observeValue( m_bindingContext.getValidationRealm(), tinBuilder, TriangulationBuilder.PROPERTY_TIN );

      final IScenarioDataProvider caseDataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();
      final ITerrainModel model = caseDataProvider.getModel( ITerrainModel.class.getName() );
      final IElevationModel elevationModel = model.getTerrainElevationModelSystem();

      m_tinAssigned = new IValueChangeListener()
      {
        @Override
        public void handleValueChange( ValueChangeEvent event )
        {
          try
          {
            final GM_TriangulatedSurface mesh = tinBuilder.getTin();
            final String coordinateSystem = mesh.getCoordinateSystem();
            final List<GM_Triangle> triangles = new ArrayList<>( mesh.size() );
            for( final GM_Triangle triangle : mesh )
            {
              final GM_Position[] pos = triangle.getExteriorRing();
              for( int i = 0; i < 3; i++ )
              {
                final GM_Point p = GeometryFactory.createGM_Point( pos[i], coordinateSystem );
                double z = elevationModel.getElevation( p );
                pos[i] = GeometryFactory.createGM_Position( p.getX(), p.getY(), z );
              }
              final GM_Triangle newTriangle = GeometryFactory.createGM_Triangle( pos[0], pos[1], pos[2], coordinateSystem );
              triangles.add( newTriangle );
            }
            tinBuilder.setTin( GeometryFactory.createGM_TriangulatedSurface( triangles, coordinateSystem ) );
          }
          catch( final ElevationException | GM_Exception e )
          {
            e.printStackTrace();
          }
        }
      };
      m_modelTin.addValueChangeListener( m_tinAssigned );

      // add outer boundary
      if( tinBuilder.getBoundary() == null )
        addBoundary( tinBuilder );

      // add network breaklines
      final FeatureList network = m_createDitchControl.getNetworkFeatures();
      if( network != null )
        addBreaklines( tinBuilder, elevationModel );

      tinBuilder.finish();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  private void addBoundary( final TriangulationBuilder tinBuilder ) throws GM_Exception
  {
    final GM_Polygon boundaryPolygon = m_createDitchControl.getBoundaryPolygon();
    final Polygon outerRing = (Polygon)JTSAdapter.export( boundaryPolygon ).getGeometryN( 0 );
    final String coordinateSystem = boundaryPolygon.getCoordinateSystem();
    tinBuilder.setBoundary( (GM_Polygon)JTSAdapter.wrap( outerRing, coordinateSystem ), false );
  }

  private LineString[] getLines( final Feature linearFeature, final double densifyFactor, final CoordinateFilter networkFilter )
  {
    try
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
        if( networkFilter != null )
          linestringZ.apply( networkFilter );
        // TODO: densify from-with, apply factor!
        results[i] = linestringZ;// (LineString)Densifier.densify( simplified, minWidth * densifyFactor );
      }
      return results;
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
      return new LineString[0];
    }
  }

  private Pair<Double, Double> getWidth( final Feature linearFeature )
  {
    final IPropertyType startWidthProperty = m_createDitchControl.getNetworkStartWidthProperty();
    final IPropertyType endWidthProperty = m_createDitchControl.getNetworkEndWidthProperty();
    final Double startWidth = (Double)linearFeature.getProperty( startWidthProperty );
    final Double endWidth = (Double)linearFeature.getProperty( endWidthProperty );
    final Pair<Double, Double> startEnd = Pair.of( startWidth, endWidth );
    return startEnd;
  }

  private void addBreaklines( final TriangulationBuilder tinBuilder, final IElevationModel elevationModel ) throws GM_Exception
  {
    final double innerWidthFraction = m_createDitchControl.getInnerWidthFraction();
    final double innerDensifyFactor = innerWidthFraction > 0.5 ? 4 / innerWidthFraction : innerWidthFraction * 2;
    final double outerDensifyFactor = innerWidthFraction > 0.5 ? 2 / innerWidthFraction : innerWidthFraction * 4;

    final FeatureList network = m_createDitchControl.getNetworkFeatures();
    final int networkSize = network.size();
    if( networkSize == 0 )
      return;

    final Feature firstFeature = network.getResolved( 0 );
    final GM_Object firstGeometry = (GM_Object)firstFeature.getProperty( m_createDitchControl.getNetworkGeometryProperty() );
    final String networkCoordinateSystem = firstGeometry.getCoordinateSystem();

    // get outermost ring for upper bankline
    final InterpolateElevationFilter interpolateElevationFilter = new InterpolateElevationFilter( networkCoordinateSystem, elevationModel );
    final Collection<SegmentString> outerSegStrList = new ArrayList<>( networkSize );
    double minimumWidth = Double.MAX_VALUE;
    for( int i = 0; i < networkSize; i++ )
    {
      final Feature linearFeature = network.getResolved( i );
      final LineString[] lines = getLines( linearFeature, outerDensifyFactor, null );
      final Pair<Double, Double> startEnd = getWidth( linearFeature );
      final double minWidth = Math.min( startEnd.getLeft(), startEnd.getRight() );
      minimumWidth = minWidth < minimumWidth ? minWidth : minimumWidth;
      for( int j = 0; j < lines.length; j++ )
        LineStringBufferBuilder.addRawBufferLines( lines[j], startEnd.getLeft() * 1.0, startEnd.getRight() * 1.0, startEnd.getLeft() * 1.0, startEnd.getRight() * 1.0, outerSegStrList );
    }
    final Polygon outerBuffered = (Polygon)LineStringBufferBuilder.buffer( outerSegStrList );
    final Polygon outerFiltered = JTSUtilities.removeCoincidentPoints( outerBuffered, minimumWidth, minimumWidth / 5 );
    final Polygon outerDensified = (Polygon)Densifier.densify( outerFiltered, minimumWidth * outerDensifyFactor );
    outerDensified.apply( interpolateElevationFilter );
    final GM_Polygon upperBankline = (GM_Polygon)JTSAdapter.wrap( outerDensified, networkCoordinateSystem );
    addPolygonAsBreakline( tinBuilder, upperBankline );

    // temporary elevation model for ensuring minimum depth inside ditch
    final double minAngle = tinBuilder.getMinAngle();
    final double maxArea = tinBuilder.getMaxArea();
    final boolean noSteinerOnBoundary = tinBuilder.getNoSteinerOnBoundary();
    tinBuilder.setNoSteinerOnBoundary( true, false );
    tinBuilder.setMinAngle( 0, false );
    tinBuilder.setMaxArea( 0, false );
    tinBuilder.finish();
    tinBuilder.setNoSteinerOnBoundary( noSteinerOnBoundary, false );
    tinBuilder.setMinAngle( minAngle, false );
    tinBuilder.setMaxArea( maxArea, false );

    final GM_TriangulatedSurface tin = tinBuilder.getTin();
    final double minimumDepth = m_createDitchControl.getMinimumDepth();
    final CoordinateFilter minimumDepthFilter = new CoordinateFilter()
    {

      @Override
      public void filter( final Coordinate coord )
      {
        try
        {
          final GM_Point p = GeometryFactory.createGM_Point( JTSAdapter.wrap( coord ), networkCoordinateSystem );
          final double outsideElevation = tin.getValue( p );
          double depth = outsideElevation - coord.z;
          if( depth < minimumDepth )
            depth = minimumDepth;
          coord.z = outsideElevation - depth;
        }
        catch( final Exception e )
        {
          e.printStackTrace();
        }
      }
    };

    final Collection<SegmentString> innerSegStrList = new ArrayList<>( networkSize );
    for( int i = 0; i < networkSize; i++ )
    {
      final Feature linearFeature = network.getResolved( i );
      final LineString[] lines = getLines( linearFeature, innerDensifyFactor, minimumDepthFilter );
      for( int j = 0; j < lines.length; j++ )
      {
        final GM_Curve edgeCurve = (GM_Curve)JTSAdapter.wrap( lines[j], networkCoordinateSystem );
        tinBuilder.addBreakLine( edgeCurve, false );

        final Pair<Double, Double> startEnd = getWidth( linearFeature );
        LineStringBufferBuilder.addRawBufferLines( lines[j], startEnd.getLeft() * innerWidthFraction, startEnd.getRight() * innerWidthFraction, startEnd.getLeft() * innerWidthFraction, startEnd.getRight()
            * innerWidthFraction, innerSegStrList );
      }
    }
    final Polygon innerBuffered = (Polygon)LineStringBufferBuilder.buffer( innerSegStrList );
    final Polygon innerZ = JTSUtilities.interpolateMissingZ( innerBuffered );
    final Polygon innerFiltered = JTSUtilities.removeCoincidentPoints( innerZ, minimumWidth * innerWidthFraction, minimumWidth * innerWidthFraction / 5 );
    final GM_Polygon inner = (GM_Polygon)JTSAdapter.wrap( innerFiltered, networkCoordinateSystem );
    addPolygonAsBreakline( tinBuilder, inner );

    m_modelTin.removeValueChangeListener( m_tinAssigned );
    m_tinAssigned = new IValueChangeListener()
    {
      @Override
      public void handleValueChange( ValueChangeEvent event )
      {
        try
        {
          final GM_TriangulatedSurface mesh = tinBuilder.getTin();
          final String coordinateSystem = mesh.getCoordinateSystem();
          final List<GM_Triangle> triangles = new ArrayList<>( mesh.size() );
          for( final GM_Triangle triangle : mesh )
          {
            final GM_Position[] pos = triangle.getExteriorRing();
            for( int i = 0; i < 3; i++ )
            {
              if( !upperBankline.contains( pos[i] ) )
              {
                final GM_Point p = GeometryFactory.createGM_Point( pos[i], coordinateSystem );
                double z = elevationModel.getElevation( p );
                pos[i] = GeometryFactory.createGM_Position( p.getX(), p.getY(), z );
              }
            }
            final GM_Triangle newTriangle = GeometryFactory.createGM_Triangle( pos[0], pos[1], pos[2], coordinateSystem );
            triangles.add( newTriangle );
          }
          tinBuilder.setTin( GeometryFactory.createGM_TriangulatedSurface( triangles, coordinateSystem ) );
        }
        catch( final ElevationException | GM_Exception e )
        {
          e.printStackTrace();
        }
      }
    };
    m_modelTin.addValueChangeListener( m_tinAssigned );
  }

  @Override
  public Control createControl( final Composite body, final FormToolkit toolkit, final IMapModell mapModell )
  {
    m_createDitchControl = new CreateDitchControl( body, toolkit, mapModell );
    return m_createDitchControl;
  }
}
