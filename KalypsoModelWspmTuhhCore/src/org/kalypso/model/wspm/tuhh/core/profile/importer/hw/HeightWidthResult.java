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
package org.kalypso.model.wspm.tuhh.core.profile.importer.hw;

import java.io.File;
import java.util.ArrayList;
import java.util.Formatter;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import javax.xml.namespace.QName;

import org.apache.commons.lang.ArrayUtils;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.xml.XmlTypes;
import org.kalypso.gmlschema.GMLSchemaFactory;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.ITypeRegistry;
import org.kalypso.gmlschema.types.MarshallingTypeRegistrySingleton;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree_impl.io.shpapi.ShapeConst;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.geom.TopologyException;
import com.vividsolutions.jts.operation.valid.IsValidOp;
import com.vividsolutions.jts.operation.valid.TopologyValidationError;

/**
 * @author belger
 */
public abstract class HeightWidthResult extends ProblemResult implements IHeightWidthResult
{
  private final String m_parentName;

  private Polygon m_polygon;

  private double[] m_heights;

  private double[] m_widths;

  private final String m_id;

  private final String m_name;

  private final File m_tempDir;

  public HeightWidthResult( final String parentName, final String dataName, final String id, final String name, final File tempDir )
  {
    super( dataName, null );
    m_parentName = parentName;
    m_id = id;
    m_name = name;
    m_tempDir = tempDir;
  }

  private void calculate( )
  {
    if( m_polygon != null )
      return;

    final List<Coordinate> crds = new ArrayList<Coordinate>( buildPolygon() );
    if( crds.size() < 3 )
    {
      addStatus( IStatus.WARNING, "Invalid geometry (not enough points)", null ); //$NON-NLS-1$
      return;
    }

    if( !crds.get( 0 ).equals( crds.get( crds.size() - 1 ) ) )
      crds.add( new Coordinate( crds.get( 0 ) ) );
    final Coordinate[] crdArray = crds.toArray( new Coordinate[crds.size()] );
    final LinearRing ring = GF.createLinearRing( crdArray );

    final IsValidOp isValidOp = new IsValidOp( ring );
    final boolean valid = isValidOp.isValid();

    debugShapeWrite( ring, valid );
    if( !valid )
    {
      final TopologyValidationError validationError = isValidOp.getValidationError();
      final String message = validationError.getMessage();
      final Coordinate coordinate = validationError.getCoordinate();
      final String msg = String.format( "Invalid geometry: '%s' at ", message, coordinate ); //$NON-NLS-1$
      addStatus( IStatus.ERROR, msg, null );
      return;
    }

    m_polygon = GF.createPolygon( ring, null );

    /* Calculate Widths/Heights */
    final LineString shell = m_polygon.getExteriorRing();
    final Coordinate[] coordinates = shell.getCoordinates();

    m_heights = sampleHeights( coordinates );

    double min = Double.POSITIVE_INFINITY;
    double max = Double.NEGATIVE_INFINITY;
    for( final Coordinate coordinate : coordinates )
    {
      min = Math.min( min, coordinate.x );
      max = Math.max( max, coordinate.x );
    }

    /* Calculate width'es */
    m_widths = calculateWidth( m_heights, min, max );
  }

  private double[] sampleHeights( final Coordinate[] coordinates )
  {
    final Set<Double> sortedSet = new TreeSet<Double>();

// double lastY = Double.NaN;
    for( final Coordinate coordinate : coordinates )
    {
      final double y = coordinate.y;
      sortedSet.add( new Double( y ) );
// lastY = y;
    }

// Double[] ySoFar = sortedSet.toArray( new Double[sortedSet.size()] );

    final Double[] result = sortedSet.toArray( new Double[sortedSet.size()] );
    return ArrayUtils.toPrimitive( result );
  }

  private double[] calculateWidth( final double[] heights, final double min, final double max )
  {
    final double[] widths = new double[heights.length];
    for( int i = 0; i < heights.length; i++ )
    {
      try
      {
        final double height = heights[i];

        /* Construct a line that intersects at the given height */
        final Coordinate left = new Coordinate( min, height );
        final Coordinate right = new Coordinate( max, height );
        final LineString horizontalLine = m_polygon.getFactory().createLineString( new Coordinate[] { left, right } );
        final Geometry intersection = m_polygon.intersection( horizontalLine );
        widths[i] = intersection.getLength();
      }
      catch( final TopologyException e )
      {
        if( i == 0 || i == heights.length - 1 )
        {
          widths[i] = 0;
          addStatus( IStatus.INFO, "Topology Problem at start or end", e ); //$NON-NLS-1$
        }
        else
        {
          widths[i] = 0;
          addStatus( IStatus.WARNING, "Topology Problem in the middle, height/width not correctly calculated", e ); //$NON-NLS-1$
        }
      }
    }

    return widths;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.profile.importer.hw.IHeightWidthResult#formatOut(java.util.Formatter)
   */
  @Override
  public void formatOut( final Formatter formatter )
  {
    calculate();

    if( m_polygon == null )
      return;

    double maxWidth = -Double.MAX_VALUE;
    for( int i = 0; i < m_widths.length; i++ )
      maxWidth = Math.max( maxWidth, m_widths[i] );

    final String id = m_id;
    final String name = m_name;
    formatter.format( "CRDS id '%s' nm '%s' ty 0 wm %f w1 0 w2 0 sw 0 gl 0 gu 0 lt lw%n", id, name, maxWidth ); //$NON-NLS-1$
    formatter.format( "TBLE%n" ); //$NON-NLS-1$
    for( int i = 0; i < m_heights.length; i++ )
    {
      final double height = m_heights[i];
      final double width = m_widths[i];
      final double relHeight = height - m_heights[0];
      formatter.format( "%f %f %f <%n", relHeight, width, width ); //$NON-NLS-1$
    }
    formatter.format( "tble%n" ); //$NON-NLS-1$
    formatter.format( "crds%n" ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.profile.importer.hw.ProblemResult#formatErr(java.util.Formatter)
   */
  @Override
  public void formatLog( final Formatter formatter )
  {
    calculate();
    if( m_polygon == null )
      return;

    super.formatLog( formatter );

    final double areaPoly = m_polygon.getArea();
    final double areaHW = calculateArea( m_widths, m_heights );

    formatter.format( "Fl‰che Shape: %f%n", areaPoly ); //$NON-NLS-1$
    formatter.format( "Fl‰che HW   : %f%n", areaHW ); //$NON-NLS-1$
  }

  private void debugShapeWrite( final LinearRing shell, final boolean valid )
  {
    try
    {
      /* Create feature type which describes what data the shape file contains */
      final ITypeRegistry<IMarshallingTypeHandler> typeRegistry = MarshallingTypeRegistrySingleton.getTypeRegistry();

      final IMarshallingTypeHandler nameTypeHandler = typeRegistry.getTypeHandlerForTypeName( XmlTypes.XS_STRING );
      final IMarshallingTypeHandler validTypeHandler = typeRegistry.getTypeHandlerForTypeName( XmlTypes.XS_STRING );
      final IMarshallingTypeHandler lineTypeHandler = typeRegistry.getTypeHandlerForTypeName( GeometryUtilities.QN_LINE_STRING );

      final QName shapeTypeQName = new QName( "anyNS", "shapeType" ); //$NON-NLS-1$ //$NON-NLS-2$

      final IValuePropertyType nameType = GMLSchemaFactory.createValuePropertyType( new QName( "anyNS", "name" ), nameTypeHandler, 1, 1, false ); //$NON-NLS-1$ //$NON-NLS-2$
      final IValuePropertyType validType = GMLSchemaFactory.createValuePropertyType( new QName( "anyNS", "valid" ), validTypeHandler, 1, 1, false ); //$NON-NLS-1$ //$NON-NLS-2$
      final IValuePropertyType lineType = GMLSchemaFactory.createValuePropertyType( new QName( "anyNS", "aGeometry" ), lineTypeHandler, 1, 1, false ); //$NON-NLS-1$ //$NON-NLS-2$

      final IPropertyType[] properties = new IPropertyType[] { lineType, nameType, validType };
      final IFeatureType shapeFT = GMLSchemaFactory.createFeatureType( shapeTypeQName, properties );

      /* Create the shape root feature, we need it to create the children. */
      final Feature shapeRootFeature = ShapeSerializer.createWorkspaceRootFeature( shapeFT, ShapeConst.SHAPE_TYPE_POINT );
      final GMLWorkspace workspace = shapeRootFeature.getWorkspace();
      final IRelationType shapeParentRelation = (IRelationType) shapeRootFeature.getFeatureType().getProperty( ShapeSerializer.PROPERTY_FEATURE_MEMBER );

      /* Now create some features of this type */
      final GM_Curve aCurve = (GM_Curve) JTSAdapter.wrap( shell );

      final Object[] data = new Object[] { aCurve, getName(), Boolean.toString( valid ) };
      final Feature feature = FeatureFactory.createFeature( shapeRootFeature, shapeParentRelation, "FeatureID" + 0, shapeFT, data ); //$NON-NLS-1$
      workspace.addFeatureAsComposition( shapeRootFeature, shapeParentRelation, -1, feature );

      final File shapeFile = new File( m_tempDir, m_parentName + "_" + getName() ); //$NON-NLS-1$

      ShapeSerializer.serialize( workspace, shapeFile.getAbsolutePath(), null );
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
    }
    catch( final GmlSerializeException e )
    {
      e.printStackTrace();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  /** Calculates the area of a width/height profile. */
  private static double calculateArea( final double[] widths, final double[] heights )
  {
    double area = 0.0;
    for( int i = 0; i < widths.length - 1; i++ )
    {
      final double width1 = widths[i];
      final double height1 = heights[i];
      final double width2 = widths[i + 1];
      final double height2 = heights[i + 1];

      final double height = height2 - height1;
      final double trapecoidArea = calculateArea( width1, width2, height );
      area += trapecoidArea;
    }

    return area;
  }

  /** Calculates the area of a trapezoid */
  private static double calculateArea( final double width1, final double width2, final double height )
  {
    final double width = (width1 + width2) / 2;
    return width * height;
  }

  protected abstract List<Coordinate> buildPolygon( );

}
