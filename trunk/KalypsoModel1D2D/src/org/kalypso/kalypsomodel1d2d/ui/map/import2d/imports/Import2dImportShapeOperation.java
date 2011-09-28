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
package org.kalypso.kalypsomodel1d2d.ui.map.import2d.imports;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Collection;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.internal.import2dm.IPolygonWithName;
import org.kalypso.kalypsomodel1d2d.internal.import2dm.PolygonWithName;
import org.kalypso.kalypsomodel1d2d.ui.map.import2d.Import2dElementsData;
import org.kalypso.shape.FileMode;
import org.kalypso.shape.ShapeFile;
import org.kalypso.shape.ShapeFileUtils;
import org.kalypso.shape.ShapeType;
import org.kalypso.shape.dbf.DBaseException;
import org.kalypso.shape.geometry.ISHPGeometry;
import org.kalypso.shape.tools.SHP2JTS;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.MultiPolygon;
import com.vividsolutions.jts.geom.Polygon;

/**
 * @author Gernot Belger
 */
public class Import2dImportShapeOperation extends AbstractImport2DImportOperation
{
  private final SHP2JTS m_transformer = new SHP2JTS( new GeometryFactory() );

  public Import2dImportShapeOperation( final Import2dElementsData data, final Import2dImportData importData )
  {
    super( data, importData );
  }

  @Override
  public String getFilterName( )
  {
    // FIXME: get from elsewhere
    return "ESRI Shape Files";
  }

  @Override
  public String getFilterExtension( )
  {
    return ShapeFile.EXTENSION_SHP;
  }

  @Override
  protected Pair<IStatus, IPolygonWithName[]> readFileData( final File importFile, final int sourceSrid, final IProgressMonitor monitor ) throws InvocationTargetException, CoreException
  {
    final String filePath = importFile.getAbsolutePath();

    ShapeFile shapeFile = null;
    try
    {
      shapeFile = new ShapeFile( filePath, Charset.defaultCharset(), FileMode.READ );

      final ShapeType shapeType = shapeFile.getShapeType();
      if( !(shapeType == ShapeType.POLYGON || shapeType == ShapeType.POLYGONZ) )
      {
        final String message = String.format( "Illegal shape type (''%s), only %s or %s allowed.", shapeType, ShapeType.POLYGON, ShapeType.POLYGONZ );
        final IStatus readStatus = new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, message );
        return new ImmutablePair<IStatus, IPolygonWithName[]>( readStatus, null );
      }

      final int numRecords = shapeFile.getNumRecords();

      monitor.beginTask( String.format( "Reading %s", filePath ), numRecords );

      // TODO: potential heap exception here -> handle!
      final Collection<IPolygonWithName> polygons = new ArrayList<IPolygonWithName>( numRecords );

      for( int i = 0; i < numRecords; i++ )
      {
        if( i % 100 == 0 )
          monitor.subTask( String.format( "%d/%d", i + 1, numRecords ) );

        final ISHPGeometry shape = shapeFile.getShape( i );
        addPolygons( polygons, i, shape );

        ProgressUtilities.worked( monitor, 1 );
      }

      shapeFile.close();

      return new ImmutablePair<IStatus, IPolygonWithName[]>( Status.OK_STATUS, polygons.toArray( new IPolygonWithName[polygons.size()] ) );
    }
    catch( final IOException e )
    {
      throw new InvocationTargetException( e );
    }
    catch( final DBaseException e )
    {
      throw new InvocationTargetException( e );
    }
    finally
    {
      ShapeFileUtils.closeQuiet( shapeFile );

      monitor.done();
    }
  }

  private void addPolygons( final Collection<IPolygonWithName> polygons, final int row, final ISHPGeometry shape )
  {
    final Geometry geom = m_transformer.transform( shape );

    /* Ignore null geometries */
    if( geom == null )
      return;

    if( geom instanceof MultiPolygon )
    {
      final MultiPolygon multiPolygon = (MultiPolygon) geom;

      final int numGeometries = multiPolygon.getNumGeometries();
      for( int j = 0; j < numGeometries; j++ )
      {
        final Polygon polygon = (Polygon) multiPolygon.getGeometryN( row );

        final String name = buildName( row, j );

        polygons.add( new PolygonWithName( name, polygon ) );
      }
    }

    /* we test for polygon before, but now we got it, something's wrong! */
    throw new IllegalStateException();
  }

  // TODO: get name of one of the fields?
  private String buildName( final int row, final int j )
  {
    if( j == 0 )
      return Integer.toString( row );

    return String.format( "%d_%d", row, j );
  }
}