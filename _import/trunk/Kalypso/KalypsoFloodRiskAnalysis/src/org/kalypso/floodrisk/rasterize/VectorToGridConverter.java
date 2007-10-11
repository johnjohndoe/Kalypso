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
package org.kalypso.floodrisk.rasterize;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Hashtable;
import java.util.List;

import javax.xml.namespace.QName;

import ogc31.www.opengis.net.gml.FileType;
import ogc31.www.opengis.net.gml.RangeSetType;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.kalypso.contribs.ogc31.KalypsoOGC31JAXBcontext;
import org.kalypso.floodrisk.schema.UrlCatalogFloodRisk;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.SimulationDataPath;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.cv.RectifiedGridCoverage2;
import org.kalypsodeegree_impl.model.cv.RectifiedGridDomain;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Class for converting Vectordata (featureList (Properties: "GEOM" and "RasterProperty")) to Rasterdata
 * (RectifiedGridCoverages)
 * 
 * @author N. Peiler
 * 
 */
public class VectorToGridConverter
{

  /**
   * converts a List of Features to a RectifiedGridCoverage2
   * 
   * @param featureList
   *            List of Features with properties "GEOM" and "RasterProperty" (value of gridCell)
   * @param propertyTable
   *            Mapping of key and propertyValue (e.g. landuseTypeList)
   * @param baseGrid
   *            RectifiedGridCoverage that defines the origin, offset and gridRange of the new grid
   * @return new RectifiedGridCoverage
   * @throws Exception
   */
  public static RectifiedGridCoverage2 createCoverage( final List<Feature> featureList, final Hashtable<Object, Long> propertyTable, final RectifiedGridCoverage2 baseGrid, final SimulationDataPath resultRasterFile, final ISimulationMonitor monitor ) throws Exception
  {
    final QName propertyName = new QName( UrlCatalogFloodRisk.NS_VECTORDATAMODEL, "RasterProperty" );
    final RectifiedGridDomain gridDomain = baseGrid.getGridDomain();
    final GM_Point origin = gridDomain.getOrigin( null );
    final RectifiedGridDomain newGridDomain = new RectifiedGridDomain( origin, gridDomain.getOffsetX(), gridDomain.getOffsetY(), baseGrid.getGridDomain().getGridRange() );
    final double originX = origin.getX();
    final double originY = origin.getY();
    final double offsetX = baseGrid.getGridDomain().getOffsetX( origin.getCoordinateSystem() );
    final double offsetY = baseGrid.getGridDomain().getOffsetY( origin.getCoordinateSystem() );
    final String datFileName = baseGrid.getRangeSet().getFile().getFileName();
    final IPath workspacePath = new Path( baseGrid.getFeature().getWorkspace().getContext().getPath() );
    final File inputDatFile = workspacePath.removeLastSegments( 1 ).append( datFileName ).toFile();
    final File outputDatFile = new File( resultRasterFile.getPath() );
    BufferedReader reader = null;
    BufferedWriter writer = null;
    try
    {
      final long fileLength = inputDatFile.length();
      long processed = 0;
      reader = new BufferedReader( new FileReader( inputDatFile ) );
      writer = new BufferedWriter( new FileWriter( outputDatFile ) );
      String line = null;
      int cellY = 0;
      while( (line = reader.readLine()) != null )
      {
        final String[] chunks = line.split( " " );
        for( int cellX = 0; cellX < chunks.length; cellX++ )
        {
          final double x = originX + (cellX + 0.5) * offsetX;
          final double y = originY + (cellY + 0.5) * offsetY;
          final GM_Position position = GeometryFactory.createGM_Position( x, y );
          try
          {
            Long key = null;
            for( int k = 0; k < featureList.size(); k++ )
            {
              final Feature actualFeature = featureList.get( k );
              GM_Object gm_Object = actualFeature.getDefaultGeometryProperty();
              if( gm_Object.contains( position ) )
              {
                final String property = actualFeature.getProperty( propertyName ).toString();
                key = propertyTable.get( property );
                break;
              }
            }
            if( key != null )
              writer.write( key.toString() + " " );
            else
              writer.write( "null " );
          }
          catch( final NumberFormatException e )
          {
            writer.write( "null " );
          }
        }
        processed += line.length() + 1;
        monitor.setProgress( (int) (100.0 * processed / fileLength) );
        cellY++;
        writer.newLine();
      }
    }
    catch( final FileNotFoundException ex )
    {
      ex.printStackTrace();
    }
    catch( final IOException ex )
    {
      ex.printStackTrace();
    }
    finally
    {
      try
      {
        if( reader != null )
          reader.close();
        if( writer != null )
        {
          writer.flush();
          writer.close();
        }
      }
      catch( IOException ex )
      {
        ex.printStackTrace();
      }
    }
    final FileType rangeSetFile = KalypsoOGC31JAXBcontext.GML3_FAC.createFileType();
    rangeSetFile.setFileName( outputDatFile.toURL().toExternalForm() );
    rangeSetFile.setMimeType( "text/asc" );
    final RangeSetType rangeSet = KalypsoOGC31JAXBcontext.GML3_FAC.createRangeSetType();
    rangeSet.setFile( rangeSetFile );
    final RectifiedGridCoverage2 rectifiedGridCoverage = RectifiedGridCoverage2.createRectifiedGridCoverage();
    rectifiedGridCoverage.setGridDomain( newGridDomain );
    rectifiedGridCoverage.setRangeSet( rangeSet );
    return rectifiedGridCoverage;
  }
}