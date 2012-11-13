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
package org.kalypso.model.wspm.ewawi.shape.writer;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.IOUtils;
import org.kalypso.model.wspm.ewawi.data.EwawiPlus;
import org.kalypso.model.wspm.ewawi.utils.EwawiKey;
import org.kalypso.model.wspm.ewawi.utils.GewShape;
import org.kalypso.shape.ShapeFile;
import org.kalypso.shape.ShapeType;
import org.kalypso.shape.dbf.DBFField;
import org.kalypso.shape.dbf.DBaseException;
import org.kalypso.shape.dbf.FieldType;
import org.kalypso.shape.dbf.IDBFField;

import au.com.bytecode.opencsv.CSVReader;

/**
 * Writes EWAWI+ shape file 32.
 * 
 * @author Holger Albert
 */
public class EwawiShape32Writer extends AbstractEwawiShapeWriter
{
  public EwawiShape32Writer( final EwawiPlus[] data, final GewShape gewShape )
  {
    super( data, gewShape, "824_Fotos", ShapeType.POINT );
  }

  @Override
  protected String getTargetFilename( final EwawiKey key )
  {
    return String.format( "%s_VG_BJG_VMFOST_PKT_Freitext.shp", key.getAlias() );
  }

  @Override
  protected IDBFField[] createFields( ) throws DBaseException
  {
    final List<IDBFField> fields = new ArrayList<>();
    fields.add( new DBFField( "ALIAS", FieldType.C, (short)6, (short)0 ) );
    fields.add( new DBFField( "AUFN_RICHT", FieldType.C, (short)1, (short)0 ) );
    fields.add( new DBFField( "Bemerkung", FieldType.C, (short)254, (short)0 ) );
    fields.add( new DBFField( "Bildname", FieldType.C, (short)254, (short)0 ) );
    fields.add( new DBFField( "DATUM", FieldType.D, (short)8, (short)0 ) );
    fields.add( new DBFField( "LINK", FieldType.C, (short)254, (short)0 ) );
    fields.add( new DBFField( "PAK", FieldType.C, (short)60, (short)0 ) );
    fields.add( new DBFField( "PFAD", FieldType.C, (short)254, (short)0 ) );
    fields.add( new DBFField( "SL_BEARB", FieldType.C, (short)3, (short)0 ) );
    fields.add( new DBFField( "Uhrzeit", FieldType.C, (short)8, (short)0 ) );
    fields.add( new DBFField( "x", FieldType.N, (short)12, (short)3 ) );
    fields.add( new DBFField( "y", FieldType.N, (short)12, (short)3 ) );
    fields.add( new DBFField( "z", FieldType.N, (short)7, (short)3 ) );

    return fields.toArray( new IDBFField[] {} );
  }

  @Override
  protected void writeData( final ShapeFile shapeFile, final EwawiPlus data[] ) throws IOException
  {
    writeData( shapeFile );
  }

  private void writeData( final ShapeFile shapeFile ) throws IOException
  {
    CSVReader reader = null;

    try
    {
      final File fotoList = getFotoList();
      reader = new CSVReader( new BufferedReader( new FileReader( fotoList ) ), ',' );

      String[] tokens = null;
      while( (tokens = reader.readNext()) != null )
      {
        final String filename = tokens[0];
        final String rechtswert = tokens[1];
        final String hochwert = tokens[2];
        final String hoehe = tokens[3];
        final String datum = tokens[4];

        // TODO
      }
    }
    finally
    {
      IOUtils.closeQuietly( reader );
    }
  }

  private File getFotoList( )
  {
    final File targetFile = getTargetFile();
    final String fullPath = FilenameUtils.getFullPath( targetFile.getAbsolutePath() );
    final String fotoPath = FilenameUtils.normalize( String.format( "%s../%s", fullPath, "824_Fotos/" ) );

    final EwawiPlus[] data = getData();
    final EwawiKey key = data[0].getKey();
    final String fotoName = key.getAlias();

    return new File( fotoPath, fotoName );
  }

  private Path getRelativeFotoPath( )
  {
    final File targetFile = getTargetFile();
    final String fullPath = FilenameUtils.getFullPath( targetFile.getAbsolutePath() );
    final String fotoPath = FilenameUtils.normalize( String.format( "%s../%s", fullPath, "824_Fotos/" ) );

    final Path absoluteFullPath = Paths.get( fullPath );
    final Path absoluteFotoPath = Paths.get( fotoPath );
    final Path relativeFotoPath = absoluteFullPath.relativize( absoluteFotoPath );

    return relativeFotoPath;
  }
}