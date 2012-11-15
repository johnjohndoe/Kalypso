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

import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.apache.commons.io.FilenameUtils;
import org.kalypso.model.wspm.ewawi.data.EwawiPlus;
import org.kalypso.model.wspm.ewawi.utils.EwawiKey;
import org.kalypso.model.wspm.ewawi.utils.GewShape;
import org.kalypso.model.wspm.ewawi.utils.GewWidthShape;
import org.kalypso.shape.ShapeFile;
import org.kalypso.shape.ShapeType;
import org.kalypso.shape.dbf.DBFField;
import org.kalypso.shape.dbf.DBaseException;
import org.kalypso.shape.dbf.FieldType;
import org.kalypso.shape.dbf.IDBFField;
import org.kalypso.shape.geometry.SHPPoint;
import org.kalypso.shape.shp.SHPException;

/**
 * Writes EWAWI+ shape file 32.
 * 
 * @author Holger Albert
 */
public class EwawiShape32Writer extends AbstractEwawiShapeWriter
{
  public EwawiShape32Writer( final EwawiPlus[] data, final GewShape gewShape, final GewWidthShape gewWidthShape )
  {
    super( data, gewShape, gewWidthShape, "824_Fotos", ShapeType.POINT );
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
  protected void writeData( final ShapeFile shapeFile, final EwawiPlus[] data ) throws DBaseException, IOException, SHPException
  {
    writeData( shapeFile, data[0] );
  }

  private void writeData( final ShapeFile shapeFile, final EwawiPlus data ) throws DBaseException, IOException, SHPException
  {
    final File file = getFotoList();

    final FotoListReader reader = new FotoListReader();
    reader.read( file );

    final FotoListData[] fotoListData = reader.getFotoListData();
    for( final FotoListData oneFotoListData : fotoListData )
    {
      final SHPPoint shape = getShape( oneFotoListData );
      final Object[] values = getValues( oneFotoListData, data );

      shapeFile.addFeature( shape, values );
    }
  }

  private SHPPoint getShape( final FotoListData oneFotoListData )
  {
    final double rechtswert = oneFotoListData.getRechtswert().doubleValue();
    final double hochwert = oneFotoListData.getHochwert().doubleValue();

    return new SHPPoint( rechtswert, hochwert );
  }

  private Object[] getValues( final FotoListData fotoListData, final EwawiPlus data )
  {
    final EwawiKey key = data.getKey();
    final Path relativeFotoPath = getRelativeFotoPath();

    final String alias = key.getAlias();
    final String aufn_richt = getAufnRicht( fotoListData.getFilename() );
    final String comment = "";
    final String bildname = fotoListData.getFilename();
    final Date datum = fotoListData.getDatum();
    final String link = Paths.get( relativeFotoPath.toString(), fotoListData.getFilename() ).toString();
    final String pak = "";
    final String pfad = relativeFotoPath.toString();
    final String bearb = "BJG";
    final String uhrzeit = "";
    final BigDecimal x = fotoListData.getRechtswert();
    final BigDecimal y = fotoListData.getHochwert();
    final BigDecimal z = fotoListData.getHoehe();

    checkFotoLink( link );

    final List<Object> values = new ArrayList<>();
    values.add( alias );
    values.add( aufn_richt );
    values.add( comment );
    values.add( bildname );
    values.add( datum );
    values.add( link );
    values.add( pak );
    values.add( pfad );
    values.add( bearb );
    values.add( uhrzeit );
    values.add( x );
    values.add( y );
    values.add( z );

    return values.toArray( new Object[] {} );
  }

  private File getFotoList( )
  {
    final File targetFile = getTargetFile();
    final String fullPath = FilenameUtils.getFullPath( targetFile.getAbsolutePath() );
    final String fotoPath = FilenameUtils.normalize( String.format( "%s../%s", fullPath, "824_Fotos/" ) );

    final EwawiPlus[] data = getData();
    final EwawiKey key = data[0].getKey();
    final String fotoName = String.format( "%s.csv", key.getAlias() );

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

  private String getAufnRicht( final String filename )
  {
    final String baseName = FilenameUtils.removeExtension( filename );
    final String[] splittedBaseName = baseName.split( "_" );
    final String aufn_richt = splittedBaseName[splittedBaseName.length - 1];
    if( aufn_richt.length() != 1 )
      throw new IllegalStateException( String.format( "Die Aufnahmerichtung muss am Ende des Dateinamens angehängt sein und darf nur ein Zeichen lang sein. Gefunden: %s (%s)", filename, aufn_richt ) );

    return aufn_richt;
  }

  private void checkFotoLink( final String link )
  {
    final File targetFile = getTargetFile();
    final String fullPath = FilenameUtils.getFullPath( targetFile.getAbsolutePath() );

    final Path absoluteFullPath = Paths.get( fullPath );
    final Path relativeFotoPath = Paths.get( link );
    final Path absoluteFotoPath = absoluteFullPath.resolve( relativeFotoPath );

    final File file = absoluteFotoPath.toFile();
    if( !file.exists() )
      System.out.println( String.format( "Die Datei '%s' existiert nicht...", file.getPath() ) );
  }
}