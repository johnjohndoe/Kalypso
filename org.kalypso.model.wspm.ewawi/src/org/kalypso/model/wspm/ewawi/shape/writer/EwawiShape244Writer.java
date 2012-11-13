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
import java.util.List;

import org.apache.commons.io.FilenameUtils;
import org.kalypso.model.wspm.ewawi.data.EwawiPlus;
import org.kalypso.model.wspm.ewawi.data.EwawiPro;
import org.kalypso.model.wspm.ewawi.data.EwawiSta;
import org.kalypso.model.wspm.ewawi.utils.EwawiException;
import org.kalypso.model.wspm.ewawi.utils.EwawiKey;
import org.kalypso.model.wspm.ewawi.utils.GewShape;
import org.kalypso.model.wspm.ewawi.utils.profiles.EwawiProfile;
import org.kalypso.model.wspm.ewawi.utils.profiles.EwawiProfilePart;
import org.kalypso.shape.ShapeFile;
import org.kalypso.shape.ShapeType;
import org.kalypso.shape.dbf.DBFField;
import org.kalypso.shape.dbf.DBaseException;
import org.kalypso.shape.dbf.FieldType;
import org.kalypso.shape.dbf.IDBFField;
import org.kalypso.shape.geometry.SHPPolyLinez;
import org.kalypso.shape.shp.SHPException;

/**
 * Writes EWAWI+ shape file 244.
 * 
 * @author Holger Albert
 */
public class EwawiShape244Writer extends AbstractEwawiShapeWriter
{
  /**
   * The number of written parts.
   */
  private int m_countParts;

  public EwawiShape244Writer( final EwawiPlus[] data, final GewShape gewShape )
  {
    super( data, gewShape, "404_GIS", ShapeType.POLYLINEZ );

    m_countParts = 0;
  }

  @Override
  protected String getTargetFilename( final EwawiKey key )
  {
    return String.format( "%s_VG_BJG_PROFIL_LMZ_Freitext.shp", key.getAlias() );
  }

  @Override
  protected IDBFField[] createFields( ) throws DBaseException
  {
    final List<IDBFField> fields = new ArrayList<>();
    fields.add( new DBFField( "ABRECHNUNG", FieldType.N, (short)3, (short)0 ) );
    fields.add( new DBFField( "ALIAS", FieldType.C, (short)6, (short)0 ) );
    fields.add( new DBFField( "BEMERKUNG", FieldType.C, (short)254, (short)0 ) );
    fields.add( new DBFField( "FKM", FieldType.N, (short)8, (short)3 ) );
    fields.add( new DBFField( "HORIZONT", FieldType.N, (short)3, (short)0 ) );
    fields.add( new DBFField( "ID_Profil", FieldType.N, (short)4, (short)0 ) );
    fields.add( new DBFField( "KLASSE", FieldType.C, (short)1, (short)0 ) );
    fields.add( new DBFField( "LINK", FieldType.C, (short)254, (short)0 ) );
    fields.add( new DBFField( "OBJEKTART", FieldType.N, (short)4, (short)0 ) );
    fields.add( new DBFField( "PAK", FieldType.C, (short)60, (short)0 ) );
    fields.add( new DBFField( "PDF", FieldType.C, (short)254, (short)0 ) );
    fields.add( new DBFField( "PFAD", FieldType.C, (short)254, (short)0 ) );
    fields.add( new DBFField( "Profilname", FieldType.C, (short)70, (short)0 ) );
    fields.add( new DBFField( "Profiltyp", FieldType.N, (short)4, (short)0 ) );
    fields.add( new DBFField( "PROFNR", FieldType.N, (short)4, (short)0 ) );
    fields.add( new DBFField( "SL_GEW_KNZ", FieldType.N, (short)15, (short)0 ) );
    fields.add( new DBFField( "SL_GEW_NAM", FieldType.C, (short)254, (short)0 ) );
    fields.add( new DBFField( "UEBERLANG", FieldType.C, (short)1, (short)0 ) );
    fields.add( new DBFField( "ZUSATZ", FieldType.N, (short)4, (short)0 ) );

    return fields.toArray( new IDBFField[] {} );
  }

  @Override
  protected void writeData( final ShapeFile shapeFile, final EwawiPlus data[] ) throws DBaseException, IOException, SHPException, EwawiException
  {
    for( final EwawiPlus ewawiData : data )
      writeData( shapeFile, ewawiData );
  }

  private void writeData( final ShapeFile shapeFile, final EwawiPlus data ) throws DBaseException, IOException, SHPException, EwawiException
  {
    final EwawiPro proIndex = data.getProIndex();
    final EwawiProfile[] profiles = proIndex.getProfiles();
    for( final EwawiProfile profile : profiles )
      writeProfile( shapeFile, profile, data );
  }

  private void writeProfile( final ShapeFile shapeFile, final EwawiProfile profile, final EwawiPlus data ) throws DBaseException, IOException, SHPException, EwawiException
  {
    final EwawiSta staIndex = data.getStaIndex();

    final EwawiProfilePart[] parts = profile.getParts();
    for( final EwawiProfilePart part : parts )
    {
      final SHPPolyLinez shape = part.getShape( staIndex );
      final Object[] values = getValues( part, data );

      shapeFile.addFeature( shape, values );
    }
  }

  private Object[] getValues( final EwawiProfilePart part, final EwawiPlus data ) throws DBaseException, EwawiException
  {
    final EwawiKey key = data.getKey();
    final EwawiSta staIndex = data.getStaIndex();
    final Path relativeFotoPath = getRelativePlotPath();

    final Short abrechnung = 0; // TODO ausgerechnete Meter?
    final String alias = key.getAlias();
    final String comment = part.getComment( staIndex );
    final BigDecimal station = part.getStation();
    final Integer horizont = part.getHorizont();
    final int idProfil = ++m_countParts;
    final String klasse = ""; // TODO Klasse 1-4
    final int objectArt = part.getObjectArt().getKey();
    final String pak = "";
    final String pfad = relativeFotoPath.toString();
    final int profilArt = part.getProfilArt( staIndex ).getKey();
    final Short profilNummer = part.getProfilNummer( staIndex );
    final Long gewKennzahl = part.getGewKennzahl();
    final String gewName = getGewShape().getName( key.getAlias() );
    final String ueberlang = ""; // TODO Küpfmüller
    final Short zusatz = part.getZusatz();

    final String profilName = String.format( "%s_%d", alias, profilNummer );
    final String pdf = String.format( "%s_%s_%s_VP_BJG_QPPLOT_%d.pdf", key.getPe(), key.getAlias(), key.getModelId(), profilNummer );
    final String link = Paths.get( pfad, pdf ).toString();

    checkPlotLink( link );

    final List<Object> values = new ArrayList<>();
    values.add( abrechnung );
    values.add( alias );
    values.add( comment );
    values.add( station );
    values.add( horizont );
    values.add( idProfil );
    values.add( klasse );
    values.add( link );
    values.add( objectArt );
    values.add( pak );
    values.add( pdf );
    values.add( pfad );
    values.add( profilName );
    values.add( profilArt );
    values.add( profilNummer );
    values.add( gewKennzahl );
    values.add( gewName );
    values.add( ueberlang );
    values.add( zusatz );

    return values.toArray( new Object[] {} );
  }

  private void checkPlotLink( final String link )
  {
    final File targetFile = getTargetFile();
    final String fullPath = FilenameUtils.getFullPath( targetFile.getAbsolutePath() );

    final Path absoluteFullPath = Paths.get( fullPath );
    final Path relativePlotPath = Paths.get( link );
    final Path absolutePlotPath = absoluteFullPath.resolve( relativePlotPath );

    final File file = absolutePlotPath.toFile();
    if( !file.exists() )
      System.out.println( String.format( "Die Datei '%s' existiert nicht...", file.getPath() ) );
  }

  private Path getRelativePlotPath( )
  {
    final File targetFile = getTargetFile();
    final String fullPath = FilenameUtils.getFullPath( targetFile.getAbsolutePath() );
    final String plotPath = FilenameUtils.normalize( String.format( "%s../%s", fullPath, "844_Plot/" ) );

    final Path absoluteFullPath = Paths.get( fullPath );
    final Path absolutePlotPath = Paths.get( plotPath );
    final Path relativePlotPath = absoluteFullPath.relativize( absolutePlotPath );

    return relativePlotPath;
  }
}