/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Bj�rnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universit�t Hamburg-Harburg, Institut f�r Wasserbau, Hamburg, Germany
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

import java.io.IOException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import org.kalypso.model.wspm.ewawi.data.EwawiPlus;
import org.kalypso.model.wspm.ewawi.data.EwawiPro;
import org.kalypso.model.wspm.ewawi.data.EwawiProLine;
import org.kalypso.model.wspm.ewawi.data.EwawiSta;
import org.kalypso.model.wspm.ewawi.data.EwawiStaLine;
import org.kalypso.model.wspm.ewawi.data.enums.EwawiPunktart;
import org.kalypso.model.wspm.ewawi.utils.EwawiKey;
import org.kalypso.model.wspm.ewawi.utils.EwawiException;
import org.kalypso.model.wspm.ewawi.utils.GewShape;
import org.kalypso.model.wspm.ewawi.utils.profiles.EwawiProfilePoint;
import org.kalypso.shape.ShapeFile;
import org.kalypso.shape.ShapeType;
import org.kalypso.shape.dbf.DBFField;
import org.kalypso.shape.dbf.DBaseException;
import org.kalypso.shape.dbf.FieldType;
import org.kalypso.shape.dbf.IDBFField;
import org.kalypso.shape.geometry.SHPPoint;
import org.kalypso.shape.shp.SHPException;

/**
 * Writes EWAWI+ shape file 347.
 * 
 * @author Holger Albert
 */
public class EwawiShape347Writer extends AbstractEwawiShapeWriter
{
  public EwawiShape347Writer( final EwawiPlus[] data, final GewShape gewShape )
  {
    super( data, gewShape, "404_GIS", ShapeType.POINT );
  }

  @Override
  protected String getTargetFilename( final EwawiKey key )
  {
    return String.format( "%s_VG_BJG_PROFIL_PKT_Freitext.shp", key.getAlias() );
  }

  @Override
  protected IDBFField[] createFields( ) throws DBaseException
  {
    final List<IDBFField> fields = new ArrayList<>();
    fields.add( new DBFField( "BEMERKUNG", FieldType.C, (short)254, (short)0 ) );
    fields.add( new DBFField( "HOCHWERT", FieldType.N, (short)7, (short)3 ) );
    fields.add( new DBFField( "HORIZONT", FieldType.N, (short)3, (short)0 ) );
    fields.add( new DBFField( "ID_Profil", FieldType.N, (short)4, (short)0 ) );
    fields.add( new DBFField( "OBJEKTART", FieldType.N, (short)4, (short)0 ) );
    fields.add( new DBFField( "PAK", FieldType.C, (short)60, (short)0 ) );
    fields.add( new DBFField( "Punktart", FieldType.N, (short)3, (short)0 ) );
    fields.add( new DBFField( "Punktnr", FieldType.N, (short)4, (short)0 ) );
    fields.add( new DBFField( "PUNKTSEQ", FieldType.N, (short)4, (short)0 ) );
    fields.add( new DBFField( "Rechtswert", FieldType.N, (short)7, (short)3 ) );
    fields.add( new DBFField( "SL_GEW_KNZ", FieldType.N, (short)15, (short)0 ) );
    fields.add( new DBFField( "Z", FieldType.N, (short)7, (short)3 ) );

    return fields.toArray( new IDBFField[] {} );
  }

  @Override
  protected void writeData( final ShapeFile shapeFile, final EwawiPlus[] data ) throws DBaseException, IOException, SHPException, EwawiException
  {
    for( final EwawiPlus ewawiData : data )
      writeData( shapeFile, ewawiData );
  }

  private void writeData( final ShapeFile shapeFile, final EwawiPlus data ) throws DBaseException, IOException, SHPException, EwawiException
  {
    /* Get the pro index and the sta index. */
    final EwawiPro proIndex = data.getProIndex();
    final EwawiSta staIndex = data.getStaIndex();

    /* Get all pro lines (all profile points). */
    final EwawiProLine[] proLines = proIndex.getProLines();
    for( final EwawiProLine proLine : proLines )
      writeProLine( shapeFile, staIndex, proLine );
  }

  private void writeProLine( final ShapeFile shapeFile, final EwawiSta staIndex, final EwawiProLine proLine ) throws EwawiException, IOException, DBaseException, SHPException
  {
    /* Find the fix points. */
    final EwawiStaLine leftFixPoint = staIndex.findFixPoint( proLine.getObjectArt(), EwawiPunktart._1, proLine.getGewKennzahl(), proLine.getStation() );
    final EwawiStaLine rightFixPoint = staIndex.findFixPoint( proLine.getObjectArt(), EwawiPunktart._2, proLine.getGewKennzahl(), proLine.getStation() );
    if( leftFixPoint == null || rightFixPoint == null )
      throw new EwawiException( "Einer der Festpunkte wurde nicht gefunden." );

    /* Create the profile point. */
    final EwawiProfilePoint proPoint = new EwawiProfilePoint( leftFixPoint, rightFixPoint, proLine );

    /* Get the shape. */
    final SHPPoint shape = proPoint.getShape();

    /* Get the values. */
    final Object[] values = getValues( proLine, proPoint );

    /* Add the feature. */
    shapeFile.addFeature( shape, values );
  }

  private Object[] getValues( final EwawiProLine proLine, final EwawiProfilePoint proPoint )
  {
    final String comment = proLine.getComment();
    final BigDecimal hochwert = proLine.getHochwert();
    final int horizont = proLine.getHorizont().getKey();
    final Short profilNummer = proPoint.getProfilNummer();
    final int objectArt = proLine.getObjectArt().getKey();
    final String pak = "";
    final int punktArt = proLine.getPunktArt().getKey();
    final Short punktNummer = proLine.getPunktNummer();
    final Short punktReihenfolge = proLine.getPunktReihenfolge();
    final BigDecimal rechtswert = proLine.getRechtswert();
    final Long gewKennzahl = proLine.getGewKennzahl();
    final BigDecimal hoehe = proPoint.getHoehe();

    final List<Object> values = new ArrayList<>();
    values.add( comment );
    values.add( hochwert );
    values.add( horizont );
    values.add( profilNummer );
    values.add( objectArt );
    values.add( pak );
    values.add( punktArt );
    values.add( punktNummer );
    values.add( punktReihenfolge );
    values.add( rechtswert );
    values.add( gewKennzahl );
    values.add( hoehe );

    return values.toArray( new Object[] {} );
  }
}