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

import java.io.IOException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.kalypso.model.wspm.ewawi.data.EwawiEpl;
import org.kalypso.model.wspm.ewawi.data.EwawiEplLine;
import org.kalypso.model.wspm.ewawi.data.EwawiPlus;
import org.kalypso.model.wspm.ewawi.data.enums.EwawiObjectart;
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
import org.kalypso.shape.tools.SHP2JTS;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;

/**
 * Writes EWAWI+ shape file 38.
 * 
 * @author Holger Albert
 */
public class EwawiShape38Writer extends AbstractEwawiShapeWriter
{
  public EwawiShape38Writer( final EwawiPlus[] data, final GewShape gewShape, final GewWidthShape gewWidthShape )
  {
    super( data, gewShape, gewWidthShape, "404_GIS", ShapeType.POINT );
  }

  @Override
  protected String getTargetFilename( final EwawiKey key )
  {
    return String.format( "%s_VG_BJG_PUNKTE_PKT_Freitext.shp", key.getAlias() );
  }

  @Override
  protected IDBFField[] createFields( ) throws DBaseException
  {
    final List<IDBFField> fields = new ArrayList<>();
    fields.add( new DBFField( "BEMERKUNG", FieldType.C, (short)254, (short)0 ) );
    fields.add( new DBFField( "DATUM", FieldType.D, (short)8, (short)0 ) );
    fields.add( new DBFField( "FKM", FieldType.C, (short)254, (short)0 ) );
    fields.add( new DBFField( "OBJEKTART", FieldType.N, (short)4, (short)0 ) );
    fields.add( new DBFField( "OBJEKTNR", FieldType.N, (short)6, (short)0 ) );
    fields.add( new DBFField( "PAK", FieldType.C, (short)60, (short)0 ) );
    fields.add( new DBFField( "Punktcode", FieldType.N, (short)3, (short)0 ) );
    fields.add( new DBFField( "SL_BEARB", FieldType.C, (short)3, (short)0 ) );
    fields.add( new DBFField( "SL_GEW_KNZ", FieldType.N, (short)15, (short)0 ) );
    fields.add( new DBFField( "SL_GEW_NAM", FieldType.C, (short)254, (short)0 ) );
    fields.add( new DBFField( "Z", FieldType.N, (short)7, (short)3 ) );
    fields.add( new DBFField( "ZUSATZ", FieldType.N, (short)4, (short)0 ) );

    return fields.toArray( new IDBFField[] {} );
  }

  @Override
  protected void writeData( final ShapeFile shapeFile, final EwawiPlus[] data ) throws DBaseException, IOException, SHPException
  {
    for( final EwawiPlus ewawiData : data )
      writeData( shapeFile, ewawiData );
  }

  private void writeData( final ShapeFile shapeFile, final EwawiPlus data ) throws DBaseException, IOException, SHPException
  {
    final EwawiEpl eplIndex = data.getEplIndex();
    final EwawiEplLine[] eplLines = eplIndex.getEplLines();
    for( final EwawiEplLine eplLine : eplLines )
    {
      final EwawiObjectart objectArt = eplLine.getObjectArt();
      if( EwawiObjectart._3000 == objectArt )
        writeEplLine( shapeFile, eplLine );
    }
  }

  private void writeEplLine( final ShapeFile shapeFile, final EwawiEplLine eplLine ) throws DBaseException, IOException, SHPException
  {
    final SHPPoint shape = getShape( eplLine );
    final Object[] values = getValues( eplLine, shape );

    shapeFile.addFeature( shape, values );
  }

  private SHPPoint getShape( final EwawiEplLine eplLine )
  {
    final double rechtswert = eplLine.getRechtswert().doubleValue();
    final double hochwert = eplLine.getHochwert().doubleValue();

    return new SHPPoint( rechtswert, hochwert );
  }

  private Object[] getValues( final EwawiEplLine eplLine, final SHPPoint shape ) throws DBaseException
  {
    final String comment = eplLine.getComment();
    final Date validity = eplLine.getValidity();
    final BigDecimal station = eplLine.getStation();
    final int objectArt = eplLine.getObjectArt().getKey();
    final Short objektNummer = eplLine.getObjektNummer();
    final String pak = "";
    final int punktArt = eplLine.getPunktArt().getKey();
    final String bearb = "BJG";
    final Long gewKennzahl = eplLine.getGewKennzahl();
    final SHP2JTS shp2jts = new SHP2JTS( new GeometryFactory() );
    final Geometry geometry = shp2jts.transform( shape );
    final String gewName = (String)getGewShape().getValue( gewKennzahl, GewShape.GN_ACHS_08, geometry );
    final BigDecimal hoehe = eplLine.getHoehe();
    final Short zusatz = eplLine.getZusatz();

    final List<Object> values = new ArrayList<>();
    values.add( comment );
    values.add( validity );
    values.add( station );
    values.add( objectArt );
    values.add( objektNummer );
    values.add( pak );
    values.add( punktArt );
    values.add( bearb );
    values.add( gewKennzahl );
    values.add( gewName );
    values.add( hoehe );
    values.add( zusatz );

    return values.toArray( new Object[] {} );
  }
}