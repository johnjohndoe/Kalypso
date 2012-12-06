package org.kalypso.ewawi.shape.writer;

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
import org.kalypso.model.wspm.ewawi.utils.log.XyzEwawiLogger;
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
  protected void writeData( final ShapeFile shapeFile, final EwawiPlus[] data, final XyzEwawiLogger logger ) throws DBaseException, IOException, SHPException
  {
    for( final EwawiPlus ewawiData : data )
      writeData( shapeFile, ewawiData, logger );
  }

  private void writeData( final ShapeFile shapeFile, final EwawiPlus data, final XyzEwawiLogger logger ) throws DBaseException, IOException, SHPException
  {
    final EwawiEpl eplIndex = data.getEplIndex();
    final EwawiEplLine[] eplLines = eplIndex.getEplLines();
    for( final EwawiEplLine eplLine : eplLines )
    {
      final EwawiObjectart objectArt = eplLine.getObjectArt();
      if( EwawiObjectart._3000 == objectArt )
        writeEplLine( shapeFile, eplLine, logger );
    }
  }

  private void writeEplLine( final ShapeFile shapeFile, final EwawiEplLine eplLine, final XyzEwawiLogger logger ) throws DBaseException, IOException, SHPException
  {
    final SHPPoint shape = getShape( eplLine );
    final Object[] values = getValues( eplLine, shape, logger );

    shapeFile.addFeature( shape, values );
  }

  private SHPPoint getShape( final EwawiEplLine eplLine )
  {
    final double rechtswert = eplLine.getRechtswert().doubleValue();
    final double hochwert = eplLine.getHochwert().doubleValue();

    return new SHPPoint( rechtswert, hochwert );
  }

  private Object[] getValues( final EwawiEplLine eplLine, final SHPPoint shape, final XyzEwawiLogger logger ) throws DBaseException
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
    final String gewName = (String)getGewShape().getValue( gewKennzahl, GewShape.GN_ACHS_08, geometry, logger );
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