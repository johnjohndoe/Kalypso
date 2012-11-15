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
import java.util.ArrayList;
import java.util.List;

import org.kalypso.model.wspm.ewawi.data.EwawiEpl;
import org.kalypso.model.wspm.ewawi.data.EwawiPlus;
import org.kalypso.model.wspm.ewawi.utils.EwawiKey;
import org.kalypso.model.wspm.ewawi.utils.GewShape;
import org.kalypso.model.wspm.ewawi.utils.GewWidthShape;
import org.kalypso.model.wspm.ewawi.utils.structures.EwawiLengthStructure;
import org.kalypso.shape.ShapeFile;
import org.kalypso.shape.ShapeType;
import org.kalypso.shape.dbf.DBFField;
import org.kalypso.shape.dbf.DBaseException;
import org.kalypso.shape.dbf.FieldType;
import org.kalypso.shape.dbf.IDBFField;
import org.kalypso.shape.geometry.SHPPolyLinez;
import org.kalypso.shape.shp.SHPException;
import org.kalypso.shape.tools.SHP2JTS;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;

/**
 * Writes EWAWI+ shape file 348.
 * 
 * @author Holger Albert
 */
public class EwawiShape348Writer extends AbstractEwawiShapeWriter
{
  public EwawiShape348Writer( final EwawiPlus[] data, final GewShape gewShape, final GewWidthShape gewWidthShape )
  {
    super( data, gewShape, gewWidthShape, "404_GIS", ShapeType.POLYLINEZ );
  }

  @Override
  protected String getTargetFilename( final EwawiKey key )
  {
    return String.format( "%s_VG_BJG_LAENGS_LMZ_Freitext.shp", key.getAlias() );
  }

  @Override
  protected IDBFField[] createFields( ) throws DBaseException
  {
    final List<IDBFField> fields = new ArrayList<>();
    fields.add( new DBFField( "BEMERKUNG", FieldType.C, (short)254, (short)0 ) );
    fields.add( new DBFField( "OBJEKTART", FieldType.N, (short)4, (short)0 ) );
    fields.add( new DBFField( "OBJEKTNR", FieldType.N, (short)5, (short)0 ) );
    fields.add( new DBFField( "PAK", FieldType.C, (short)60, (short)0 ) );
    fields.add( new DBFField( "PUNKTNR", FieldType.N, (short)6, (short)0 ) );
    fields.add( new DBFField( "SL_GEW_KNZ", FieldType.N, (short)15, (short)0 ) );
    fields.add( new DBFField( "SL_GEW_NAM", FieldType.C, (short)254, (short)0 ) );

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
    final EwawiLengthStructure[] structures = eplIndex.getLengthStructures();
    for( final EwawiLengthStructure structure : structures )
      writeStructure( shapeFile, structure );
  }

  private void writeStructure( final ShapeFile shapeFile, final EwawiLengthStructure structure ) throws DBaseException, IOException, SHPException
  {
    final SHPPolyLinez shape = structure.getShape();
    final Object[] values = getValues( structure, shape );

    shapeFile.addFeature( shape, values );
  }

  private Object[] getValues( final EwawiLengthStructure structure, final SHPPolyLinez shape ) throws DBaseException
  {
    final String comment = structure.getComment();
    final int objectArt = structure.getObjektArt().getKey();
    final Short objektNummer = structure.getObjektNummer();
    final String pak = "";
    final Short punktNummer = 0;
    final Long gewKennzahl = structure.getGewKennzahl();
    final SHP2JTS shp2jts = new SHP2JTS( new GeometryFactory() );
    final Geometry geometry = shp2jts.transform( shape );
    final String gewName = (String)getGewShape().getValue( gewKennzahl, GewShape.GN_ACHS_08, geometry );

    final List<Object> values = new ArrayList<>();
    values.add( comment );
    values.add( objectArt );
    values.add( objektNummer );
    values.add( pak );
    values.add( punktNummer );
    values.add( gewKennzahl );
    values.add( gewName );

    return values.toArray( new Object[] {} );
  }
}