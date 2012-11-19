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
import org.kalypso.commons.java.nio.file.PathUtilities;
import org.kalypso.model.wspm.ewawi.data.EwawiPlus;
import org.kalypso.model.wspm.ewawi.data.EwawiPro;
import org.kalypso.model.wspm.ewawi.data.EwawiProLine;
import org.kalypso.model.wspm.ewawi.data.EwawiSta;
import org.kalypso.model.wspm.ewawi.data.EwawiStaLine;
import org.kalypso.model.wspm.ewawi.data.enums.EwawiClass;
import org.kalypso.model.wspm.ewawi.data.enums.EwawiHorizont;
import org.kalypso.model.wspm.ewawi.data.enums.EwawiPunktart;
import org.kalypso.model.wspm.ewawi.data.enums.EwawiUeberlang;
import org.kalypso.model.wspm.ewawi.shape.writer.log.XyzEwawiLogger;
import org.kalypso.model.wspm.ewawi.utils.EwawiException;
import org.kalypso.model.wspm.ewawi.utils.EwawiKey;
import org.kalypso.model.wspm.ewawi.utils.GewShape;
import org.kalypso.model.wspm.ewawi.utils.GewWidthShape;
import org.kalypso.model.wspm.ewawi.utils.profiles.EwawiProfile;
import org.kalypso.model.wspm.ewawi.utils.profiles.EwawiProfilePart;
import org.kalypso.model.wspm.ewawi.utils.profiles.EwawiProfilePoint;
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
import com.vividsolutions.jts.geom.Point;

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

  public EwawiShape244Writer( final EwawiPlus[] data, final GewShape gewShape, final GewWidthShape gewWidthShape )
  {
    super( data, gewShape, gewWidthShape, "404_GIS", ShapeType.POLYLINEZ );

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
  protected void writeData( final ShapeFile shapeFile, final EwawiPlus[] data, final XyzEwawiLogger logger ) throws DBaseException, IOException, SHPException, EwawiException
  {
    for( final EwawiPlus ewawiData : data )
      writeData( shapeFile, ewawiData, logger );
  }

  private void writeData( final ShapeFile shapeFile, final EwawiPlus data, final XyzEwawiLogger logger ) throws DBaseException, IOException, SHPException, EwawiException
  {
    final EwawiPro proIndex = data.getProIndex();
    final EwawiProfile[] profiles = proIndex.getProfiles();
    for( final EwawiProfile profile : profiles )
      writeProfile( shapeFile, profile, data, logger );
  }

  private void writeProfile( final ShapeFile shapeFile, final EwawiProfile profile, final EwawiPlus data, final XyzEwawiLogger logger ) throws DBaseException, IOException, SHPException, EwawiException
  {
    final EwawiSta staIndex = data.getStaIndex();

    final EwawiProfilePart[] parts = profile.getParts();
    for( final EwawiProfilePart part : parts )
    {
      final SHPPolyLinez shape = part.getShape( staIndex );
      final Object[] values = getValues( part, data, shape, logger );

      shapeFile.addFeature( shape, values );
    }
  }

  private Object[] getValues( final EwawiProfilePart part, final EwawiPlus data, final SHPPolyLinez shape, final XyzEwawiLogger logger ) throws DBaseException, EwawiException
  {
    final EwawiKey key = data.getKey();
    final EwawiSta staIndex = data.getStaIndex();
    final SHP2JTS shp2jts = new SHP2JTS( new GeometryFactory() );
    final Geometry geometry = shp2jts.transform( shape );
    final Path relativePlotPath = getRelativePlotPath();

    final int abrechnung = getAbrechnung( part, staIndex, geometry, logger );
    final String alias = key.getAlias();
    final String comment = part.getComment( staIndex );
    final BigDecimal station = part.getStation();
    final Integer horizont = part.getHorizont();
    final int idProfil = ++m_countParts;
    final String klasse = String.format( "%d", abrechnung );
    final int objectArt = part.getObjectArt().getKey();
    final String pak = "";
    final String pfad = relativePlotPath.toString();
    final int profilArt = part.getProfilArt( staIndex ).getKey();
    final Short profilNummer = part.getProfilNummer( staIndex );
    final Long gewKennzahl = part.getGewKennzahl();
    final String gewName = (String)getGewShape().getValue( gewKennzahl, GewShape.GN_ACHS_08, geometry, logger );
    final String ueberlang = String.format( "%d", getUeberlang( part, staIndex, logger, geometry.getCentroid() ).getKey() );
    final Short zusatz = part.getZusatz();

    final String profilName = String.format( "%s_%d", alias, profilNummer );
    final String pdf = String.format( "%s_%s_%s_VP_BJG_QPPLOT_%d.pdf", key.getPe(), key.getAlias(), key.getModelId(), profilNummer );
    final String link = Paths.get( pfad, pdf ).toString();

    checkPlotLink( link, logger, geometry.getCentroid() );

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

  private Path getRelativePlotPath( )
  {
    /* Build the path to the target directory. */
    final File targetFile = getTargetFile();
    final String targetPath = FilenameUtils.getFullPath( targetFile.getAbsolutePath() );
    final Path absoluteTargetPath = Paths.get( targetPath );

    /* Build the path to the plot directory. */
    final String plotPath = FilenameUtils.normalize( String.format( "%s../%s", targetPath, "844_Plot/" ) );

    /* Find the path until the "BY" segment. */
    final Path byPath = PathUtilities.findPathToSegment( absoluteTargetPath, "BY" );
    if( byPath == null )
    {
      /* If it is not found make a relative path to the target path. */
      final Path absolutePlotPath = Paths.get( plotPath );
      return absoluteTargetPath.relativize( absolutePlotPath );
    }

    final Path absolutePlotPath = Paths.get( plotPath );
    return byPath.getParent().relativize( absolutePlotPath );
  }

  private int getAbrechnung( final EwawiProfilePart part, final EwawiSta staIndex, final Geometry geometry, final XyzEwawiLogger logger ) throws DBaseException
  {
    /* Find the ewawi class. */
    final String riverWidth = getRiverWidth( part, geometry, logger );
    final EwawiClass ewawiClass = EwawiClass.findEwawiClass( riverWidth );
    if( ewawiClass == null )
    {
      final String message = String.format( "Keine Kategorie gefunden für Gewässerabschnitt '%d' am Profil '%f'.", part.getGewKennzahl().longValue(), part.getStation().doubleValue() );
      System.out.println( message );

      if( logger != null )
      {
        final Point centroid = geometry.getCentroid();
        logger.logXyzLine( centroid.getX(), centroid.getY(), -9999.0, message, "", -9999.0 );
      }

      return 0;
    }

    /* Calculate the river width. */
    final EwawiProLine[] bukPoints = part.findPoints( EwawiPunktart._8 );
    if( bukPoints.length < 2 )
    {
      /* This may be a horizon, horizons have no buks. */
      final Integer horizont = part.getHorizont();
      if( horizont.intValue() == EwawiHorizont._0.getKey() )
      {
        // TODO This is a profile...
        final String message = String.format( "Konnte Kategorie '%s' für Gewässerabschnitt '%d' am Profil '%f' nicht verifizieren, da zu wenige BUKn.", ewawiClass.getLabel(), part.getGewKennzahl().longValue(), part.getStation().doubleValue() );
        System.out.println( message );

        if( logger != null )
        {
          final Point centroid = geometry.getCentroid();
          logger.logXyzLine( centroid.getX(), centroid.getY(), -9999.0, message, "", -9999.0 );
        }
      }

      return ewawiClass.getKey();
    }

    /* Verify class. */
    final EwawiProLine firstBUK = bukPoints[0];
    final EwawiProLine lastBUK = bukPoints[bukPoints.length - 1];

    /* Find the fix points. */
    final EwawiStaLine leftFixPoint = staIndex.findFixPoint( firstBUK.getObjectArt(), EwawiPunktart._1, firstBUK.getGewKennzahl(), firstBUK.getStation() );
    final EwawiStaLine rightFixPoint = staIndex.findFixPoint( firstBUK.getObjectArt(), EwawiPunktart._2, firstBUK.getGewKennzahl(), firstBUK.getStation() );
    if( leftFixPoint == null || rightFixPoint == null )
    {
      final String message = String.format( "Konnte Kategorie '%s' für Gewässerabschnitt '%d' am Profil '%f' nicht verifizieren, einer der Festpunkte nicht gefunden wurde.", ewawiClass.getLabel(), part.getGewKennzahl().longValue(), part.getStation().doubleValue() );
      System.out.println( message );

      if( logger != null )
      {
        final Point centroid = geometry.getCentroid();
        logger.logXyzLine( centroid.getX(), centroid.getY(), -9999.0, message, "", -9999.0 );
      }

      return ewawiClass.getKey();
    }

    /* Create the profile points. */
    final EwawiProfilePoint firstProfilePoint = new EwawiProfilePoint( leftFixPoint, rightFixPoint, firstBUK );
    final EwawiProfilePoint lastProfilePoint = new EwawiProfilePoint( leftFixPoint, rightFixPoint, lastBUK );

    /* Get the widths of the points. */
    final BigDecimal firstWidth = firstProfilePoint.getBreite();
    final BigDecimal lastWidth = lastProfilePoint.getBreite();
    final double width = lastWidth.doubleValue() - firstWidth.doubleValue();
    if( !(ewawiClass.getMin() < width && width <= ewawiClass.getMax()) )
    {
      final String message = String.format( "Kategorie '%s' für Gewässerabschnitt '%d' am Profil '%f' stimmt nicht mit der errechneten Breite überein: %f", ewawiClass.getLabel(), part.getGewKennzahl().longValue(), part.getStation().doubleValue(), width );
      System.out.println( message );

      if( logger != null )
      {
        final Point centroid = geometry.getCentroid();
        logger.logXyzLine( centroid.getX(), centroid.getY(), -9999.0, message, "", -9999.0 );
      }
    }

    return ewawiClass.getKey();
  }

  private String getRiverWidth( final EwawiProfilePart part, final Geometry geometry, final XyzEwawiLogger logger ) throws DBaseException
  {
    final Long gewKennzahl = part.getGewKennzahl();

    final GewWidthShape gewWidthShape = getGewWidthShape();
    final String riverWidth = (String)gewWidthShape.getValue( gewKennzahl, GewWidthShape.BREITE, geometry, logger );

    return riverWidth;
  }

  private EwawiUeberlang getUeberlang( final EwawiProfilePart part, final EwawiSta staIndex, final XyzEwawiLogger logger, final Point centroid ) throws EwawiException
  {
    final boolean ueberlangLeft = checkProfile( part, staIndex, true, logger, centroid );
    final boolean ueberlangRight = checkProfile( part, staIndex, false, logger, centroid );

    if( !ueberlangLeft && !ueberlangRight )
      return EwawiUeberlang._0;

    if( ueberlangLeft && ueberlangRight )
      return EwawiUeberlang._2;

    return EwawiUeberlang._1;
  }

  private boolean checkProfile( final EwawiProfilePart part, final EwawiSta staIndex, final boolean leftSide, final XyzEwawiLogger logger, final Point centroid ) throws EwawiException
  {
    /* Get all points. */
    final EwawiProLine[] allPoints = part.getProLines();

    /* Get bok points. */
    final EwawiProLine[] bokPoints = part.findPoints( EwawiPunktart._9 );
    if( bokPoints.length < 2 )
    {
      /* This may be a horizon, horizons have no boks. */
      final Integer horizont = part.getHorizont();
      if( horizont.intValue() == EwawiHorizont._0.getKey() )
      {
        // TODO This is a profile...
        final String message = String.format( "Kann Überlänge nicht bestimmen. Das Profil '%f' besitzt keine BOKn.", part.getStation().doubleValue() );
        System.out.println( message );

        if( logger != null )
          logger.logXyzLine( centroid.getX(), centroid.getY(), -9999.0, message, "", -9999.0 );

        return false;
      }

      return false;
    }

    /* Check left. */
    if( leftSide )
      return checkProfile( part, staIndex, allPoints[0], bokPoints[0] );

    /* Check right. */
    return checkProfile( part, staIndex, bokPoints[bokPoints.length - 1], allPoints[allPoints.length - 1] );
  }

  private boolean checkProfile( final EwawiProfilePart part, final EwawiSta staIndex, final EwawiProLine leftPoint, final EwawiProLine rightPoint ) throws EwawiException
  {
    /* Find the fix points. */
    final EwawiStaLine leftFixPoint = staIndex.findFixPoint( leftPoint.getObjectArt(), EwawiPunktart._1, leftPoint.getGewKennzahl(), leftPoint.getStation() );
    final EwawiStaLine rightFixPoint = staIndex.findFixPoint( leftPoint.getObjectArt(), EwawiPunktart._2, leftPoint.getGewKennzahl(), leftPoint.getStation() );
    if( leftFixPoint == null || rightFixPoint == null )
      throw new EwawiException( "Einer der Festpunkte wurde nicht gefunden." );

    final EwawiProfilePoint leftProfilePoint = new EwawiProfilePoint( leftFixPoint, rightFixPoint, leftPoint );
    final EwawiProfilePoint rightProfilePoint = new EwawiProfilePoint( leftFixPoint, rightFixPoint, rightPoint );

    final BigDecimal leftBreite = leftProfilePoint.getBreite();
    final BigDecimal rightBreite = rightProfilePoint.getBreite();

    final double difference = rightBreite.doubleValue() - leftBreite.doubleValue();
    if( difference <= 10.0 )
      return false;

    if( !part.hasDykePoints( leftPoint, rightPoint ) )
      return false;

    return true;
  }

  private void checkPlotLink( final String link, final XyzEwawiLogger logger, final Point point )
  {
    /* Build the path to the target directory. */
    final File targetFile = getTargetFile();
    final String targetPath = FilenameUtils.getFullPath( targetFile.getAbsolutePath() );
    final Path absoluteTargetPath = Paths.get( targetPath );

    /* Need to find the absolute plot path. */
    final Path relativePlotPath = Paths.get( link );
    Path absolutePlotPath = null;

    /* Find the path until the "BY" segment. */
    final Path byPath = PathUtilities.findPathToSegment( absoluteTargetPath, "BY" );
    if( byPath == null )
    {
      /* If it is not found resolve against the target path. */
      absolutePlotPath = absoluteTargetPath.resolve( relativePlotPath );
    }
    else
      absolutePlotPath = byPath.getParent().resolve( relativePlotPath );

    final File file = absolutePlotPath.toFile();
    if( !file.exists() )
    {
      final String message = "Die Datei '%s' existiert nicht...";
      System.out.println( String.format( message, file.getPath() ) );

      if( logger != null )
        logger.logXyzLine( point.getX(), point.getY(), -9999.0, message, "", -9999.0 );
    }
  }
}