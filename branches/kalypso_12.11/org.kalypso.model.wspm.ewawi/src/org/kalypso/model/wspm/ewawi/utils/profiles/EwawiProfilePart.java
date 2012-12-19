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
package org.kalypso.model.wspm.ewawi.utils.profiles;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.SortedMap;
import java.util.TreeMap;

import org.kalypso.model.wspm.ewawi.data.EwawiProLine;
import org.kalypso.model.wspm.ewawi.data.EwawiSta;
import org.kalypso.model.wspm.ewawi.data.EwawiStaLine;
import org.kalypso.model.wspm.ewawi.data.enums.EwawiObjectart;
import org.kalypso.model.wspm.ewawi.data.enums.EwawiProfilart;
import org.kalypso.model.wspm.ewawi.data.enums.EwawiPunktart;
import org.kalypso.model.wspm.ewawi.utils.EwawiException;
import org.kalypso.shape.geometry.ISHPMultiPoint;
import org.kalypso.shape.geometry.SHPEnvelope;
import org.kalypso.shape.geometry.SHPGeometryUtils;
import org.kalypso.shape.geometry.SHPMultiPointz;
import org.kalypso.shape.geometry.SHPPoint;
import org.kalypso.shape.geometry.SHPPointz;
import org.kalypso.shape.geometry.SHPPolyLinez;
import org.kalypso.shape.geometry.SHPRange;

/**
 * A EWAWI+ profile part.
 * 
 * @author Holger Albert
 */
public class EwawiProfilePart
{
  private final Integer m_horizont;

  private final SortedMap<Short, EwawiProLine> m_proLines;

  public EwawiProfilePart( final Integer horizont )
  {
    m_horizont = horizont;
    m_proLines = new TreeMap<>();
  }

  public Integer getHorizont( )
  {
    return m_horizont;
  }

  public EwawiProLine[] getProLines( )
  {
    return m_proLines.values().toArray( new EwawiProLine[] {} );
  }

  public SHPPolyLinez getShape( final EwawiSta staIndex ) throws EwawiException
  {
    /* Find the fix points. */
    final EwawiStaLine leftFixPoint = getLeftFixPoint( staIndex );
    final EwawiStaLine rightFixPoint = getRightFixPoint( staIndex );

    /* The profile points. */
    final List<SHPPointz> shpPoints = new ArrayList<>();

    /* Loop all pro lines. */
    for( final EwawiProLine proLine : m_proLines.values() )
    {
      final EwawiProfilePoint proPoint = new EwawiProfilePoint( leftFixPoint, rightFixPoint, proLine );
      final SHPPoint shape = proPoint.getShapeIdealized();

      /* We use the rechtswert of the point to determine m. */
      final double x = shape.getX();
      final double y = shape.getY();
      final double z = proPoint.getHoehe().doubleValue();
      final double m = proPoint.getBreite().doubleValue();

      final SHPPointz shapez = new SHPPointz( x, y, z, m );
      shpPoints.add( shapez );
    }

    /* Create the line geometry. */
    final SHPPointz[] points = shpPoints.toArray( new SHPPointz[] {} );
    final SHPEnvelope envelope = SHPGeometryUtils.createEnvelope( points );
    final SHPRange zrange = SHPGeometryUtils.createZRange( points );
    final SHPRange mrange = SHPGeometryUtils.createMRange( points );
    final ISHPMultiPoint multiPoint = new SHPMultiPointz( envelope, points, zrange, mrange );

    return new SHPPolyLinez( multiPoint, new int[] { 0 } );
  }

  public void addProLine( final EwawiProLine proLine )
  {
    m_proLines.put( proLine.getPunktReihenfolge(), proLine );
  }

  /**
   * This function returns true, if there are dyke points ({@link EwawiPunktart#_13} and {@link EwawiPunktart#_14}) within the given points.
   * 
   * @param leftProLine
   *          The left pro line.
   * @param rightProLine
   *          The right pro line.
   * @return True, if there are dyke points ({@link EwawiPunktart#_13} and {@link EwawiPunktart#_14}) within the given points.
   */
  public boolean hasDykePoints( final EwawiProLine leftProLine, final EwawiProLine rightProLine )
  {
    boolean foundUK = false;
    boolean foundOK = false;

    final EwawiProLine[] proLines = findProLines( leftProLine, rightProLine );
    for( final EwawiProLine proLine : proLines )
    {
      final EwawiPunktart punktArt = proLine.getPunktArt();
      if( punktArt == null )
        continue;

      if( punktArt.equals( EwawiPunktart._13 ) )
        foundUK = true;

      if( punktArt.equals( EwawiPunktart._14 ) )
        foundOK = true;
    }

    if( foundUK && foundOK )
      return true;

    return false;
  }

  /**
   * This function returns all points with the given punkt art. They will be in the order of the profile part (left to right, 1 to n).
   * 
   * @param punktArt
   *          The punkt art to search for.
   * @return All points with the given punkt art. They will be in the order of the profile part (left to right, 1 to n).
   */
  public EwawiProLine[] findPoints( final EwawiPunktart punktArt )
  {
    final List<EwawiProLine> results = new ArrayList<>();

    for( final EwawiProLine proLine : m_proLines.values() )
    {
      final EwawiPunktart punktArt2 = proLine.getPunktArt();
      if( punktArt2 == null )
        continue;

      if( punktArt2.equals( punktArt ) )
        results.add( proLine );
    }

    return results.toArray( new EwawiProLine[] {} );
  }

  // HINT: Every function downward uses the first pro point to retrieve the data.

  public EwawiObjectart getObjectArt( )
  {
    final EwawiProLine[] proLines = getProLines();
    return proLines[0].getObjectArt();
  }

  public Long getGewKennzahl( )
  {
    final EwawiProLine[] proLines = getProLines();
    return proLines[0].getGewKennzahl();
  }

  public BigDecimal getStation( )
  {
    final EwawiProLine[] proLines = getProLines();
    return proLines[0].getStation();
  }

  public Short getZusatz( )
  {
    final EwawiProLine[] proLines = getProLines();
    return proLines[0].getZusatz();
  }

  // HINT: Every function downward uses the left fix point to retrieve the data.

  public EwawiObjectart getObjectArt( final EwawiSta staIndex ) throws EwawiException
  {
    final EwawiStaLine leftFixPoint = getLeftFixPoint( staIndex );
    return leftFixPoint.getObjectArt();
  }

  public Short getZusatz( final EwawiSta staIndex ) throws EwawiException
  {
    final EwawiStaLine leftFixPoint = getLeftFixPoint( staIndex );
    return leftFixPoint.getZusatz();
  }

  public EwawiPunktart getPunktArt( final EwawiSta staIndex ) throws EwawiException
  {
    final EwawiStaLine leftFixPoint = getLeftFixPoint( staIndex );
    return leftFixPoint.getPunktArt();
  }

  public Date getValidity( final EwawiSta staIndex ) throws EwawiException
  {
    final EwawiStaLine leftFixPoint = getLeftFixPoint( staIndex );
    return leftFixPoint.getValidity();
  }

  public String getComment( final EwawiSta staIndex ) throws EwawiException
  {
    final EwawiStaLine leftFixPoint = getLeftFixPoint( staIndex );
    return leftFixPoint.getComment();
  }

  public EwawiProfilart getProfilArt( final EwawiSta staIndex ) throws EwawiException
  {
    final EwawiStaLine leftFixPoint = getLeftFixPoint( staIndex );
    return leftFixPoint.getProfilArt();
  }

  public Short getProfilNummer( final EwawiSta staIndex ) throws EwawiException
  {
    final EwawiStaLine leftFixPoint = getLeftFixPoint( staIndex );
    return leftFixPoint.getProfilNummer();
  }

  public String[] getPhotos( final EwawiSta staIndex ) throws EwawiException
  {
    final EwawiStaLine leftFixPoint = getLeftFixPoint( staIndex );
    return leftFixPoint.getPhotos();
  }

  private EwawiProLine[] findProLines( final EwawiProLine leftProLine, final EwawiProLine rightProLine )
  {
    final List<EwawiProLine> results = new ArrayList<>();

    boolean leftReached = false;
    for( final EwawiProLine proLine : m_proLines.values() )
    {
      if( proLine.equals( leftProLine ) )
        leftReached = true;

      if( !leftReached )
        continue;

      results.add( proLine );

      if( proLine.equals( rightProLine ) )
        break;
    }

    return results.toArray( new EwawiProLine[] {} );
  }

  private EwawiStaLine getLeftFixPoint( final EwawiSta staIndex ) throws EwawiException
  {
    final EwawiProLine[] proLines = getProLines();
    final EwawiProLine proLine = proLines[0];

    final EwawiStaLine leftFixPoint = staIndex.findFixPoint( proLine.getObjectArt(), EwawiPunktart._1, proLine.getGewKennzahl(), proLine.getStation() );
    if( leftFixPoint == null )
      throw new EwawiException( "Der linke Festpunkt wurde nicht gefunden." );

    return leftFixPoint;
  }

  private EwawiStaLine getRightFixPoint( final EwawiSta staIndex ) throws EwawiException
  {
    final EwawiProLine[] proLines = getProLines();
    final EwawiProLine proLine = proLines[0];

    final EwawiStaLine rightFixPoint = staIndex.findFixPoint( proLine.getObjectArt(), EwawiPunktart._2, proLine.getGewKennzahl(), proLine.getStation() );
    if( rightFixPoint == null )
      throw new EwawiException( "Der rechte Festpunkt wurde nicht gefunden." );

    return rightFixPoint;
  }
}