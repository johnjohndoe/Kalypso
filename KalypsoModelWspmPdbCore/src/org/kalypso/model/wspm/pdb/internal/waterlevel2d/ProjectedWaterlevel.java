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
package org.kalypso.model.wspm.pdb.internal.waterlevel2d;

import java.math.BigDecimal;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.hibernatespatial.mgeom.MCoordinate;
import org.hibernatespatial.mgeom.MGeomUtils;
import org.hibernatespatial.mgeom.MLineString;
import org.kalypso.model.wspm.core.profil.IProfileObjectRecord;
import org.kalypso.model.wspm.core.profil.IProfileObjectRecords;
import org.kalypso.model.wspm.pdb.db.mapping.Point;
import org.kalypso.model.wspm.pdb.db.mapping.WaterlevelFixation;
import org.kalypso.model.wspm.pdb.db.utils.PdbMappingUtils;
import org.kalypso.model.wspm.pdb.gaf.GafPointCode;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCorePlugin;
import org.kalypso.transformation.transformer.JTSTransformer;
import org.opengis.geometry.MismatchedDimensionException;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.linearref.LengthIndexedLine;

/**
 * Helper class that projects a waterlevel fixation to a prfoile and provides acces to its attributes.
 * 
 * @author Gernot Belger
 */
class ProjectedWaterlevel
{
  private final MLineString m_profileLine;

  private final WaterlevelFixation m_waterlevel;

  public ProjectedWaterlevel( final MLineString profileLine, final WaterlevelFixation waterlevel )
  {
    m_profileLine = profileLine;
    m_waterlevel = waterlevel;
  }

  public IStatus createOriginalRecord( final IProfileObjectRecords records ) throws MismatchedDimensionException, FactoryException, TransformException
  {
    /* Fetch data from waterlevel */
    final com.vividsolutions.jts.geom.Point waterlevelPoint = getWaterlevelLocationInProfileSrs();

    /* skip points without location, we cannot project them to the profile */
    if( waterlevelPoint == null )
      return Status.OK_STATUS;

    final Coordinate waterlevelLocation = waterlevelPoint.getCoordinate();

    final String description = m_waterlevel.getDescription();

    /* create record and add values */
    final IProfileObjectRecord record = records.addNewRecord();

    record.setComment( description );

    /* keep original location of waterlevel, not the projection on the section, which can be computed by width */
    record.setRechtswert( waterlevelLocation.x );
    record.setHochwert( waterlevelLocation.y );

    record.setCode( GafPointCode.WS.getKey() );

    record.setHoehe( m_waterlevel.getWaterlevel().doubleValue() );

    final double distance = m_profileLine.distance( waterlevelPoint );
    final double maxDistance = 1.0; // [m]
    if( distance > maxDistance )
    {
      final BigDecimal station = m_waterlevel.getStation();
      final String message = String.format( "Waterlevel with station %s: big distance to corresponding profile line: %d [m]", station, distance );
      return new Status( IStatus.WARNING, WspmPdbCorePlugin.PLUGIN_ID, message );
    }

    final BigDecimal width = calculateWidth( waterlevelLocation );
    // FIXME: why Doubles in record??
    record.setBreite( width.doubleValue() );

    return Status.OK_STATUS;
  }

  private com.vividsolutions.jts.geom.Point getWaterlevelLocationInProfileSrs( ) throws FactoryException, MismatchedDimensionException, TransformException
  {
    final com.vividsolutions.jts.geom.Point location = m_waterlevel.getLocation();
    if( location == null )
      return null;

    final Coordinate coordinate = location.getCoordinate();

    final int wSRID = location.getSRID();
    final int targetSRID = m_profileLine.getSRID();

    final JTSTransformer transformer = new JTSTransformer( wSRID, targetSRID );
    final Coordinate transformed = transformer.transform( coordinate );

    return m_profileLine.getFactory().createPoint( transformed );
  }

  private BigDecimal calculateWidth( final Coordinate waterlevelLocation )
  {
    /* calculate width and location on profile line */
    final LengthIndexedLine index = new LengthIndexedLine( m_profileLine );
    final double projectedIndex = index.project( waterlevelLocation );
    final MCoordinate projectedLocationWithM = MGeomUtils.extractPoint( m_profileLine, projectedIndex );

    if( Double.isNaN( projectedLocationWithM.m ) )
      return null;

    final BigDecimal mWidth = new BigDecimal( projectedLocationWithM.m );

    final int scale = PdbMappingUtils.findScale( Point.class, Point.PROPERTY_WIDTH );
    return mWidth.setScale( scale, BigDecimal.ROUND_HALF_UP );
  }

  public BigDecimal getDischarge( )
  {
    return m_waterlevel.getDischarge();
  }

  public String getDescription( )
  {
    return m_waterlevel.getDescription();
  }

  public double getWidth( ) throws MismatchedDimensionException, FactoryException, TransformException
  {
    /* Fetch data from waterlevel */
    final com.vividsolutions.jts.geom.Point waterlevelPoint = getWaterlevelLocationInProfileSrs();

    /* skip points without location, we cannot project them to the profile */
    if( waterlevelPoint == null )
      return Double.NaN;

    final Coordinate waterlevelLocation = waterlevelPoint.getCoordinate();

    final BigDecimal width = calculateWidth( waterlevelLocation );
    if( width == null )
      return Double.NaN;

    return width.doubleValue();
  }
}