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
package org.kalypso.model.wspm.tuhh.ui.imports.ewawi;

import org.kalypso.model.wspm.ewawi.data.EwawiProLine;
import org.kalypso.model.wspm.ewawi.data.EwawiSta;
import org.kalypso.model.wspm.ewawi.data.EwawiStaLine;
import org.kalypso.model.wspm.ewawi.data.enums.EwawiPunktart;
import org.kalypso.model.wspm.ewawi.utils.EwawiException;
import org.kalypso.model.wspm.ewawi.utils.profiles.EwawiProfilePoint;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;

/**
 * @author Holger Albert
 */
public class EwawiUtilities
{
  private EwawiUtilities( )
  {
  }

  public static EwawiProfilePoint createProfilePoint( final EwawiSta staIndex, final EwawiProLine proLine ) throws EwawiException
  {
    final EwawiStaLine leftFixPoint = staIndex.findFixPoint( proLine.getObjectArt(), EwawiPunktart._1, proLine.getGewKennzahl(), proLine.getStation() );
    final EwawiStaLine rightFixPoint = staIndex.findFixPoint( proLine.getObjectArt(), EwawiPunktart._2, proLine.getGewKennzahl(), proLine.getStation() );
    if( leftFixPoint == null || rightFixPoint == null )
      throw new EwawiException( Messages.getString( "EwawiProfilePointCreator.1" ) ); //$NON-NLS-1$

    return new EwawiProfilePoint( leftFixPoint, rightFixPoint, proLine );
  }

  public static String getRecordDescription( final EwawiProLine proLine )
  {
    final StringBuilder description = new StringBuilder();

    final String comment = proLine.getComment();
    if( comment != null && !comment.equals( "-" ) ) //$NON-NLS-1$
    {
      final String commentText = String.format( "%s", comment ); //$NON-NLS-1$
      description.append( commentText );
    }

    return description.toString();
  }

  public static String getRecordCode( final EwawiProLine proLine )
  {
    final EwawiPunktart punktArt = proLine.getPunktArt();
    return String.format( "EWAWI_%d", punktArt.getKey() ); //$NON-NLS-1$
  }
}