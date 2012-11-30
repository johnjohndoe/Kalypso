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

import java.text.DateFormat;
import java.util.Date;

import org.kalypso.model.wspm.ewawi.data.EwawiProLine;
import org.kalypso.model.wspm.ewawi.data.EwawiSta;
import org.kalypso.model.wspm.ewawi.data.EwawiStaLine;
import org.kalypso.model.wspm.ewawi.data.enums.EwawiPunktart;
import org.kalypso.model.wspm.ewawi.utils.EwawiException;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;

/**
 * @author Holger Albert
 */
public class EwawiWaterLevel
{
  private final EwawiSta m_staIndex;

  private final EwawiProLine[] m_proLines;

  public EwawiWaterLevel( final EwawiSta staIndex, final EwawiProLine[] proLines )
  {
    m_staIndex = staIndex;
    m_proLines = proLines;
  }

  public double calculateMeanValue( ) throws EwawiException
  {
    if( m_proLines == null || m_proLines.length == 0 )
      return Double.NaN;

    double heightSum = 0.0;
    for( final EwawiProLine proLine : m_proLines )
    {
      final EwawiStaLine leftFixPoint = m_staIndex.findFixPoint( proLine.getObjectArt(), EwawiPunktart._1, proLine.getGewKennzahl(), proLine.getStation() );
      if( leftFixPoint == null )
        throw new EwawiException( Messages.getString("EwawiWaterLevel.0") ); //$NON-NLS-1$

      final double height = leftFixPoint.getHoehe().doubleValue() + proLine.getHoehe().doubleValue();

      heightSum = heightSum + height;
    }

    final double heightMean = heightSum / m_proLines.length;

    return heightMean;
  }

  public String getComment( ) throws EwawiException
  {
    if( m_proLines == null || m_proLines.length == 0 )
      return ""; //$NON-NLS-1$

    if( m_proLines.length == 1 )
      return getCommentSinglePoint();

    return getCommentMultiplePoints();
  }

  private String getCommentSinglePoint( )
  {
    final Date aufnahmeDatum = m_proLines[0].getAufnahmeDatum();
    if( aufnahmeDatum == null )
      return ""; //$NON-NLS-1$

    final DateFormat df = DateFormat.getDateInstance( DateFormat.MEDIUM );
    return String.format( Messages.getString("EwawiWaterLevel.3"), df.format( aufnahmeDatum ) ); //$NON-NLS-1$
  }

  private String getCommentMultiplePoints( ) throws EwawiException
  {
    final StringBuilder comment = new StringBuilder();
    comment.append( String.format( Messages.getString("EwawiWaterLevel.4"), m_proLines.length ) ); //$NON-NLS-1$

    for( int i = 0; i < m_proLines.length; i++ )
    {
      final EwawiProLine proLine = m_proLines[i];

      final EwawiStaLine leftFixPoint = m_staIndex.findFixPoint( proLine.getObjectArt(), EwawiPunktart._1, proLine.getGewKennzahl(), proLine.getStation() );
      if( leftFixPoint == null )
        throw new EwawiException( Messages.getString("EwawiWaterLevel.0") ); //$NON-NLS-1$

      final double height = leftFixPoint.getHoehe().doubleValue() + proLine.getHoehe().doubleValue();

      comment.append( String.format( "%.2f", height ) ); //$NON-NLS-1$
      if( i < m_proLines.length - 1 )
        comment.append( ", " ); //$NON-NLS-1$
    }

    comment.append( Messages.getString("EwawiWaterLevel.8") ); //$NON-NLS-1$

    final Date aufnahmeDatum = m_proLines[0].getAufnahmeDatum();
    if( aufnahmeDatum != null )
    {
      final DateFormat df = DateFormat.getDateInstance( DateFormat.MEDIUM );
      return String.format( Messages.getString("EwawiWaterLevel.9"), df.format( aufnahmeDatum ) ); //$NON-NLS-1$
    }

    return comment.toString();
  }
}