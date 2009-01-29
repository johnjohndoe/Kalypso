/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.sensor.status;

import java.util.logging.Level;

import org.kalypso.contribs.java.util.logging.ILogger;
import org.kalypso.contribs.java.util.logging.LoggerUtilities;
import org.kalypso.core.i18n.Messages;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.ObservationConstants;
import org.kalypso.ogc.sensor.SensorException;

/**
 * KalypsoProcolHelper analyses observations' values and produces a kind of protocol.
 * 
 * @author schlienger
 */
public class KalypsoProtocolWriter
{
  private KalypsoProtocolWriter()
  {
  // not to be instanciated
  }

  public static void analyseValues( final IObservation observation, final ITuppleModel model, final ILogger logger )
      throws SensorException
  {
    analyseValues( new IObservation[]
    { observation }, new ITuppleModel[]
    { model }, logger );
  }

  /**
   * Analyses the given tupple models and reports possible errors (according to status of tupples).
   */
  public static void analyseValues( final IObservation[] observations, final ITuppleModel[] models, final ILogger logger )
      throws SensorException
  {
    if( observations.length != models.length )
      throw new IllegalArgumentException( Messages.getString("org.kalypso.ogc.sensor.status.KalypsoProtocolWriter.1") ); //$NON-NLS-1$

    final StringBuffer bf = new StringBuffer();

    for( int i = 0; i < models.length; i++ )
    {
      final ITuppleModel tuppleModel = models[i];
      final IAxis[] statusAxes = KalypsoStatusUtils.findStatusAxes( tuppleModel.getAxisList() );
      final int[] mergedStati = new int[statusAxes.length];
      for( int iAxes = 0; iAxes < mergedStati.length; iAxes++ )
        mergedStati[iAxes] = KalypsoStati.BIT_OK;

      if( statusAxes.length != 0 )
      {
        final IObservation observation = observations[i];
        for( int ix = 0; ix < tuppleModel.getCount(); ix++ )
        {
          // clear reporting buffer
          bf.setLength( 0 );

          for( int iAxes = 0; iAxes < statusAxes.length; iAxes++ )
          {
            final IAxis axis = statusAxes[iAxes];
            final Number nb = (Number)tuppleModel.getElement( ix, axis );
            final int statusValue = nb == null ? 0 : nb.intValue();

            mergedStati[iAxes] = mergedStati[iAxes] | statusValue;
          }
        }

        final MetadataList metadataList = observation.getMetadataList();

        for( int iAxes = 0; iAxes < mergedStati.length; iAxes++ )
        {
          final IAxis axis = statusAxes[iAxes];

          final String obsName = observation.getName();
          final String type = KalypsoStatusUtils.getAxisLabelFor( axis );

          final StringBuffer sb = new StringBuffer( "Zeitreihe " );
          sb.append( obsName );

          final String desc = metadataList.getProperty( ObservationConstants.MD_DESCRIPTION, "" ); //$NON-NLS-1$
          if( desc.length() > 0 )
          {
            // desc += " aus " + observations[i].getMetadataList().getProperty( ObservationConstants.MD_ORIGIN,
            // "<unbekannt>" );
            sb.append( " (" ); //$NON-NLS-1$
            sb.append( desc ); 
            sb.append( ")" );//$NON-NLS-1$
          }

          sb.append( ", " );//$NON-NLS-1$
          sb.append( type );
          sb.append( ", " );//$NON-NLS-1$

          final String message = sb.toString();

          if( axis.isPersistable() && KalypsoStatusUtils.checkMask( mergedStati[iAxes], KalypsoStati.BIT_CHECK ) )
            logger.log( Level.FINE, LoggerUtilities.CODE_SHOW_DETAILS, message + " Werte müssen geprüft werden" );
          else if( !axis.isPersistable()
              && KalypsoStatusUtils.checkMask( mergedStati[iAxes], KalypsoStati.BIT_DERIVATION_ERROR ) )
            logger.log( Level.FINE, LoggerUtilities.CODE_SHOW_DETAILS, message + " Fehler beim Ableiten der Werte" );
        }

      }
    }
  }
}