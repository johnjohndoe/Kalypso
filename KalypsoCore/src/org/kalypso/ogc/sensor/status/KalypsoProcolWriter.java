package org.kalypso.ogc.sensor.status;

import java.io.BufferedWriter;
import java.io.IOException;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationConstants;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;

/**
 * KalypsoProcolHelper analyses observations' values and produces a kind of
 * protocol.
 * 
 * @author schlienger
 */
public class KalypsoProcolWriter
{
  private KalypsoProcolWriter( )
  {
    // not to be instanciated
  }

  /**
   * @see KalypsoProcolWriter#analyseValues(IObservation[], ITuppleModel[],
   *      BufferedWriter, BufferedWriter)
   * 
   * @param observation
   * @param model
   * @param summaryWriter
   * @param detailsWriter
   * @throws SensorException
   */
  public static void analyseValues( final IObservation observation,
      final ITuppleModel model, final BufferedWriter summaryWriter,
      final BufferedWriter detailsWriter ) throws SensorException
  {
    analyseValues( new IObservation[] { observation },
        new ITuppleModel[] { model }, summaryWriter, detailsWriter );
  }

  /**
   * Analyses the given tupple models and reports possible errors (according to
   * status of tupples).
   * 
   * @param observations
   * @param models
   * @param summaryWriter
   * @param detailsWriter
   * @throws SensorException
   */
  public static void analyseValues( final IObservation[] observations,
      final ITuppleModel[] models, final BufferedWriter summaryWriter,
      final BufferedWriter detailsWriter ) throws SensorException
  {
    if( observations.length != models.length )
      throw new IllegalArgumentException( "Arrays not same length" );

    final StringBuffer bf = new StringBuffer();

    try
    {
      for( int i = 0; i < models.length; i++ )
      {
        boolean sumDone = false;

        final IAxis[] statusAxes = KalypsoStatusUtils.findStatusAxes( models[i]
            .getAxisList() );

        if( statusAxes.length != 0 )
        {
          for( int ix = 0; ix < models[i].getCount(); ix++ )
          {
            // clear reporting buffer
            boolean bError = false;
            bf.delete( 0, bf.length() );

            for( int iAxes = 0; iAxes < statusAxes.length; iAxes++ )
            {
              final Number nb = (Number) models[i].getElement( ix,
                  statusAxes[iAxes] );
              if( !KalypsoStatusUtils.checkMask( nb.intValue(),
                  KalypsoStati.BIT_OK ) )
              {
                bError = true;

                bf.append( "["
                    + KalypsoStatusUtils.getAxisLabelFor( statusAxes[iAxes] )
                    + " - " + KalypsoStatusUtils.getTooltipFor( nb.intValue() )
                    + "]\n" );
              }
            }

            // got error at least for one axis?
            if( bError )
            {
              // did already summ up?
              if( !sumDone )
              {
                sumDone = true;
                
                String header = "Warnung in Zeitreihe: "
                    + observations[i].getName();
                
                final String desc = observations[i].getMetadataList().getProperty( ObservationConstants.MD_DESCRIPTION, "" );
                if( desc.length() > 0 )
                  header += " (" + desc + ")";

                summaryWriter.write( header + '\n');

                detailsWriter.newLine();
                detailsWriter.write( header );
                detailsWriter.newLine();
                detailsWriter.write( "Details:" );
                detailsWriter.newLine();
              }

              detailsWriter.write( ObservationUtilities.dump( models[i], "  ", ix, true )
                  + " Grund: " + bf.toString() );
            }
          }
        }
      }
    }
    catch( IOException e )
    {
      e.printStackTrace();
      throw new SensorException( e );
    }
  }
}