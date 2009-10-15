package org.kalypso.test.evapotranspiration;

/* Haude:
 * ---------------------------------------------------------------------------------------------------------------------
 * ETp(Haude)=f * (es(T)-e)_14 <= 7mm/d (Gl. 6.1, S. 34,DVWK 238)
 * ((es(T)-e)_14 - Sättigungsdefizit der Luft mit Wasserdampf in hPa zum Mittagstermin - 14:30 Uhr MEZ - siehe GL. 9.16)
 * 
 * ETp :    Potentielle Evapotranspiration [mm/d]
 * es(T):   Sättigungdampfdruck [hPa], abh. von der Temperatur (Tafel 9.3 oder Gl. 9.1)
 *              es(T <  0) = 6.11 * EXP ((22.46 * T) / (272.62 + T))
 *              es(T >= 0) = 6.11 * EXP ((17.62 * T) / (243.12 + T)
 * T:       Temperatur [°C] (gemessen)              
 * e:       Aktueller Sättigungsdampfdruck (14:30 Uhr)
 *              e = es(T) * U/100
 * U:       relative Luftfeuchte [%] (gemessen)              
 * f:       Haude-Faktor zur Abhängigkeit der Jahreszeit nach DVWK 238 Tafel 6.1 oder Maniak S. 56
 *  
 */

import java.io.File;
import java.io.FileOutputStream;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import junit.framework.TestCase;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.DefaultAxis;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.impl.SimpleTuppleModel;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.zml.Observation;

public class EvapotranspirationHaude extends TestCase
{

  final Boolean m_useCorr = false;

  public void testCalcEvapotranspirationHaude( ) throws Exception
  {
    final File zmlTUFile = new File( "C:\\TMP\\eva\\input\\Climate.zml" );
    final IObservation obsTU = ZmlFactory.parseXML( zmlTUFile.toURL(), "TU" );
    // calculate haude
    final IObservation obsEva = calcHaude( obsTU );
    final Observation observation = ZmlFactory.createXML( obsEva, null );
    final File outDir = new File( "C:\\TMP\\eva\\out\\Haude.zml" );
    // File outFile = File.createTempFile( "Haude", ".zml", outDir );
    final FileOutputStream outs = new FileOutputStream( outDir );
    ZmlFactory.getMarshaller().marshal( observation, outs );
    outs.close();
  }

  private IAxis[] createAxis( )
  {
    final IAxis dateAxis = new DefaultAxis( TimeserieUtils.getName( TimeserieConstants.TYPE_DATE ), TimeserieConstants.TYPE_DATE, TimeserieUtils.getUnit( TimeserieConstants.TYPE_DATE ), Date.class, true );
    final IAxis valueEAxis = new DefaultAxis( TimeserieUtils.getName( TimeserieConstants.TYPE_EVAPORATION ), TimeserieConstants.TYPE_EVAPORATION, TimeserieUtils.getUnit( TimeserieConstants.TYPE_EVAPORATION ), Double.class, false );
    final IAxis[] axis = new IAxis[] { dateAxis, valueEAxis };
    return axis;
  }

  private SimpleObservation calcHaude( final IObservation inputTUObs ) throws SensorException
  {
    final List<Date> dateCollector = new ArrayList<Date>();
    final List<Double> valueCollector = new ArrayList<Double>();
    final ITuppleModel valuesTU = inputTUObs.getValues( null );
    final IAxis dateTAxis = ObservationUtilities.findAxisByType( inputTUObs.getAxisList(), TimeserieConstants.TYPE_DATE );
    final IAxis tAxis = ObservationUtilities.findAxisByType( inputTUObs.getAxisList(), TimeserieConstants.TYPE_TEMPERATURE );
    final IAxis uAxis = ObservationUtilities.findAxisByType( inputTUObs.getAxisList(), TimeserieConstants.TYPE_HUMIDITY );

    for( int i = 0; i < valuesTU.getCount(); i++ )
    {
      final Date date = (Date) valuesTU.getElement( i, dateTAxis );
      dateCollector.add( date );
      final Double valueT = (Double) valuesTU.getElement( i, tAxis );
      final Double valueU = (Double) valuesTU.getElement( i, uAxis );
      Double valueES;
      // Berechnung des Sättigungddampfdruckes
      if( valueT < 0.0 )
      {
        valueES = 6.11 * Math.exp( (22.46 * valueT) / (272.62 + valueT) );
      }
      else
      {
        valueES = 6.11 * Math.exp( (17.62 * valueT) / (243.12 + valueT) );
      }

      final Double valueE = valueES * valueU / 100d;
      final Double haudeFaktor = getHaudeFaktor( date );
      Double valueET = haudeFaktor * (valueES - valueE);
      if( m_useCorr )
      {
        final double corrFaktor = getCorrFaktor( date );
        valueET = corrFaktor * valueET;
      }
      valueCollector.add( valueET );
    }
    final Object[][] tupelData = new Object[dateCollector.size()][2];
    for( int i = 0; i < dateCollector.size(); i++ )
    {
      tupelData[i][0] = dateCollector.get( i );
      tupelData[i][1] = valueCollector.get( i );
    }
    final IAxis[] axis = createAxis();
    final SimpleTuppleModel evaHaudeTupple = new SimpleTuppleModel( axis, tupelData );
    final MetadataList metaDataList = new MetadataList();
    final SimpleObservation observation = new SimpleObservation( "href", "ID", "titel", false, metaDataList, axis, evaHaudeTupple );
    return observation;
  }

  private double getHaudeFaktor( final Date date )
  {
    // Parameter aus DVWK 238 Tafel 6.1
    final double[] haudeParam = new double[] { 0.22, 0.22, 0.22, 0.29, 0.29, 0.28, 0.26, 0.25, 0.23, 0.22, 0.22, 0.22 };
    final int month = date.getMonth();
    return haudeParam[month];

  }

  private double getCorrFaktor( final Date date )
  {
    // Korrekturwerte zum Umrechnen der Haude Werte in Penman-Monteith Ergebnisse (Station Fuhlsbüttel)
    final double[] corrParam = new double[] { 2.146, 2.388, 2.787, 2.149, 2.172, 2.212, 2.259, 2.173, 2.371, 2.195, 1.983, 2.022 };
    final int month = date.getMonth();
    return corrParam[month];

  }
}
