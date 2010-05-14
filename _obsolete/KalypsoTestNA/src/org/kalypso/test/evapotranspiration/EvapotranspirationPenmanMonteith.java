package org.kalypso.test.evapotranspiration;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.xml.bind.JAXBException;

import junit.framework.TestCase;

import org.kalypso.commons.factory.FactoryException;
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

/*
 * 
 * Penman-Monteith-Formel (Berechnung nach FAO-Standard):
 * 
 *  Et0 = g(T,v) * (Rn/L + f(T) * Deltat * v_2 (1-U/100))   (Gl. 6.34, S. 54, DVWK 238)
 *           
 *           
 *  f(T) = es(T)/s * (gamma * 3,75)/(T + 273) (Gl. 6.35)
 *           
 *                        s
 *  g(T,v) = ----------------------------- (Gl. 6.36)
 *            s + gamma * (1 + 0.34 * v_2)  
 * ---------------------------------------------------------------------------------------------                              
 *  Rn:     STRAHLUNGSBILANZ (NETTOSTRAHLUNG), [J/cm**2]
 *              Rn = (1 - alpha) * RG - sigma * Tabs^4 * (0.1 + 0.9 * S/S0) * (0.34 - 0.044 * Wurzel(e))
 *  alpha:  Albedo für Gras = 0.23
 *  sigma:  Stefan-Bolzman-Konstante = 0.49 * 10^-6 = 0.00000049 [J/(cm^2 * K^4)]
 *  R0:     Extraterrestrische Strahlung [J/cm^2] (Gl. 9.19)
 *              R0 = 245 * [9.9 + 7.08 * sin(phi) + 0.18 * (Breite-51) * (sin(phi)-1)]
 *              phi = 0.0172 * JT - 1.39
 *  JT:     fortlaufende Tageszählung (1-365/366) 
 *  RG:     Globalstrahlung [J/cm^2]
 *              RG = =R0 * (0.19 + 0.55 * S/S0)
 *              
 *  S0:     Astronomisch mögliche Sonnenscheindauer [h]
 *              S0 = 12.3 + sin(phi) * [4.3 + (Breite - 51)/6]
 *              phi = 0.0172 * JT - 1.39
 * ---------------------------------------------------------------------------------------------                              
 *  L:      spezielle Verdunstungswärme [(J/mm^2)/mm] (Gl. 9.27)
 *              L = 249.8 - 0.242 * T
 *  v_2:    WINDGESCHWINDIGKEIT 2 m UEBER GRUND [m/s]
 *              V_2 = V_z * (4.2/(ln z + 3.5))
 *  z:      Messhöhe [m]
 *  s:      Steigung der Sättigungsdampfdruckkurve (Tafel 9.3 oder Gl. 9.4)
 *              s = es(T) * [4284/(243.12 + T)^2] über Wasser
 *              s = es(T) * [6123/(272.62 + T)^2] über Eis
 *  gamma : Psychrometerkonstante = 0,655 hPa/K  für T >  0
 *                                = 0,576 hPa/K  für T <= 0
 *  es(T):  Sättigungdampfdruck [hPa], abh. von der Temperatur (Tafel 9.3 oder Gl. 9.1)
 *              es(T<= 0,01) = 6.11 * EXP ((22.46 * T) / (272.62 + T))
 *              es(T > 0,01) = 6.11 * EXP ((17.62 * T) / (243.12 + T)
 *  e:      Dampfdruck der Luft [hPa] (Gl. 9.1)
 *              e = es(T) * (U/100)
 *  U:      relative Luftfeuchte [%] 
 */

public class EvapotranspirationPenmanMonteith extends TestCase
{

  double m_latitude = 53.64;// HH-Fuhlsbuettel 53.64

  double m_hoehe = 2.0; // Fuhlsbüttel 11.0m

  public void testCalcEvapotranspirationPenmanMonteith( ) throws SensorException, FactoryException, JAXBException, IOException
  {
    final File zmlClimateFile = new File( "C:\\TMP\\eva\\input\\Climate.zml" );
    final IObservation obsClimate = ZmlFactory.parseXML( zmlClimateFile.toURL(), "" );
    // calculate Penman-Monteith
    final IObservation obsEva = calcPenmanMonteith( obsClimate );
    final Observation observation = ZmlFactory.createXML( obsEva, null );
    final FileOutputStream outs = new FileOutputStream( new File( "C:\\TMP\\eva\\out\\PenmanMonteith.zml" ) );
    ZmlFactory.getMarshaller().marshal( observation, outs );
    outs.close();

  }

  private IObservation calcPenmanMonteith( final IObservation inputClimateObs ) throws SensorException
  {
    final List<Date> dateCollector = new ArrayList<Date>();
    final List<Double> valueCollector = new ArrayList<Double>();
    final ITuppleModel valuesClimate = inputClimateObs.getValues( null );
    final IAxis dateAxis = ObservationUtilities.findAxisByType( inputClimateObs.getAxisList(), TimeserieConstants.TYPE_DATE );
    final IAxis tAxis = ObservationUtilities.findAxisByType( inputClimateObs.getAxisList(), TimeserieConstants.TYPE_TEMPERATURE );
    final IAxis uAxis = ObservationUtilities.findAxisByType( inputClimateObs.getAxisList(), TimeserieConstants.TYPE_HUMIDITY );
    final IAxis sunAxis = ObservationUtilities.findAxisByType( inputClimateObs.getAxisList(), TimeserieConstants.TYPE_HOURS );
    final IAxis windAxis = ObservationUtilities.findAxisByType( inputClimateObs.getAxisList(), TimeserieConstants.TYPE_VELOCITY );

    for( int i = 0; i < valuesClimate.getCount(); i++ )
    {
      final Date date = (Date) valuesClimate.getElement( i, dateAxis );
      dateCollector.add( date );
      final Double valueT = (Double) valuesClimate.getElement( i, tAxis );
      final Double valueU = (Double) valuesClimate.getElement( i, uAxis );
      final Double valueSun = (Double) valuesClimate.getElement( i, sunAxis );
      final Double valueWind = (Double) valuesClimate.getElement( i, windAxis );
      Double Es;
      Double s;
      // Berechnung des Sättigungddampfdruckes
      if( valueT < 0.0 )
      {
        Es = 6.11 * Math.exp( (22.46 * valueT) / (272.62 + valueT) );
        s = Es * (6123d / Math.pow( (272.62 + valueT), 2 ));
      }
      else
      {
        Es = 6.11 * Math.exp( (17.62 * valueT) / (243.12 + valueT) );
        s = Es * (4284d / Math.pow( (243.12 + valueT), 2 ));
      }
      final Double gamma = getGamma( valueT );
      final Double f = (Es / s) * ((gamma * 3.75) / (valueT + 273d));
      final Double v2 = valueWind * (4.2 / (Math.log( m_hoehe ) + 3.5));
      final Double g = s / (s + gamma * (1d + 0.34 * v2));
      final Double L = 249.8 - 0.242 * valueT;
      final int JT = getJT( date );
      final Double chi = 0.0172 * JT - 1.39;
      final Double S0 = 12.3 + Math.sin( chi) * (4.3 + (m_latitude - 51d) / 6d); //Math.sin erfordert rad - chi ist in rad
      final Double R0 = 245d * (9.9 + 7.08 * Math.sin( chi ) + 0.18 * (m_latitude - 51d) * (Math.sin( chi ) - 1d));
      final Double RG = R0 * (0.19 + 0.55 * valueSun / S0);
      Double Rn = (1d - 0.23) * RG - 0.00000049 * Math.pow( (273d + valueT), 4d ) * (0.1 + 0.9 * valueSun / S0) * (0.34 - 0.044 * Math.sqrt( Es * valueU / 100d ));
      if( Rn < 0 )
        Rn = 0.0;

      final Double valueET = g * (Rn / L + f * 24d * v2 * (1 - valueU / 100));
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

  private int getJT( final Date inputDate )
  {// Tage seit Jahresbeginn
    final Date startDate = new Date();
    startDate.setTime( inputDate.getTime() );
    startDate.setMonth( 0 );
    startDate.setDate( 1 );
    // zeit seit Jahresbeginn in Millisekunden
    final long diffTime = (inputDate.getTime() - startDate.getTime());
    final int days = (int) (diffTime / (1000 * 60 * 60 * 24) + 1);
    return days;
  }

  private double getGamma( final Double valueT )
  {
    // gamma: Psychrometerkonstante
    Double gamma;
    if( valueT > 0 )
      gamma = 0.655;
    else
      gamma = 0.576;
    return gamma;
  }

  private IAxis[] createAxis( )
  {
    final IAxis dateAxis = new DefaultAxis( TimeserieUtils.getName( TimeserieConstants.TYPE_DATE ), TimeserieConstants.TYPE_DATE, TimeserieUtils.getUnit( TimeserieConstants.TYPE_DATE ), Date.class, true );
    final IAxis valueEAxis = new DefaultAxis( TimeserieUtils.getName( TimeserieConstants.TYPE_EVAPORATION ), TimeserieConstants.TYPE_EVAPORATION, TimeserieUtils.getUnit( TimeserieConstants.TYPE_EVAPORATION ), Double.class, false );
    final IAxis[] axis = new IAxis[] { dateAxis, valueEAxis };
    return axis;
  }

}
