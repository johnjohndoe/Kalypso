package org.kalypso.services.calculation.service;

import java.io.Serializable;

/**
 * <p>
 * Enthält die aktuellen Daten eines {@link org.kalypso.services.calculation.job.ICalcJob}
 * </p>
 * <p>
 * Sollte die JavaBean Spezifikation erfüllen
 * </p>
 * 
 * @author Belger
 */
public class CalcJobBean implements Serializable
{
  /** ID des beschriebenen Jobs */
  private String m_id = null;

  /** Textuelle Beschreibung des Jobs, beim Erzeugen des Jobs an den Service übergeben */
  private String m_description = null;
  
  /** Der Berechnungstyp des Jobs */
  private String m_type = null;

  /** Status des Jobs */
  private int m_state = ICalculationService.UNKNOWN;

  /** Fortschritt des Jobs, ziwschen 0 und 100, -1 bedeutet: unbekannt */
  private int m_progress = -1;

  /** Beschreibung des Job-Zustandes, falls Status der Fehlerstatus: die Fehlermeldung */
  private String m_message = null;

  /** Die vom Client übergebenen/erwarteten Input-Dateien */
  private CalcJobDataBean[] m_inputData = null;
  
  /** aktuelle Ergebnisse */
  private CalcJobDataBean[] m_results = null;
  
  /** Die Basis-URL, an welcher die temporären Daten stehen */
  private String m_baseURL = null;
  
  public CalcJobBean()
  {
  // empty 
  }
  
  public CalcJobBean( final String id, final String description, final String type, final int state, final int progress, final String baseURL, final CalcJobDataBean[] results )
  {
    m_id = id;
    m_description = description;
    m_state = state;
    m_progress = progress;
    m_results = results;
    m_type = type;
    m_baseURL = baseURL;
  }
  
  public String getDescription()
  {
    return m_description;
  }

  public String getId()
  {
    return m_id;
  }

  public int getProgress()
  {
    return m_progress;
  }

  public int getState()
  {
    return m_state;
  }

  public String getMessage()
  {
    return m_message;
  }

  public void setDescription( String string )
  {
    m_description = string;
  }

  public void setId( String string )
  {
    m_id = string;
  }

  public void setProgress( int i )
  {
    m_progress = i;
  }

  public void setState( int i )
  {
    m_state = i;
  }

  public void setMessage( final String message )
  {
    m_message = message;
  }

  public final CalcJobDataBean[] getResults()
  {
    return m_results;
  }
  
  public final void setResults( CalcJobDataBean[] results )
  {
    m_results = results;
  }
  
  public final CalcJobDataBean[] getInputData()
  {
    return m_inputData;
  }

  public final void setInputData( final CalcJobDataBean[] inputData )
  {
    m_inputData = inputData;
  }
  
  public final String getType()
  {
    return m_type;
  }
  public final void setType( String type )
  {
    m_type = type;
  }
  public final String getBaseURL()
  {
    return m_baseURL;
  }
  public final void setBaseURL( String baseURL )
  {
    m_baseURL = baseURL;
  }
}