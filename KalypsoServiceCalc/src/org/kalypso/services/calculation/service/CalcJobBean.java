package org.kalypso.services.calculation.service;

import java.io.Serializable;

import org.kalypso.services.calculation.common.ICalcJobInfo;

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
public class CalcJobBean implements Serializable, ICalcJobInfo
{
  /** ID des beschriebenen Jobs*/
  private String m_id;

  /** Typ des Jobs */
  private String m_type;

  /** Textuelle Beschreibung des Jobs, beim Erzeugen des Jobs an den Service übergeben */
  private String m_description;

  /** Status des Jobs */
  private int m_state;

  /** Fortschritt des Jobs, ziwschen 0 und 100, -1 bedeutet: unbekannt */
  private int m_progress;

  /** Beschreibung des Job-Zustandes, falls Status der Fehlerstatus: die Fehlermeldung */
  private String m_message = "";

  /** Die vom Client übergebenen/erwarteten Input-Dateien */
  private CalcJobDataBean[] m_inputData;
  
  /** aktuelle Ergebnisse */
  private CalcJobDataBean[] m_results;
  
  public CalcJobBean()
  {
    this( "-1", "UNKNOWN", "", UNKNOWN, 0, null );
  }
  
  public CalcJobBean( final String idParm, final String descriptionParm,
      final String typeParm, final int stateParm, final int progressParm, final CalcJobDataBean[] results )
  {
    this.m_id = idParm;
    this.m_description = descriptionParm;
    this.m_type = typeParm;
    this.m_state = stateParm;
    this.m_progress = progressParm;
    this.m_results = results;
  }
  
  /**
   * @see org.kalypso.services.calculation.common.ICalcJobInfo#getDescription()
   */
  public String getDescription()
  {
    return m_description;
  }

  /**
   * @see org.kalypso.services.calculation.common.ICalcJobInfo#getId()
   */
  public String getId()
  {
    return m_id;
  }

  /**
   * @see org.kalypso.services.calculation.common.ICalcJobInfo#getProgress()
   */
  public int getProgress()
  {
    return m_progress;
  }

  /**
   * @see org.kalypso.services.calculation.common.ICalcJobInfo#getState()
   */
  public int getState()
  {
    return m_state;
  }

  /**
   * @see org.kalypso.services.calculation.common.ICalcJobInfo#getType()
   */
  public String getType()
  {
    return m_type;
  }
  
  /**
   * @see org.kalypso.services.calculation.common.ICalcJobInfo#getMessage()
   */
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

  public void setType( String string )
  {
    m_type = string;
  }

  public void setMessage( final String message )
  {
    m_message = message;
  }

  /**
   * @see org.kalypso.services.calculation.common.ICalcJobInfo#getResults()
   */
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
}