package org.kalypso.services.calculation;

import java.io.Serializable;

/**
 * <p>
 * Enthält die aktuellen Daten eines {@link org.kalypso.services.calculation.ICalcJob}
 * </p>
 * <p>
 * Sollte die JavaBean Spezifikation erfüllen
 * </p>
 * 
 * @serviceComplextype 
 * @author Belger
 */
public class CalcJobBean implements Serializable
{
  public final static int UNKNOWN = -1;
  public final static int RUNNING = 0;
  public final static int FINISHED = 1;
  public final static int CANCELED = 2;
  public final static int WAITING = 3;
  public final static int ERROR = 4;
  
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

  private CalcJobResultBean[] m_results;
  
  public CalcJobBean()
  {
    this( "-1", "UNKNOWN", "", UNKNOWN, 0, null );
  }

  public CalcJobBean( final String idParm, final String descriptionParm,
      final String typeParm, final int stateParm, final int progressParm, final CalcJobResultBean[] results )
  {
    this.m_id = idParm;
    this.m_description = descriptionParm;
    this.m_type = typeParm;
    this.m_state = stateParm;
    this.m_progress = progressParm;
    this.m_results = results;
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

  public String getType()
  {
    return m_type;
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

  public void setType( String string )
  {
    m_type = string;
  }

  public void setMessage( final String message )
  {
    m_message = message;
  }

  public final CalcJobResultBean[] getResults()
  {
    return m_results;
  }
  public final void setResults( CalcJobResultBean[] results )
  {
    m_results = results;
  }
}