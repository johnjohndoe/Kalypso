package org.kalypso.services.calcjob;

import java.io.Serializable;

/**
 * <p>
 * Beschreibung eines Jobs.
 * </p>
 * <p>
 * Sollte die JavaBean Spezifikation erfüllen
 * </p>
 * 
 * @serviceComplextype 
 * @author Belger
 */
public class CalcJobDescription implements Serializable
{
  private String m_id;

  private String m_description;

  private String m_type;

  private int m_state;

  private int m_progress;

  private String m_message = "";

  public CalcJobDescription()
  {
    this( "-1", "UNKNOWN", "", CalcJobStatus.UNKNOWN, 0 );
  }

  public CalcJobDescription( final String idParm, final String descriptionParm,
      final String typeParm, final int stateParm, final int progressParm )
  {
    this.m_id = idParm;
    this.m_description = descriptionParm;
    this.m_type = typeParm;
    this.m_state = stateParm;
    this.m_progress = progressParm;
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

}