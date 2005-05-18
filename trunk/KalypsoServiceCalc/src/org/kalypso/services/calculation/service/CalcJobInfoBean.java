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
package org.kalypso.services.calculation.service;

import java.io.Serializable;

import org.kalypso.services.calculation.common.ICalcServiceConstants;
import org.kalypso.services.calculation.job.ICalcMonitor;

/**
 * <p>
 * Enthält die aktuellen Daten eines
 * {@link org.kalypso.services.calculation.job.ICalcJob}
 * </p>
 * 
 * @author Belger
 */
public class CalcJobInfoBean implements Serializable, ICalcMonitor
{
  /** ID des beschriebenen Jobs */
  private String m_id = null;

  /**
   * Textuelle Beschreibung des Jobs, beim Erzeugen des Jobs an den Service
   * übergeben
   */
  private String m_description;

  /** Der Berechnungstyp des Jobs */
  private String m_type;

  /** Status des Jobs */
  private int m_state = ICalcServiceConstants.UNKNOWN;

  /** Fortschritt des Jobs, ziwschen 0 und 100, -1 bedeutet: unbekannt */
  private int m_progress = -1;

  /**
   * Beschreibung des Job-Zustandes, falls Status der Fehlerstatus: die
   * Fehlermeldung
   */
  private String m_message = "Warte auf Ausführung...";

  /**
   * IDs der aktuellen Ergebnisse
   */
  private String[] m_currentResults = new String[0];

  /**
   * Wird vom Client nach erfolgter Berechnung dargestellt. Dient dazu, dem
   * Benutzer ggfls. Hinweise auf Logdateien im Fehlerfall o.ä. zu geben.
   */
  private String m_finishText = "";

  private int m_status = 0; // = IStatus.OK;

  public CalcJobInfoBean()
  {
  // nur für wscompile
  }

  public CalcJobInfoBean( final String id, final String description, final String type,
      final int state, final int progress, final String finishText )
  {
    m_id = id;
    m_description = description;
    m_state = state;
    m_progress = progress;
    m_type = type;
    m_finishText = finishText;
  }

  public String getDescription()
  {
    return m_description;
  }

  public String getId()
  {
    return m_id;
  }

  /**
   * @see org.kalypso.services.calculation.job.ICalcMonitor#getProgress()
   */
  public int getProgress()
  {
    return m_progress;
  }

  public int getState()
  {
    return m_state;
  }

  /**
   * @see org.kalypso.services.calculation.job.ICalcMonitor#getMessage()
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

  /**
   * @see org.kalypso.services.calculation.job.ICalcMonitor#setMessage(java.lang.String)
   */
  public void setMessage( final String message )
  {
    m_message = message;
  }

  public final String[] getCurrentResults()
  {
    return m_currentResults;
  }

  public final void setCurrentResults( String[] currentResults )
  {
    m_currentResults = currentResults;
  }

  public final String getType()
  {
    return m_type;
  }

  public final void setType( String type )
  {
    m_type = type;
  }

  /**
   * @see org.kalypso.services.calculation.job.ICalcMonitor#cancel()
   */
  public void cancel()
  {
    m_state = ICalcServiceConstants.CANCELED;
  }

  /**
   * @see org.kalypso.services.calculation.job.ICalcMonitor#isCanceled()
   */
  public boolean isCanceled()
  {
    return m_state == ICalcServiceConstants.CANCELED;
  }

  public String getFinishText()
  {
    return m_finishText;
  }

  public void setFinishText( String finishText )
  {
    m_finishText = finishText;
  }

  /**
   * @see org.kalypso.services.calculation.job.ICalcMonitor#setFinishInfo(int,
   *      java.lang.String)
   */
  public void setFinishInfo( final int status, final String text )
  {
    setFinishStatus( status );
    setFinishText( text );
  }

  public void setFinishStatus( final int status )
  {
    m_status = status;
  }

  /**
   * @see org.kalypso.services.calculation.job.ICalcMonitor#getFinishStatus()
   */
  public int getFinishStatus()
  {
    return m_status;
  }
}