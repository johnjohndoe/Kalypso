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
  private int m_state = ICalcServiceConstants.UNKNOWN;

  /** Fortschritt des Jobs, ziwschen 0 und 100, -1 bedeutet: unbekannt */
  private int m_progress = -1;

  /** Beschreibung des Job-Zustandes, falls Status der Fehlerstatus: die Fehlermeldung */
  private String m_message = null;

  /** Die vom Client übergebenen/erwarteten Input-Dateien */
  private CalcJobDataBean[] m_inputData = null;
  
  /** <p>aktuelle Ergebnisse</p>
   * <p>alle Pfade sind relativ zu <m_basedir>/<OUTPUT_DIR_NAME>/ 
   */
  private CalcJobDataBean[] m_results = null;
  
  /** Das Basis-Verzeichnis, welches zum Übertragen der input/output Dateien zur Verfügung steht */
  private String m_basedir = null;
  
  public CalcJobBean()
  {
    // nur für wscompile
  }
  
  public CalcJobBean( final String id, final String description, final String type, final int state, final int progress, final String basedir, final CalcJobDataBean[] results )
  {
    m_id = id;
    m_description = description;
    m_state = state;
    m_progress = progress;
    m_results = results;
    m_type = type;
    m_basedir = basedir;
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
  public final String getBasedir()
  {
    return m_basedir;
  }
  public final void setBasedir( String basedir )
  {
    m_basedir = basedir;
  }
}