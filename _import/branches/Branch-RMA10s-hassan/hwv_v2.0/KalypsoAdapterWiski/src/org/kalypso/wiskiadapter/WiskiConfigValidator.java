/*--------------- Kalypso-Header ------------------------------------------

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

 --------------------------------------------------------------------------*/

package org.kalypso.wiskiadapter;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.logging.Level;

import org.eclipse.jface.dialogs.IInputValidator;

/**
 * Die Wiski Configuration String ist wie folgt aufgebaut: <br>
 * <code>url#domain#login#password#language</code> <br>
 * oder optional mit DEBUG Information: <br>
 * <code>url#domain#login#password#language{DEBUG#[SIMULATE]#filepath}</code> <br>
 * Im Debugmodus werden die Ergebnisse der WDP-Aufrufe in Dateien (in filepath zu finden) serialisiert, vorausgesetzt
 * SIMULATE ist nicht angegeben. Ist SIMULATE angegeben, so werden die WDP-Aufrufe simuliert und nicht dem echten WDP
 * weitergeleitet, sondern die in die Dateien (unter filepath liegend) gespeicherte Ergebnisse werden benutzt.
 * 
 * @author schlienger
 */
public class WiskiConfigValidator implements IInputValidator
{
  /* System-Property: comma-separated list of class-names that are forbidden (ie. will be ignored). */
  private final String SYSPROP_FORBIDDEN_CALLS = "org.kalypso.wiskiadapter.forbiddencalls";
  
  /** expected number of items in the configuration string */
  private final static int CONF_NB_ITEMS = 5;

  /** separator of the configuration string */
  private final static String CONF_SEP = "#";

  private String m_url;

  private String m_domain;

  private String m_logonName;

  private String m_password;

  private String m_language;

  private boolean m_debugMode;

  private String m_debugDir;

  private boolean m_simulateMode;

  private Level m_logLevel;

  private Collection<String> m_forbiddenCalls;

  /**
   * @see org.eclipse.jface.dialogs.IInputValidator#isValid(java.lang.String)
   */
  public String isValid( final String confString )
  {
    String conf = confString;

    if( conf == null )
      return "Die Wiski-Verbindungsinformation ist null";

    // first check if DEBUG mode is set
    final int ixDbg = conf.indexOf( "{DEBUG#" );
    if( ixDbg != -1 )
    {
      m_debugMode = true;

      final int ixDbgEnd = conf.indexOf( '}', ixDbg );
      if( ixDbgEnd == -1 )
        return "Eingabe überprüfen, Syntax: URL#Domain#Benutzername#Passwort#Sprache{DEBUG#[SIMULATE]#Pfad}";
      
      final String confDebug = conf.substring( ixDbg + 1, ixDbgEnd );
      final String[] confDebugItems = confDebug.split( "#" );
      // confDebugItems[0] beinhaltet immer DEBUG
      m_simulateMode = confDebugItems[1].equalsIgnoreCase( "SIMULATE" );
      m_debugDir = m_simulateMode ? confDebugItems[2] : confDebugItems[1];
      
      if( confDebugItems.length < 2 || confDebugItems.length > 3 )
        return "Eingabe überprüfen, Syntax: URL#Domain#Benutzername#Passwort#Sprache{DEBUG#[SIMULATE]#Pfad}";
      
      conf = conf.replaceAll( "\\{DEBUG.*\\}", "" );
    }
    else
      m_debugMode = false;

    final String[] items = conf.split( CONF_SEP );

    if( items.length != CONF_NB_ITEMS )
      return "Eingabe überprüfen, Syntax: URL#Domain#Benutzername#Passwort#Sprache{DEBUG#[SIMULATE]#Pfad}";

    parse( items );

    return null;
  }

  private void parse( final String[] items )
  {
    m_url = items[0];
    m_domain = items[1];
    m_logonName = items[2];
    m_password = items[3];
    m_language = items[4];
    
    final String forbiddenCallsProp = System.getProperty( SYSPROP_FORBIDDEN_CALLS, "" );
    final String[] forbiddenCalls = forbiddenCallsProp.split( "," );
    m_forbiddenCalls = new HashSet<String>( Arrays.asList( forbiddenCalls ) );
  }

  public String getDomain()
  {
    return m_domain;
  }

  public String getLanguage()
  {
    return m_language;
  }

  public String getLogonName()
  {
    return m_logonName;
  }

  public String getPassword()
  {
    return m_password;
  }

  public String getUrl()
  {
    return m_url;
  }

  public boolean isDebugMode()
  {
    return m_debugMode;
  }

  public String getDebugDir()
  {
    return m_debugDir;
  }

  public boolean isSimulateMode()
  {
    return m_simulateMode;
  }

  public Level getLogLevel()
  {
    return m_logLevel;
  }

  /**
   * A collection of forbidden wiski-calls (its class names, no package).
   */
  public Collection<String> getForbiddenCalls( )
  {
    return m_forbiddenCalls;
  }
}
