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

package org.kalypso.core;

import java.util.logging.Logger;

/**
 * Helper class for better handling the start of Kalypso. It parses program arguments and allows others to retrieve the
 * values.
 * <p>
 * This class resides in the KalypsoCore plugin because it should not be in the same plugin as the one that uses it.
 * This is due to the plugin lifecycle. We would have no chance to get access on it before the plugin starts. The
 * information contained in the arguments might be used within the start process, so we need to pack the argument
 * parsing stuff in a plugin that does not depend on it.
 * 
 * @author schlienger
 */
public class KalypsoStart
{
  /**
   * The list of client-configuration locations, can be given as program argument.
   * <p>
   * If defined, it overwrites the one found in the user preferences.
   * 
   * @see #parseArguments(String[])
   */
  public static String SERVER_LOCATIONS = null;

  private static final Logger LOGGER = Logger.getLogger( KalypsoStart.class.getName() );

  private KalypsoStart()
  {
  // not intended to be instanciated
  }

  /**
   * Parse the programm arguments and looks for known arguments that should be handled in this plugin. This method
   * should be called on program start from within the KalypsoApplication.
   * <p>
   * Following arguments are supported:
   * <dl>
   * <dt>kalypsoserver</dt>
   * <dd>if specified, denotes the list (comma separated) of locations of kalypso client configurations. The client
   * configuration contains information on how to locate the kalypso services. Example (replace SERVER_NAME with a valid
   * name): <br>
   * <code>-kalypsoserver=http://SERVER_NAME:8088/webdav/kalypso-client.ini</code></dd>
   * </dl>
   * 
   * @param args
   *          array of string
   */
  public static void parseArguments( final String[] args )
  {
    SERVER_LOCATIONS = null;

    if( args == null )
      return;

    final String ARG_NAME = "-kalypsoserver=";
    for( int i = 0; i < args.length; i++ )
    {
      if( args[i].startsWith( ARG_NAME ) )
      {
        SERVER_LOCATIONS = args[i].replaceAll( ARG_NAME, "" );

        LOGGER.info( "Starting KALYPSO with a predefined list of locations: " + SERVER_LOCATIONS );
        LOGGER.info( "This list will overwrite the one found in the user preferences" );
      }
    }
  }
}
