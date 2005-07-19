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

package org.kalypso.auth.scenario;

/**
 * Provides utility methods for scenario-handling.
 * 
 * @author schlienger
 */
public class ScenarioUtilities
{
  private ScenarioUtilities()
  {
  // utility class, do not instanciate
  }

  /**
   * The kalypso convention says that a (or the) default scenario has an id which value is the empty string.
   * 
   * @return true if the given scenario is a default scenario
   */
  public static boolean isDefaultScenario( final IScenario sce )
  {
    if( sce == null )
      throw new IllegalArgumentException( "scenario is null" );

    return sce.getId().equals( "" );
  }
  
  /**
   * Convenient method for replacing the tokens within a string with
   * the values of the properties delivered by the given scenario.
   * 
   * @param propertyValue some string which might contain tokens
   * 
   * @return a new string with token replaced by values as found in the scenario, if any
   */
  public static String replaceTokens( final String propertyValue, final IScenario scenario )
  {
    String newPropertyValue = propertyValue;
    
    final String str = scenario.getProperty( IScenario.PROP_TOKEN_LIST, null );
    if( str != null )
    {
      final String[] tokens = str.split( ";" );
      for( int i = 0; i < tokens.length; i++ )
      {
        final String token = IScenario.TOKEN_BEGIN + tokens[i] + IScenario.TOKEN_END;
        final String value = scenario.getProperty( tokens[i], "" );
        
        newPropertyValue = newPropertyValue.replaceAll( token, value );
      }
    }
    
    return newPropertyValue;
  }
}
