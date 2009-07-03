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
package org.kalypso.convert.dwd;

/**
 * maps the DWD-ombrometer-ID to PSI-ID for the rainfall runoff model implementation
 * "weisse elster".
 * 
 * @author doemming
 */
public class DWD_PSI_Mapper
{
  //  "kalypso-ocs:psicompact://HN.5_WE.01OM...104710"

  /**
   * prefix href to psi database <br>
   * as this is linking inside the repository the prefix of "kalypso-ocs:" must
   * not occur. <br>
   * e.g. <br>
   * 
   * the link from client :"kalypso-ocs:psicompact://HN.5_WE.01OM...104710" <br>
   * will result as this link: "psicompact://HN.5_WE.01OM...104710"
   */
  private final static String PSIPrefix = "psicompact://";

  /**
   * maps the DWD-ID to PSI-ID for the rainfall runoff model implementation
   * "weisse elster"
   */
  public static String mapDWDtoPSI( String dwdID )
  {
    // Schleiz
    if( "4234".equals( dwdID ) )
      return PSIPrefix + "HN.5_WE.01OM...430570";
    // Plauen
    if( "4426".equals( dwdID ) )
      return PSIPrefix + "HN.5_WE.01OM...433060";
    //  Carlsfeld
    if( "4435".equals( dwdID ) )
      return PSIPrefix + "HN.4_MU.01OM...420130";
    // Leipzig-Schkeuditz
    if( "3368".equals( dwdID ) )
      return PSIPrefix + "HN.5_WE.01OM...425440";
    // Leipzig
    if( "3375".equals( dwdID ) )
      return PSIPrefix + "HN.5_WE.01OM...104710";
    // Gera
    if( "4406".equals( dwdID ) )
      return PSIPrefix + "HN.5_WE.01OM...433160";
    // Chemnitz
    if( "4412".equals( dwdID ) )
      return PSIPrefix + "HN.4_MU.01OM...421550";
    // Aue
    if( "4422".equals( dwdID ) )
      return PSIPrefix + "HN.4_MU.01OM...420300";
    throw new UnsupportedOperationException( "unknown DWD-ID, can not map to PSI timeseries" );
  }
}