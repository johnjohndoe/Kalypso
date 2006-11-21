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
package org.kalypso.convert.namodel;

/**
 * @author doemming
 */
public interface NaModelConstants
{
  //Bean IDs

  public static final String IN_META_ID = "MetaSteuerdaten";

  public final static String IN_MODELL_ID = "Modell";

  public final static String IN_HYDROTOP_ID = "Hydrotop";

  public final static String IN_PARAMETER_ID = "Parameter";

  public final static String IN_CONTROL_ID = "Control";

  public final static String IN_TEMPLATE_ID = "Template";

  public static final String IN_OPTIMIZECONF_ID = "SceConf";

  public final static String LOG_EXE_STDOUT_ID = "LOG_EXE_STDOUT";

  public static final String NS_NAMETA = "org.kalypso.na.control";
  
  public static final String NS_NAMODELL = "http://www.tuhh.de/kalypsoNA";

  public static final String NS_NACONTROL = "org.kalypso.namodell.control";

  public static final String NS_NAHYDROTOP = "http://www.tuhh.de/hydrotop";

  public static final String NS_NAPARAMETER = "http://www.tuhh.de/parameter";

  public static final String NS_OMBROMETER = "http://org.kalypso.ombrometer";

  public static final String LOG_EXE_ERROUT_ID = "LOG_EXE_ERROUT";

  public static final String LOG_OUTRES_ID = "LOG_OUTRES";

  public static final String LOG_OUTERR_ID = "LOG_OUTERR";

  public static final String OUT_ZML = "OUT_ZML";
  
  public static final String OUT_OPTIMIZEFILE= "OUT_OPTIMIZEFILE";

  public static final String OUTPUT_DIR_NAME = "results";

  public static final String LOG_INFO_ID = "LOG_INFO";

}