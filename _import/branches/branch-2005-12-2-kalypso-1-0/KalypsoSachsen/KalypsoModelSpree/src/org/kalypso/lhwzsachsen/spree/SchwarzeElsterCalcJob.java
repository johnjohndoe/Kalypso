/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  g.belger@bjoernsen.de
 *  m.schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.lhwzsachsen.spree;

import java.io.File;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;

/**
 * Clac job for the Schwarze-Elster Modell (WASY Model). Uses common code from the Spree-Modell.
 * 
 * @author thuel2
 */
public class SchwarzeElsterCalcJob extends WasyCalcJob
{
  private final static TSDesc[] TS_DESCRIPTOR = new TSDesc[]
  {
      new TSDesc( "W_TRADO", false, null, true ),
      new TSDesc( "Q_TRADO" ),
      new TSDesc( "QX_TRADO", true, "W_TRADO", false ),
      new TSDesc( "WV_TRADO" ),
      new TSDesc( "QV_TRADO", true, "W_TRADO", false ),
      new TSDesc( "QP_TRADO", true, "W_TRADO", false ),
      new TSDesc( "PG_TRADO" ),
      new TSDesc( "PP_TRADO" ),
      new TSDesc( "PA_TRADO", false, null, true ),

      new TSDesc( "W_SCHOEN", false, null, true ),
      new TSDesc( "Q_SCHOEN" ),
      new TSDesc( "QX_SCHOEN", true, "W_SCHOEN", false ),
      new TSDesc( "WV_SCHOEN" ),
      new TSDesc( "QV_SCHOEN", true, "W_SCHOEN", false ),
      new TSDesc( "QP_SCHOEN", true, "W_SCHOEN", false ),
      new TSDesc( "PG_SCHOEN" ),
      new TSDesc( "PP_SCHOEN" ),
      new TSDesc( "PA_SCHOEN", false, null, true ),

      new TSDesc( "W_ZESCHA", false, null, true ),
      new TSDesc( "Q_ZESCHA" ),
      new TSDesc( "QX_ZESCHA", true, "W_ZESCHA", false ),
      new TSDesc( "WV_ZESCHA" ),
      new TSDesc( "QV_ZESCHA", true, "W_ZESCHA", false ),
      new TSDesc( "QP_ZESCHA", true, "W_ZESCHA", false ),
      new TSDesc( "PG_ZESCHA" ),
      new TSDesc( "PP_ZESCHA" ),
      new TSDesc( "PA_ZESCHA", false, null, true ),

      new TSDesc( "QV_ZUKNAP", false, null, true ),
      new TSDesc( "QP_ZUKNAP" ),

      new TSDesc( "Q_SPKNAP", true, "QV_SPKNAP", false ),
      new TSDesc( "QV_SPKNAP", false, null, true ),
      new TSDesc( "QP_SPKNAP", true, "QV_SPKNAP", false ),
      new TSDesc( "V_SPKNAP", true, "QV_SPKNAP", false ),

      new TSDesc( "W_NEUWIE", false, null, true ),
      new TSDesc( "Q_NEUWIE" ),
      new TSDesc( "QX_NEUWIE", true, "W_NEUWIE", false ),
      new TSDesc( "WV_NEUWIE" ),
      new TSDesc( "QV_NEUWIE", true, "W_NEUWIE", false ),
      new TSDesc( "QP_NEUWIE", true, "W_NEUWIE", false ),

      new TSDesc( "QV_ZUNIEM", false, null, true ),
      new TSDesc( "QP_ZUNIEM" ),

      new TSDesc( "Q_SPNIEM", true, "QV_SPNIEM", false ),
      new TSDesc( "QV_SPNIEM", false, null, true ),
      new TSDesc( "QP_SPNIEM", true, "QV_SPNIEM", false ),
      new TSDesc( "V_SPNIEM", true, "QV_SPNIEM", false ),

      new TSDesc( "QV_RLKETTE", false, null, true ),
      new TSDesc( "QP_RLKETTE" ),

      new TSDesc( "W_BIEHLEN", false, null, true ),
      new TSDesc( "Q_BIEHLEN" ),
      new TSDesc( "QX_BIEHLEN", true, "W_BIEHLEN", false ),
      new TSDesc( "WV_BIEHLEN" ),
      new TSDesc( "QV_BIEHLEN", true, "W_BIEHLEN", false ),
      new TSDesc( "QP_BIEHLEN", true, "W_BIEHLEN", false ),

      new TSDesc( "QV_KLETTW", false, null, true ),
      new TSDesc( "QP_KLETTW" ),

      new TSDesc( "QP_LIPSA" ),

      new TSDesc( "W_LAUCHH", false, null, true ),
      new TSDesc( "Q_LAUCHH" ),
      new TSDesc( "QX_LAUCHH", true, "W_LAUCHH", false ),
      new TSDesc( "WV_LAUCHH" ),
      new TSDesc( "QV_LAUCHH", true, "W_LAUCHH", false ),
      new TSDesc( "QP_LAUCHH", true, "W_LAUCHH", false ) };

  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#getSpezifikation()
   */
  public URL getSpezifikation()
  {
    return getClass().getResource( "schwarzeelstercalcjob_spec.xml" );
  }

  /**
   * @see org.kalypso.lhwzsachsen.spree.WasyCalcJob#TS_DESCRIPTOR()
   */
  protected TSDesc[] TS_DESCRIPTOR()
  {
    return TS_DESCRIPTOR;
  }

  /**
   * @see org.kalypso.lhwzsachsen.spree.WasyCalcJob#getExeFilename()
   */
  protected String getExeFilename()
  {
    return "schwarzeelster.exe";
  }

  /**
   * @see org.kalypso.lhwzsachsen.spree.WasyCalcJob#getOtherFiles()
   */
  protected String[] getOtherFiles()
  {
    return new String[]
    {
        getExeFilename(),
        "HW_WQ.DBF",
        "FLUSSPAR.DBF",
        "FLUTUNG.DBF",
        "NA_PARA.DBF",
        "SP_KNAPP.DBF",
        "SP_NIEMT.DBF",
        "TS_PARA.DBF",
        "HWSEKERNEL.dll" };
  }

  /**
   * @see org.kalypso.lhwzsachsen.spree.WasyCalcJob#getResourceBase()
   */
  protected String getResourceBase()
  {
    return "resources/exe/schwarzeelster/";
  }

  /**
   * @see org.kalypso.lhwzsachsen.spree.WasyCalcJob#getWQMap()
   */
  protected WQInfo[] getWQMap()
  {
    return new WQInfo[]
    {
        new WQInfo( "Trado 1", "W_TRADO", 4 ),
        new WQInfo( "Schoenau", "W_SCHOEN", 4 ),
        new WQInfo( "Zescha", "W_ZESCHA", 4 ),
        new WQInfo( "Neuwiese", "W_NEUWIE", 4 ),
        new WQInfo( "Biehlen 1", "W_BIEHLEN", 3 ),
        new WQInfo( "Lauchhammer", "W_LAUCHH", 2 ) };
  }

  /**
   * @see org.kalypso.lhwzsachsen.spree.WasyCalcJob#getWQParamCount()
   */
  protected Integer getWQParamCount()
  {
    return new Integer( 3 );
  }

  /**
   * @see org.kalypso.lhwzsachsen.spree.WasyCalcJob#getStartVolumeMap()
   */
  protected Map getStartVolumeMap()
  {
    final Map map = new HashMap();

    map.put( "V_SPKNAP", "SP_KNAPPENRODE" );

    return map;
  }

  /**
   * @see org.kalypso.lhwzsachsen.spree.WasyCalcJob#makeNapFilename(java.io.File, java.lang.String)
   */
  public String makeNapFilename( final File nativedir, final String tsFilename )
  {
    return new File( nativedir, "NA_KORR" ).getAbsolutePath();
  }

  /**
   * @see org.kalypso.lhwzsachsen.spree.WasyCalcJob#makeFlpFilename(java.io.File, java.lang.String)
   */
  public String makeFlpFilename( final File nativedir, final String tsFilename )
  {
    return new File( nativedir, "KORRPAR" ).getAbsolutePath();
  }

}
