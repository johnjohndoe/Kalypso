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
package org.kalypso.convert.update;

public class WeisseElsterConstants
{

  //    public final static String PREFIX_LINK_OMBROMETER_Temperatur =
  // "kalypso-ocs:WeisseElster://Temperatur/Ombrometer_";

  //      kalypso-ocs:GebietsNiederschlaege://Catchment1002
  public final static String PREFIX_LINK_GebietsNiederschlagModell = "kalypso-ocs:GebietsNiederschlaege://";// ..+

  // catchmentFE.ID

  //      kalypso-ocs:Vorhersage://Niederschlaege/Catchment1022.zml
  public final static String PREFIX_LINK_NIEDERSCHLAGVORHERSAGE = "kalypso-ocs:Vorhersage://Niederschlaege/";// ...".zml"

  public final static String PREFIX_LINK_N_LOKAL = "Niederschlag/Niederschlag_";//...+.zml

  //      kalypso-ocs:psicompact://HN.5_WE.02PG...577510
  public final static String PREFIX_LINK_WQ_Zufluss_Rep = "kalypso-ocs:psicompact://HN.5_WE.02PG...";//...+PSIID

  //    kalypso-ocs:WeisseElster://Zufluss/Zufluss_Node1500

  public final static String ALTERNATIV_PREFIX_LINK_WQ_Zufluss_Rep = "kalypso-ocs:WeisseElster://Zufluss/Zufluss_";//...+FID

  // TODO let LFUG define a PSI-timeseries for forecast
  public final static String PREFIX_LINK_WQ_Zufluss_Rep_Vorhersage = PREFIX_LINK_WQ_Zufluss_Rep;

  //      kalypso-ocs:psicompact://HN.5_WE.02PG...577510
  public final static String PREFIX_LINK_WQ_Pegel_Rep = "kalypso-ocs:psicompact://HN.5_WE.02PG...";//...+ID

  //    kalypso-ocs:FlussPegel://Pegel/q_goessnitz.zml

  public final static String ALTERNATIV_PREFIX_LINK_WQ_Pegel_Rep = "kalypso-ocs:FlussPegel://Pegel/";//...ZML-datei

  public final static String PREFIX_LINK_WQ_PEGEL_LOKAL = "Pegel/Pegel_";//...+.zml //$NON-NLS-1$

  public final static String PREFIX_LINK_WQ_ZUFLUSS_LOKAL = "Zufluss/Zufluss_";//...+.zml //$NON-NLS-1$

  public final static String PREFIX_LINK_WQ_BERECHNET_LOKAL = "Ergebnisse/Berechnet/Abfluss_";//...+.zml //$NON-NLS-1$

  //    Pegel:
  //      kalypso-ocs:psicompact://HN.5_WE.02PG...577510
  //
  //      Gebietsniederschlaege:
  //      kalypso-ocs:GebietsNiederschlaege://Catchment1002
  //
  //      Ombrometer:
  //      kalypso-ocs:WeisseElster://Niederschlag/Ombrometer_3368
  //
  //      N-Vorhersage
  //      kalypso-ocs:Vorhersage://Niederschlaege/Catchment1022.zml

  //    <qberechnetZR>
  //    <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink"
  // xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum"
  // valueaxis="Q" ns1:href="Ergebnisse/Berechnet/Abfluss_Node7203.zml"
  // ns1:type="simple"/>
  //  </qberechnetZR>
}