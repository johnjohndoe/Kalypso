package org.kalypso.convert;

public class WeisseElsterConstants
{
    //      kalypso-ocs:WeisseElster://Niederschlag/Ombrometer_3368
    public final static String PREFIX_LINK_OMBROMETER_Niederschlag
    = "WeisseElster://Niederschlag/Ombrometer_";
//    public final static String PREFIX_LINK_OMBROMETER_Temperatur = "kalypso-ocs:WeisseElster://Temperatur/Ombrometer_";
    
//      kalypso-ocs:GebietsNiederschlaege://Catchment1002
    public final static String PREFIX_LINK_GebietsNiederschlagModell = 
      "kalypso-ocs:GebietsNiederschlaege://";// ..+ catchmentFE.ID

//      kalypso-ocs:Vorhersage://Niederschlaege/Catchment1022.zml
    public final static String PREFIX_LINK_NIEDERSCHLAGVORHERSAGE = "kalypso-ocs:Vorhersage://Niederschlaege/";// ...".zml"
    
    public final static String PREFIX_LINK_N_LOKAL="Niederschlag/Niederschlag_";//...+.zml
    //      kalypso-ocs:psicompact://HN.5_WE.02PG...577510
    public final static String PREFIX_LINK_WQ_Zufluss_Rep = "kalypso-ocs:psicompact://HN.5_WE.02PG...";//...+PSIID
//    kalypso-ocs:WeisseElster://Zufluss/Zufluss_Node1500
    public final static String ALTERNATIV_PREFIX_LINK_WQ_Zufluss_Rep = "kalypso-ocs:WeisseElster://Zufluss/Zufluss_";//...+FID

     // TODO let LFUG define a PSI-timeseries for forecast
    public final static String PREFIX_LINK_WQ_Zufluss_Rep_Vorhersage = PREFIX_LINK_WQ_Zufluss_Rep;

    //      kalypso-ocs:psicompact://HN.5_WE.02PG...577510
    public final static String PREFIX_LINK_WQ_Pegel_Rep =  "kalypso-ocs:psicompact://HN.5_WE.02PG...";//...+ID
//    kalypso-ocs:FlussPegel://Pegel/q_goessnitz.zml
    public final static String ALTERNATIV_PREFIX_LINK_WQ_Pegel_Rep =  "kalypso-ocs:FlussPegel://Pegel/";//...ZML-datei

    
    public final static String PREFIX_LINK_WQ_PEGEL_LOKAL="Pegel/Pegel_";//...+.zml
    public final static String PREFIX_LINK_WQ_ZUFLUSS_LOKAL="Zufluss/Zufluss_";//...+.zml
    public final static String PREFIX_LINK_WQ_BERECHNET_LOKAL="Ergebnisse/Berechnet/Abfluss_";//...+.zml
    
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
//    <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Q" ns1:href="Ergebnisse/Berechnet/Abfluss_Node7203.zml" ns1:type="simple"/>
//  </qberechnetZR>
}