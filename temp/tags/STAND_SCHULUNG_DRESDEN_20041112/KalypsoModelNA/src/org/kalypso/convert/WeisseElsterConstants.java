package org.kalypso.convert;

public class WeisseElsterConstants
{
    // example: "kalypso-ocs:WeisseElster://Niederschlag/Niederschlag_Catchment1000.zml"
    // "kalypso-ocs:WeisseElster://Ombrometer/Ombrometer_7662.zml";
    public final static String PREFIX_LINK_OMBROMETER_Niederschlag
    = "WeisseElster://Niederschlag/Ombrometer_";
    public final static String PREFIX_LINK_OMBROMETER_Temperatur = "kalypso-ocs:WeisseElster://Temperatur/Ombrometer_";
    public final static String PREFIX_LINK_GebietsNiederschlagModell = 
      "kalypso-ocs:GebietsNiederschlaege://";// ..+ catchmentFE.ID
    public final static String PREFIX_LINK_NIEDERSCHLAGVORHERSAGE = "kalypso-ocs:GebietsVorhersage://";
    public final static String PREFIX_LINK_FLUSSPEGEL = "kalypso-ocs:FlussPegel://Pegel/q_";
    public final static String PREFIX_LINK_ZUFLUSSPEGEL =  "kalypso-ocs:WeisseElster://Zufluss/Zufluss_"; //..nodeFE.ID

}