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