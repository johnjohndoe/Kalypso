package org.kalypso.lhwzsachsen.spree;

import java.io.File;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;

/**
 * <p>
 * Der Rechenservice für das SpreeModell
 * </p>
 * 
 * @author Belger
 */
public class SpreeCalcJob extends WasyCalcJob
{
  private final static TSDesc[] TS_DESCRIPTOR = new TSDesc[] { new TSDesc( "S_SCHIRG" ), new TSDesc( "W_SCHIRG", false, null, true ), new TSDesc( "Q_SCHIRG" ),
      new TSDesc( "QX_SCHIRG", true, "W_SCHIRG", false ), new TSDesc( "WV_SCHIRG" ), new TSDesc( "QV_SCHIRG", true, "W_SCHIRG", false ), new TSDesc( "QP_SCHIRG", true, "W_SCHIRG", false ),
      new TSDesc( "PG_SCHIRG" ), new TSDesc( "PP_SCHIRG" ), new TSDesc( "PA_SCHIRG", false, null, true ), new TSDesc( "S_BAUTZWB" ), new TSDesc( "W_BAUTZWB", false, null, true ),
      new TSDesc( "Q_BAUTZWB" ), new TSDesc( "QX_BAUTZWB", true, "W_BAUTZWB", false ), new TSDesc( "WV_BAUTZWB" ), new TSDesc( "QV_BAUTZWB", true, "W_BAUTZWB", false ),
      new TSDesc( "QP_BAUTZWB", true, "W_BAUTZWB", false ), new TSDesc( "PG_BAUTZWB" ), new TSDesc( "PP_BAUTZWB" ), new TSDesc( "PA_BAUTZWB", false, null, true ), new TSDesc( "S_TSBAUTZ" ),
      new TSDesc( "Q_TSBAUTZ", true, "QV_TSBAUTZ", false ), new TSDesc( "QV_TSBAUTZ", false, null, true ), new TSDesc( "QP_TSBAUTZ", true, "QV_TSBAUTZ", false ),
      new TSDesc( "V_TSBAUTZ", true, "QV_TSBAUTZ", false ), new TSDesc( "S_GROEDI" ), new TSDesc( "W_GROEDI", false, null, true ), new TSDesc( "Q_GROEDI" ),
      new TSDesc( "QX_GROEDI", true, "W_GROEDI", false ), new TSDesc( "WV_GROEDI" ), new TSDesc( "QV_GROEDI", true, "W_GROEDI", false ), new TSDesc( "QP_GROEDI", true, "W_GROEDI", false ),
      new TSDesc( "ZG_GROEDI" ), new TSDesc( "PG_GROEDI" ), new TSDesc( "PP_GROEDI" ), new TSDesc( "PA_GROEDI", false, null, true ), new TSDesc( "S_SPWIESE" ),
      new TSDesc( "QV_SPWIESE", false, null, true ), new TSDesc( "QP_SPWIESE" ), new TSDesc( "S_LIESKE" ), new TSDesc( "W_LIESKE", false, null, true ), new TSDesc( "Q_LIESKE" ),
      new TSDesc( "QX_LIESKE", true, "W_LIESKE", false ), new TSDesc( "WV_LIESKE" ), new TSDesc( "QV_LIESKE", true, "W_LIESKE", false ), new TSDesc( "QP_LIESKE", true, "W_LIESKE", false ),
      new TSDesc( "S_JAENKD" ), new TSDesc( "W_JAENKD", false, null, true ), new TSDesc( "Q_JAENKD" ), new TSDesc( "QX_JAENKD", true, "W_JAENKD", false ), new TSDesc( "WV_JAENKD" ),
      new TSDesc( "QV_JAENKD", true, "W_JAENKD", false ), new TSDesc( "QP_JAENKD", true, "W_JAENKD", false ), new TSDesc( "PG_JAENKD" ), new TSDesc( "PP_JAENKD" ),
      new TSDesc( "PA_JAENKD", false, null, true ), new TSDesc( "S_TSQUITZ" ), new TSDesc( "Q_TSQUITZ", true, "QV_TSQUITZ", false ), new TSDesc( "QV_TSQUITZ", false, null, true ),
      new TSDesc( "QP_TSQUITZ", true, "QV_TSQUITZ", false ), new TSDesc( "V_TSQUITZ", true, "QV_TSQUITZ", false ), new TSDesc( "S_HOLTD" ), new TSDesc( "W_HOLTD", false, null, true ),
      new TSDesc( "Q_HOLTD" ), new TSDesc( "QX_HOLTD", true, "W_HOLTD", false ), new TSDesc( "WV_HOLTD" ), new TSDesc( "QV_HOLTD", true, "W_HOLTD", false ),
      new TSDesc( "QP_HOLTD", true, "W_HOLTD", false ), new TSDesc( "PG_HOLTD" ), new TSDesc( "PP_HOLTD" ), new TSDesc( "PA_HOLTD", false, null, true ), new TSDesc( "S_SAERI" ),
      new TSDesc( "W_SAERI", false, null, true ), new TSDesc( "Q_SAERI" ), new TSDesc( "QX_SAERI", true, "W_SAERI", false ), new TSDesc( "WV_SAERI" ),
      new TSDesc( "QV_SAERI", true, "W_SAERI", false ), new TSDesc( "QP_SAERI", true, "W_SAERI", false ), new TSDesc( "ZG_SAERI" ), new TSDesc( "PG_SAERI" ), new TSDesc( "PP_SAERI" ),
      new TSDesc( "PA_SAERI", false, null, true ), new TSDesc( "S_BOXBRG" ), new TSDesc( "W_BOXBRG", false, null, true ), new TSDesc( "Q_BOXBRG" ), new TSDesc( "QX_BOXBRG", true, "W_BOXBRG", false ),
      new TSDesc( "WV_BOXBRG" ), new TSDesc( "QV_BOXBRG", true, "W_BOXBRG", false ), new TSDesc( "QP_BOXBRG", true, "W_BOXBRG", false ), new TSDesc( "S_BWALDE" ),
      new TSDesc( "QV_BWALDE", false, null, true ), new TSDesc( "QP_BWALDE" ), new TSDesc( "S_LOHSA" ), new TSDesc( "QV_LOHSA", false, null, true ), new TSDesc( "QP_LOHSA" ), new TSDesc( "S_SPREY" ),
      new TSDesc( "W_SPREY", false, null, true ), new TSDesc( "Q_SPREY" ), new TSDesc( "QX_SPREY", true, "W_SPREY", false ), new TSDesc( "WV_SPREY" ),
      new TSDesc( "QV_SPREY", true, "W_SPREY", false ), new TSDesc( "QP_SPREY", true, "W_SPREY", false ), new TSDesc( "S_BURGNEU" ), new TSDesc( "QP_BURGNEU", false, null, true ),
      new TSDesc( "S_SPWITZ" ), new TSDesc( "W_SPWITZ", false, null, true ), new TSDesc( "Q_SPWITZ" ), new TSDesc( "QX_SPWITZ", true, "W_SPWITZ", false ), new TSDesc( "WV_SPWITZ" ),
      new TSDesc( "QV_SPWITZ", true, "W_SPWITZ", false ), new TSDesc( "QP_SPWITZ", true, "W_SPWITZ", false ), new TSDesc( "S_RLKETTE" ), new TSDesc( "QV_RLKETTE", false, null, true ),
      new TSDesc( "QP_RLKETTE" ), new TSDesc( "S_SPREMB" ), new TSDesc( "W_SPREMB", false, null, true ), new TSDesc( "Q_SPREMB" ), new TSDesc( "QX_SPREMB", true, "W_SPREMB", false ),
      new TSDesc( "WV_SPREMB" ), new TSDesc( "QV_SPREMB", true, "W_SPREMB", false ), new TSDesc( "QP_SPREMB", true, "W_SPREMB", false ) };

  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#getSpezifikation()
   */
  public URL getSpezifikation( )
  {
    return getClass().getResource( "spreecalcjob_spec.xml" );
  }

  /**
   * @see org.kalypso.lhwzsachsen.spree.WasyCalcJob#TS_DESCRIPTOR()
   */
  @Override
  protected TSDesc[] TS_DESCRIPTOR( )
  {
    return TS_DESCRIPTOR;
  }

  /**
   * @see org.kalypso.lhwzsachsen.spree.WasyCalcJob#getExeFilename()
   */
  @Override
  protected String getExeFilename( )
  {
    return "spree.exe";
  }

  /**
   * @see org.kalypso.lhwzsachsen.spree.WasyCalcJob#getOtherFiles()
   */
  @Override
  protected String[] getOtherFiles( )
  {
    return new String[] { "spree.exe", "FLUSSPAR.DBF", "FLUTUNG.DBF", "NA_PARA.DBF", "TS_BAUTZ.DBF", "TS_PARA.DBF", "TS_QUITZ.DBF", "ZWIPAR.DBF", "xHWKERNEL.DLL" };
  }

  /**
   * @see org.kalypso.lhwzsachsen.spree.WasyCalcJob#getResourceBase()
   */
  @Override
  protected String getResourceBase( )
  {
    return "resources/exe/spree/";
  }

  /**
   * @see org.kalypso.lhwzsachsen.spree.WasyCalcJob#getWQMap()
   */
  @Override
  protected WQInfo[] getWQMap( )
  {
    return new WQInfo[] { new WQInfo( "Schirgisw", "W_SCHIRG", 1 ), new WQInfo( "Bautzen WB", "W_BAUTZWB", 3 ), new WQInfo( "Gröditz 1", "W_GROEDI", 3 ), new WQInfo( "Lieske", "W_LIESKE", 2 ),
        new WQInfo( "Jänkendorf", "W_JAENKD", 3 ), new WQInfo( "Holtendorf", "W_HOLTD", 1 ), new WQInfo( "Särichen", "W_SAERI", 4 ), new WQInfo( "Boxberg", "W_BOXBRG", 4 ),
        new WQInfo( "Sprey", "W_SPREY", 1 ), new WQInfo( "Spreewitz", "W_SPWITZ", 1 ), new WQInfo( "Spremberg", "W_SPREMB", 0 ) };
  }

  /**
   * @see org.kalypso.lhwzsachsen.spree.WasyCalcJob#getWQParamCount()
   */
  @Override
  protected Integer getWQParamCount( )
  {
    return new Integer( 4 );
  }

  /**
   * @see org.kalypso.lhwzsachsen.spree.WasyCalcJob#getStartVolumeMap()
   */
  @Override
  protected Map<String, String> getStartVolumeMap( )
  {
    final Map<String, String> map = new HashMap<String, String>();

    map.put( "V_TSQUITZ", "TS_QUITZDORF" );
    map.put( "V_TSBAUTZ", "TS_BAUTZEN" );

    return map;
  }

  /**
   * @see org.kalypso.lhwzsachsen.spree.WasyCalcJob#makeNapFilename(java.io.File, java.lang.String)
   */
  @Override
  public String makeNapFilename( final File nativedir, final String tsFilename )
  {
    return tsFilename + "_nap";
  }

  /**
   * @see org.kalypso.lhwzsachsen.spree.WasyCalcJob#makeFlpFilename(java.io.File, java.lang.String)
   */
  @Override
  public String makeFlpFilename( final File nativedir, final String tsFilename )
  {
    return tsFilename + "_flp";
  }

  /**
   * @see org.kalypso.lhwzsachsen.spree.WasyCalcJob#isSpreeFormat()
   */
  @Override
  public boolean isSpreeFormat( )
  {
    return true;
  }
}