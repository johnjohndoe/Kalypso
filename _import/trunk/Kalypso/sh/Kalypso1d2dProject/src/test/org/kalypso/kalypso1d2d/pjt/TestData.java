/**
 * 
 */
package test.org.kalypso.kalypso1d2d.pjt;

import java.net.URL;

/**
 * @author congo
 *
 */
public class TestData
{
	
	public static final URL URL_AGGER_KARTE;
	public static final String NAME_AGGER_KARTE="agger_karte.gmt";
	
	public static final URL URL_AGGER_MODEL;
	public static final String NAME_AGGER_MODEL="agger_modell.gml";
	
	static 
	{
		URL_AGGER_KARTE=TestData.class.getResource( NAME_AGGER_KARTE );
		URL_AGGER_MODEL=TestData.class.getResource( NAME_AGGER_MODEL );
	}
}
