package org.kalypso.ui.preferences;

/**
 * Constants.
 * 
 * @author schlienger
 */
public interface IKalypsoPreferences
{
  /** Number of days used in the date-range preview for the observation repository */
  public final static String NUMBER_OF_DAYS = "kalypso.repository.number_of_days";

  /** name of the property where the client conf files can be found */
  public static final String CLIENT_CONF_URLS = "kalypso.client.conf";
  
  public static final String HTTP_PROXY_USE = "kalypso.http.proxy.use";
  public static final String HTTP_PROXY_HOST = "kalypso.http.proxy.host";
  public static final String HTTP_PROXY_PORT = "kalypso.http.proxy.port";
  public static final String HTTP_PROXY_USER = "kalypso.http.proxy.user";
  public static final String HTTP_PROXY_PASS = "kalypso.http.proxy.pass";
}
