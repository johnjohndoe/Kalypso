package org.kalypso.util;

/**
 * Constants used throughout Kalypso Util library. Constants listed here *can*
 * be modified at runtime by clients. Nevertheless, we suppose that this won't be
 * the case, except for some rare situations where the values are inapropriate.
 * 
 * @author schlienger
 */
public interface KalypsoUtilConstants
{
  /**
   * A URL may have appended to it a "fragment", also known as a "ref" or a
   * "reference". The fragment is indicated by the sharp sign character "#"
   * followed by more characters. For example,
   * 
   * <pre>http://java.sun.com/index.html#chapter1</pre>
   * 
   * <p>
   * This fragment is not technically part of the URL. Rather, it indicates that
   * after the specified resource is retrieved, the application is specifically
   * interested in that part of the document that has the tag chapter1 attached
   * to it. The meaning of a tag is resource specific.
   * 
   * @see java.net.URL
   */
  public static char URL_FRAGMENT_START = '#';

  /**
   * This is the separator used within the fragment of an URL to specify more
   * parameters. For example,
   * 
   * <pre>http://java.sun.com/index.html#chapter1?para=3</pre>
   * 
   * @see java.net.URL
   */
  public static char URL_FRAGMENT_SEPARATOR = '?';
}