package org.deegree.tools;

import java.io.IOException;
import java.net.URL;
import java.net.URLConnection;

/**
 * @author sbad0205
 */
public interface IURLConnectionFactory
{
  public URLConnection createURLConnection( URL url ) throws IOException;

}