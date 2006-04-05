package org.kalypso.dss.planerclient.browser;

import java.net.MalformedURLException;

import org.eclipse.jface.dialogs.MessageDialog;
import org.kalypso.commons.java.net.UrlResolverSingleton;
import org.kalypso.contribs.eclipse.ui.browser.AbstractBrowserView;
import org.kalypso.contribs.eclipse.ui.browser.ILocationChangedHandler;

public class KalypsoWebBrowserView extends AbstractBrowserView implements ILocationChangedHandler
{
  public static final String WEB_BROWSER_VIEW_ID = "org.kalypso.dss.planerclient.view.browser"; //$NON-NLS-1$

  public final static String PERSPECTIV_PROTOCOL = "perspective:";

  public final static String LAUNCH_PROTOCOL = "launch:";

  public static final String SEPERATOR = "&";

  public KalypsoWebBrowserView( )
  {
    // i am my own location changed handler
    setLocationChangedHandler( this );
  }

  /**
   * @see org.kalypso.contribs.eclipse.ui.browser.ILocationChangedHandler#handleLocationChange(java.lang.String)
   */
  public boolean handleLocationChange( final String link )
  {
    if( link.startsWith( PERSPECTIV_PROTOCOL ) )
    {
      final String linkStr = link.replaceFirst( PERSPECTIV_PROTOCOL, "" );
      return handlePerspectiveProtocol( linkStr );
    }
    else if( link.startsWith( LAUNCH_PROTOCOL ) )
    {
      return handleLaunchProtocol( link );
    }

    return false;
  }
  
  private boolean handleLaunchProtocol( final Object link )
  {
    MessageDialog.openConfirm( getSite().getShell(), "Not implemented yet", "Das launch-protokol ist noch nicht implementiert. Link: " + link );

    return false;
  }

  private boolean handlePerspectiveProtocol( String urlAsString )
  {
    final String[] split = urlAsString.split( SEPERATOR );
    // try {
    // KalypsoPerspectiveHelper.nextPerspective(split[0], split[1]);
    // } catch (WorkbenchException e) {
    // e.printStackTrace();
    // return false;
    // } catch (FileNotFoundException e) {
    // e.printStackTrace();
    // return false;
    // }
    // return true;
    String url = split[0];
    final String perspectiveID = split[1];
    if( m_context != null )
    {
      try
      {
        url = UrlResolverSingleton.resolveUrl( m_context, url ).toExternalForm();
      }
      catch( MalformedURLException e )
      {
        e.printStackTrace();
        // use original url
      }
    }
    return KalypsoPerspectiveHelper.nextPerspective( url, perspectiveID, getSite().getPage() );
  }

}
